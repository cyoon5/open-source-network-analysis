# =============================================================================
# github_helpers.R - safe GitHub REST API calls.
#
# Why: req_perform() stops the whole script on any failed request, which is why
# the old review collection died part-way. These helpers instead
#   * wait automatically when the 5,000 calls/hour limit is reached,
#   * retry network hiccups and GitHub server errors,
#   * return NULL for a request that really fails, so the loop keeps going,
#   * save progress so a stopped script resumes where it left off.
# =============================================================================

library(httr2)
library(dplyr)

GITHUB_PAT <- Sys.getenv("GITHUB_PAT")
if (GITHUB_PAT == "") stop("GITHUB_PAT is missing. Add it to .Renviron and restart R.")

# Value or NA when GitHub left the field out
or_na <- function(x, na = NA_character_) if (is.null(x)) na else x

# Current time according to GitHub's server (falls back to this computer's clock)
github_now <- function(resp) {
  date_header <- resp_header(resp, "date")   # e.g. "Thu, 05 Nov 2026 21:34:10 GMT"
  t <- NA
  if (!is.null(date_header)) {
    old_locale <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", "C")            # English day/month names
    t <- as.POSIXct(date_header, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
    Sys.setlocale("LC_TIME", old_locale)
  }
  if (is.na(t)) as.numeric(Sys.time()) else as.numeric(t)
}

# One GET request. Returns the response, or NULL if it failed for good.
gh_get <- function(path, query = list(), max_failures = 5) {
  url <- if (startsWith(path, "https://")) path else paste0("https://api.github.com/", path)
  failures <- 0

  while (failures < max_failures) {
    resp <- tryCatch(
      request(url) |>
        req_url_query(!!!query) |>
        req_auth_bearer_token(GITHUB_PAT) |>
        req_headers(Accept = "application/vnd.github+json",
                    `X-GitHub-Api-Version` = "2022-11-28") |>
        req_error(is_error = function(r) FALSE) |>   # never throw, we check status ourselves
        req_perform(),
      error = function(e) NULL                       # network error (no response at all)
    )

    if (is.null(resp)) {
      failures <- failures + 1
      Sys.sleep(10 * failures)
      next
    }

    status <- resp_status(resp)
    if (status == 200) return(resp)

    # Rate limited: wait until the limit resets (this does not count as a failure)
    if (status %in% c(403, 429)) {
      remaining <- resp_header(resp, "x-ratelimit-remaining")
      if (!is.null(remaining) && remaining == "0") {
        reset <- as.numeric(resp_header(resp, "x-ratelimit-reset"))
        # Use GitHub's clock (the Date header), not this computer's, so a
        # wrong system clock can't make us retry too early.
        now <- github_now(resp)
        wait <- max(reset - now, 0) + 5
        wait <- max(wait, 60)   # never retry more than once a minute
        message(sprintf("  Rate limit reached. Waiting %.0f minutes...", wait / 60))
        Sys.sleep(wait)
        next
      }
      retry_after <- resp_header(resp, "retry-after")   # GitHub's "slow down" limit
      Sys.sleep(if (!is.null(retry_after)) as.numeric(retry_after) + 1 else 60)
      failures <- failures + 1
      next
    }

    # GitHub server error: try again
    if (status >= 500) {
      failures <- failures + 1
      Sys.sleep(10 * failures)
      next
    }

    # 404 etc.: retrying will not help
    warning("GET ", url, " failed with status ", status, call. = FALSE)
    return(NULL)
  }

  warning("GET ", url, " gave up after ", max_failures, " failures", call. = FALSE)
  NULL
}

# All pages of a list endpoint (100 items per page).
# stop_when(page_items) can end paging early. Returns NULL if a page fails.
gh_get_all <- function(path, query = list(), stop_when = NULL, label = NULL) {
  items <- list()
  page  <- 1
  repeat {
    resp <- gh_get(path, c(query, list(per_page = 100, page = page)))
    if (is.null(resp)) return(NULL)
    batch <- resp_body_json(resp, simplifyVector = FALSE)
    items <- c(items, batch)
    if (!is.null(label) && page %% 25 == 0) {
      message("  ", label, ": ", page, " pages, ", length(items), " items so far")
    }
    if (length(batch) < 100) break
    if (!is.null(stop_when) && stop_when(batch)) break
    page <- page + 1
  }
  items
}

# Runs fetch_one(repo, pr_number) for every PR in `prs`, with resume support.
#   fetch_one must return a data frame (0 rows is fine) or NULL on failure.
#   Progress is saved every `every` PRs to data/checkpoints/<name>.csv.
#   PRs that failed are retried the next time you run the script.
run_per_pr <- function(prs, fetch_one, name, every = 200) {
  out_file  <- file.path(CHECKPOINT_DIR, paste0(name, ".csv"))
  done_file <- file.path(CHECKPOINT_DIR, paste0(name, "_done.csv"))

  done <- if (file.exists(done_file)) {
    read.csv(done_file, stringsAsFactors = FALSE)
  } else {
    data.frame(repo = character(), pr_number = integer())
  }
  todo <- prs |>
    distinct(repo, pr_number) |>
    anti_join(done, by = c("repo", "pr_number"))
  message(name, ": ", nrow(done), " PRs already done, ", nrow(todo), " to go.")

  rows_buf <- list()
  done_buf <- list()
  failed   <- 0
  flush <- function() {
    if (length(rows_buf)) append_csv(bind_rows(rows_buf), out_file)
    if (length(done_buf)) append_csv(bind_rows(done_buf), done_file)
    rows_buf <<- list()
    done_buf <<- list()
  }

  for (i in seq_len(nrow(todo))) {
    rows <- fetch_one(todo$repo[i], todo$pr_number[i])
    if (is.null(rows)) {
      failed <- failed + 1
    } else {
      if (nrow(rows) > 0) rows_buf[[length(rows_buf) + 1]] <- rows
      done_buf[[length(done_buf) + 1]] <- todo[i, c("repo", "pr_number")]
    }
    if (i %% every == 0) {
      flush()
      message(sprintf("  %s: %d / %d PRs (%s)", name, i, nrow(todo), format(Sys.time(), "%H:%M")))
    }
  }
  flush()

  if (failed > 0) message(failed, " PRs failed. Run this script again to retry just those.")
  if (file.exists(out_file)) read.csv(out_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8") else data.frame()
}
