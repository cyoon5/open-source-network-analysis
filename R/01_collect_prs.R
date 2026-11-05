# =============================================================================
# 01_collect_prs.R - every PR created in the date window, for each repo.
#
# Output: data/raw_prs.csv, one row per PR
#   repo, pr_number, author, author_type ("User"/"Bot"), title, state,
#   draft, created_at, closed_at, merged_at, labels (";"-separated),
#   body_mentions_ai (TRUE/FALSE), ai_keyword (first AI word found, or NA)
#
# API cost: ~1 call per 100 PRs. Paging stops once PRs are older than
# START_DATE, so we no longer download the repo's whole history.
# =============================================================================

source("R/config.R")
source("R/github_helpers.R")

# First AI-related word in a PR description (template comments removed first)
find_ai_keyword <- function(body) {
  if (is.na(body)) return(NA_character_)
  body <- gsub("(?s)<!--.*?-->", "", body, perl = TRUE)  # drop hidden template text
  m <- regmatches(body, regexpr(AI_PATTERN, body, ignore.case = TRUE, perl = TRUE))
  if (length(m) == 0) NA_character_ else tolower(m)
}

pr_to_row <- function(pr, repo) {
  label_names <- vapply(pr$labels, function(l) or_na(l$name), character(1))
  keyword <- find_ai_keyword(or_na(pr$body))
  data.frame(
    repo             = repo,
    pr_number        = pr$number,
    author           = or_na(pr$user$login),
    author_type      = or_na(pr$user$type),
    title            = or_na(pr$title),
    state            = or_na(pr$state),
    draft            = isTRUE(pr$draft),
    created_at       = or_na(pr$created_at),
    closed_at        = or_na(pr$closed_at),
    merged_at        = or_na(pr$merged_at),
    labels           = paste(label_names, collapse = ";"),
    body_mentions_ai = !is.na(keyword),
    ai_keyword       = keyword,
    stringsAsFactors = FALSE
  )
}

collect_repo_prs <- function(repo) {
  message("Fetching PRs for ", repo, " ...")
  older_than_window <- function(batch) {
    oldest <- parse_time(batch[[length(batch)]]$created_at)
    as.Date(oldest) < START_DATE
  }
  items <- gh_get_all(
    paste0("repos/", repo, "/pulls"),
    query     = list(state = "all", sort = "created", direction = "desc"),
    stop_when = older_than_window,
    label     = repo
  )
  if (is.null(items)) stop("Could not fetch PRs for ", repo, ". Check the repo name and run again.")

  prs <- bind_rows(lapply(items, pr_to_row, repo = repo))
  prs |>
    mutate(created_date = as.Date(parse_time(created_at))) |>
    filter(created_date >= START_DATE, created_date <= END_DATE) |>
    select(-created_date)
}

raw_prs <- bind_rows(lapply(TARGET_REPOS, collect_repo_prs))
save_csv(raw_prs, "raw_prs.csv")
print(count(raw_prs, repo, name = "prs_in_window"))
