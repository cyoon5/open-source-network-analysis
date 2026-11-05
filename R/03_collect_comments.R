# =============================================================================
# 03_collect_comments.R - comments on PRs from raw_prs.csv (discussion layer).
#
# Uses two repo-wide endpoints instead of one call per PR:
#   /issues/comments  -> normal conversation comments on a PR
#   /pulls/comments   -> comments on specific lines of code in a PR
# Both endpoints also return comments on plain issues; those are dropped.
#
# Output: data/raw_comments.csv, one row per comment
#   repo, pr_number, commenter, commenter_type, comment_type
#   ("conversation" or "code"), created_at
#
# API cost: ~1 call per 100 comments in the repo since START_DATE.
# Each repo/endpoint is saved to data/checkpoints/ once finished, so a re-run
# skips what is already done.
# =============================================================================

source("R/config.R")
source("R/github_helpers.R")

prs <- load_csv("raw_prs.csv")

collect_comments <- function(repo, endpoint, url_field, comment_type) {
  ckpt <- file.path(CHECKPOINT_DIR, paste0("comments_", gsub("/", "_", repo), "_", comment_type, ".csv"))
  if (file.exists(ckpt)) {
    message("Already collected ", comment_type, " comments for ", repo, " (", ckpt, ")")
    return(read.csv(ckpt, stringsAsFactors = FALSE, fileEncoding = "UTF-8"))
  }

  message("Fetching ", comment_type, " comments for ", repo, " ...")
  items <- gh_get_all(
    paste0("repos/", repo, "/", endpoint),
    query = list(since = paste0(START_DATE, "T00:00:00Z"), sort = "created", direction = "asc"),
    label = paste(repo, comment_type)
  )
  if (is.null(items)) stop("Could not fetch ", endpoint, " for ", repo, ". Run the script again.")
  if (length(items) == 0) return(data.frame())

  comments <- data.frame(
    repo           = repo,
    # the PR number is the last part of the link, e.g. ".../issues/61234"
    pr_number      = as.integer(sub(".*/", "", vapply(items, function(x) or_na(x[[url_field]]), character(1)))),
    commenter      = vapply(items, function(x) or_na(x$user$login), character(1)),
    commenter_type = vapply(items, function(x) or_na(x$user$type), character(1)),
    comment_type   = comment_type,
    created_at     = vapply(items, function(x) or_na(x$created_at), character(1)),
    stringsAsFactors = FALSE
  ) |>
    semi_join(prs, by = c("repo", "pr_number"))   # keep only PRs in our window

  write.csv(comments, ckpt, row.names = FALSE, fileEncoding = "UTF-8")
  comments
}

raw_comments <- bind_rows(lapply(TARGET_REPOS, function(repo) {
  bind_rows(
    collect_comments(repo, "issues/comments", "issue_url",        "conversation"),
    collect_comments(repo, "pulls/comments",  "pull_request_url", "code")
  )
}))

save_csv(raw_comments, "raw_comments.csv")
print(count(raw_comments, repo, comment_type, name = "comments"))
