# =============================================================================
# 02_collect_reviews.R - every review on every PR from raw_prs.csv.
#
# Output: data/raw_reviews.csv, one row per review
#   repo, pr_number, reviewer, reviewer_type, review_state, submitted_at
#
# API cost: 1 call per PR (about 1 hour per 3,500 PRs once rate limits are
# counted). Safe to stop and re-run: finished PRs are skipped.
# =============================================================================

source("R/config.R")
source("R/github_helpers.R")

prs <- load_csv("raw_prs.csv")

fetch_reviews <- function(repo, pr_number) {
  items <- gh_get_all(paste0("repos/", repo, "/pulls/", pr_number, "/reviews"))
  if (is.null(items)) return(NULL)
  if (length(items) == 0) return(data.frame())
  data.frame(
    repo          = repo,
    pr_number     = pr_number,
    reviewer      = vapply(items, function(r) or_na(r$user$login), character(1)),
    reviewer_type = vapply(items, function(r) or_na(r$user$type), character(1)),
    review_state  = vapply(items, function(r) or_na(r$state), character(1)),
    submitted_at  = vapply(items, function(r) or_na(r$submitted_at), character(1)),
    stringsAsFactors = FALSE
  )
}

raw_reviews <- run_per_pr(prs, fetch_reviews, name = "reviews")
save_csv(raw_reviews, "raw_reviews.csv")
print(count(raw_reviews, repo, name = "reviews"))
