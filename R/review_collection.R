library(httr2)
library(jsonlite)
library(dplyr)

pat <- Sys.getenv("GITHUB_PAT")
if (pat == "") stop("GitHub API Key Missing")

cleaned_prs <- read.csv("data/cleaned_prs.csv", stringsAsFactors = FALSE)

if (!all(c("repo", "pr_number") %in% names(cleaned_prs))) {
  stop("cleaned_prs.csv must contain both 'repo' and 'pr_number' columns.")
}

all_reviews <- list()
message("Fetching raw reviews dynamically for ", nrow(cleaned_prs), " PRs across repositories...")

for (i in seq_len(nrow(cleaned_prs))) {
  target_repo <- cleaned_prs$repo[i]
  pr_num <- cleaned_prs$pr_number[i]
  
  url <- paste0("https://api.github.com/repos/", target_repo, "/pulls/", pr_num, "/reviews")
  
  resp <- request(url) |>
    req_auth_bearer_token(pat) |>
    req_headers(Accept = "application/vnd.github+json") |>
    req_perform()
  
  if (resp_status(resp) == 200) {
    reviews_data <- resp_body_json(resp, simplifyVector = FALSE)
    if (length(reviews_data) > 0) {
      for (rev in reviews_data) {
        all_reviews[[length(all_reviews) + 1]] <- data.frame(
          repo = target_repo,
          pr_number = pr_num,
          reviewer = if (!is.null(rev$user$login)) rev$user$login else NA_character_,
          author = cleaned_prs$author[i],
          state = if (!is.null(rev$state)) rev$state else NA_character_,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  Sys.sleep(0.1)
}

if (length(all_reviews) > 0) {
  raw_reviews_df <- bind_rows(all_reviews)
  write.csv(raw_reviews_df, "data/raw_reviews.csv", row.names = FALSE)
  message("Successfully saved dynamic raw reviews to data/raw_reviews.csv!")
}