library(httr2)
library(jsonlite)
library(dplyr)

pat <- Sys.getenv("GITHUB_PAT")
if (pat == "") stop("GitHub API Key Missing")

cleaned_prs <- read.csv("data/cleaned_prs.csv", stringsAsFactors = FALSE)

if (!all(c("repo", "pr_number") %in% names(cleaned_prs))) {
  stop("cleaned_prs.csv must contain both 'repo' and 'pr_number' columns.")
}

all_edits <- list()
message("Fetching raw file edits dynamically for ", nrow(cleaned_prs), " PRs across repositories...")

for (i in seq_len(nrow(cleaned_prs))) {
  target_repo <- cleaned_prs$repo[i]
  pr_num <- cleaned_prs$pr_number[i]
  pr_author <- cleaned_prs$author[i]
  
  url <- paste0("https://api.github.com/repos/", target_repo, "/pulls/", pr_num, "/files")
  
  resp <- request(url) |>
    req_auth_bearer_token(pat) |>
    req_headers(Accept = "application/vnd.github+json") |>
    req_perform()
  
  if (resp_status(resp) == 200) {
    files_data <- resp_body_json(resp, simplifyVector = FALSE)
    
    if (length(files_data) > 0) {
      for (file_obj in files_data) {
        all_edits[[length(all_edits) + 1]] <- data.frame(
          repo = target_repo,
          pr_number = pr_num,
          author = pr_author,
          filename = if (!is.null(file_obj$filename)) file_obj$filename else NA_character_,
          status = if (!is.null(file_obj$status)) file_obj$status else NA_character_,
          additions = if (!is.null(file_obj$additions)) file_obj$additions else 0,
          deletions = if (!is.null(file_obj$deletions)) file_obj$deletions else 0,
          changes = if (!is.null(file_obj$changes)) file_obj$changes else 0,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  Sys.sleep(0.1)
}

if (length(all_edits) > 0) {
  raw_edits_df <- bind_rows(all_edits)
  write.csv(raw_edits_df, "data/raw_edits.csv", row.names = FALSE)
  message("Successfully saved raw file edits to data/raw_edits.csv!")
} else {
  message("No file edits found.")
}