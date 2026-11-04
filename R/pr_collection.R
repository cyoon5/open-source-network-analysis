library(httr2)
library(jsonlite)
library(dplyr)

pat <- Sys.getenv("GITHUB_PAT")
if (pat == "") {
  stop("GitHub API Key Missing")
}

#Function to fetch all PRs with pagination for a given repository
fetch_repo_prs <- function(repo_slug) {
  base_url <- paste0("https://api.github.com/repos/", repo_slug, "/pulls")
  all_prs_list <- list()
  page <- 1
  
  message("Fetching PRs for: ", repo_slug)
  
  repeat {
    req <- request(base_url) |>
      req_url_query(
        state = "all",
        sort = "created",
        direction = "desc",
        per_page = 100,
        page = page
      ) |>
      req_auth_bearer_token(pat)
    
    resp <- req |> req_perform()
    
    if (resp_status(resp) != 200) {
      warning("API request failed on page ", page, " for ", repo_slug)
      break
    }
    
    page_data <- resp_body_json(resp, simplifyVector = FALSE)
    
    # Break loop if page is empty
    if (length(page_data) == 0) {
      break
    }
    
    all_prs_list[[page]] <- page_data
    
    # If fewer than 100 items returned, this is the last page
    if (length(page_data) < 100) {
      break
    }
    
    page <- page + 1
    Sys.sleep(0.2)
  }
  
  if (length(all_prs_list) == 0) {
    return(data.frame())
  }
  
  # Flatten the nested list structure
  flat_prs <- unlist(all_prs_list, recursive = FALSE)
  message("Total PRs retrieved: ", length(flat_prs))
  
  # Extract necessary fields safely into a data frame
  prs_df <- data.frame(
    repo = repo_slug,
    pr_number = vapply(flat_prs, function(pr) pr$number, integer(1)),
    author = vapply(flat_prs, function(pr) ifelse(is.null(pr$user$login), NA_character_, pr$user$login), character(1)),
    title = vapply(flat_prs, function(pr) ifelse(is.null(pr$title), NA_character_, pr$title), character(1)),
    state = vapply(flat_prs, function(pr) ifelse(is.null(pr$state), NA_character_, pr$state), character(1)),
    created_at = vapply(flat_prs, function(pr) ifelse(is.null(pr$created_at), NA_character_, pr$created_at), character(1)),
    closed_at = vapply(flat_prs, function(pr) ifelse(is.null(pr$closed_at), NA_character_, pr$closed_at), character(1)),
    merged_at = vapply(flat_prs, function(pr) ifelse(is.null(pr$merged_at), NA_character_, pr$merged_at), character(1)),
    merged_by = vapply(flat_prs, function(pr) {
      if (is.null(pr$merged_by) || is.null(pr$merged_by$login)) NA_character_ else pr$merged_by$login
    }, character(1)),
    stringsAsFactors = FALSE
  )
  
  return(prs_df)
}

target_repos <- c(
  "tidyverse/dplyr"
  
)

all_repos_prs <- list()

for (repo_slug in target_repos) {
  repo_data <- fetch_repo_prs(repo_slug)
  if (nrow(repo_data) > 0) {
    all_repos_prs[[repo_slug]] <- repo_data
  }
}

if (length(all_repos_prs) > 0) {
  combined_raw_prs <- dplyr::bind_rows(all_repos_prs)
  
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  write.csv(combined_raw_prs, "data/raw_prs.csv", row.names = FALSE)
  message("Successfully saved a combined total of ", nrow(combined_raw_prs), " PRs from all repositories to data/raw_prs.csv!")
} else {
  warning("No PRs collected from any repository.")
}

