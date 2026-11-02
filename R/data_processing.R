library(dplyr)

# Loads raw PR data
raw_prs <- read.csv("data/raw_prs.csv", stringsAsFactors = FALSE)

# Timeframe: Summer 2025 to Summer 2026 (July 1, 2025 to June 30, 2026)
cleaned_prs <- raw_prs |>
  mutate(
    created_date = as.Date(substr(created_at, 1, 10))
  ) |>
  filter(created_date >= as.Date("2025-07-01") & created_date <= as.Date("2026-06-30"))

# check on how many PRs remain after filtering
message("Original PR count: ", nrow(raw_prs))
message("Filtered PR count within Summer 2025 - Summer 2026: ", nrow(cleaned_prs))

# Save the cleaned dataset for network analysis
write.csv(cleaned_prs, "data/cleaned_prs.csv", row.names = FALSE)
message("Successfully saved cleaned data to data/cleaned_prs.csv!")