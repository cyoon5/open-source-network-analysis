library(dplyr)

# Load raw reviews
raw_reviews <- read.csv("data/raw_reviews.csv", stringsAsFactors = FALSE)

# Process and clean with case-insensitivity and bot filtering
cleaned_reviews <- raw_reviews |>
  mutate(
    reviewer_lower = tolower(reviewer),
    author_lower = tolower(author)
  ) |>
  filter(
    !is.na(reviewer),
    reviewer_lower != author_lower,
    !grepl("\\[bot\\]$", reviewer, ignore.case = TRUE) # filter out automated bot accounts if needed
  ) |>
  select(-reviewer_lower, -author_lower) # drop helper columns

write.csv(cleaned_reviews, "data/cleaned_reviews.csv", row.names = FALSE)
message("Successfully processed and saved ", nrow(cleaned_reviews), " reviews to data/cleaned_reviews.csv!")