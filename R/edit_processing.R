library(dplyr)

# Load raw file edits
raw_edits <- read.csv("data/raw_edits.csv", stringsAsFactors = FALSE)

# Process and clean
cleaned_edits <- raw_edits |>
  filter(
    !is.na(filename), 
    !is.na(author), 
    !is.na(repo),
    filename != ""
  ) |>
  mutate(
    filename = trimws(filename),
    status = trimws(status)
  ) |>
  select(repo, pr_number, author, filename, status, additions, deletions, changes)

# Save processed output for network construction
write.csv(cleaned_edits, "data/cleaned_edits.csv", row.names = FALSE)
message("Successfully processed and saved ", nrow(cleaned_edits), " file edit records to data/cleaned_edits.csv!")