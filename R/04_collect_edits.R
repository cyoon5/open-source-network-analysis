# =============================================================================
# 04_collect_edits.R - files changed by every PR in raw_prs.csv (co-editing layer).
#
# Output: data/raw_edits.csv, one row per (PR, file)
#   repo, pr_number, filename, status, additions, deletions
#
# API cost: 1 call per PR. The first time this runs it REUSES the edits already
# in data/raw_edits.csv (pandas was collected before), so only new PRs
# (e.g. polars) are downloaded. Safe to stop and re-run.
# =============================================================================

source("R/config.R")
source("R/github_helpers.R")

prs <- load_csv("raw_prs.csv")

# --- Reuse previously collected edits ----------------------------------------
ckpt      <- file.path(CHECKPOINT_DIR, "edits.csv")
done_file <- file.path(CHECKPOINT_DIR, "edits_done.csv")
old_file  <- file.path(DATA_DIR, "raw_edits.csv")

if (!file.exists(ckpt) && file.exists(old_file)) {
  old <- read.csv(old_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8") |>
    semi_join(prs, by = c("repo", "pr_number")) |>
    select(repo, pr_number, filename, status, additions, deletions)
  if (nrow(old) > 0) {
    append_csv(old, ckpt)
    append_csv(distinct(old, repo, pr_number), done_file)
    message("Reused ", nrow(old), " existing edit rows covering ",
            nrow(distinct(old, repo, pr_number)), " PRs.")
  }
}

# --- Fetch the rest -----------------------------------------------------------
fetch_files <- function(repo, pr_number) {
  items <- gh_get_all(paste0("repos/", repo, "/pulls/", pr_number, "/files"))
  if (is.null(items)) return(NULL)
  if (length(items) == 0) return(data.frame())
  data.frame(
    repo      = repo,
    pr_number = pr_number,
    filename  = vapply(items, function(f) or_na(f$filename), character(1)),
    status    = vapply(items, function(f) or_na(f$status), character(1)),
    additions = vapply(items, function(f) as.numeric(or_na(f$additions, NA_real_)), numeric(1)),
    deletions = vapply(items, function(f) as.numeric(or_na(f$deletions, NA_real_)), numeric(1)),
    stringsAsFactors = FALSE
  )
}

raw_edits <- run_per_pr(prs, fetch_files, name = "edits")
save_csv(raw_edits, "raw_edits.csv")
print(count(raw_edits, repo, name = "file_edits"))
