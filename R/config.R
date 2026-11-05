# =============================================================================
# config.R - shared settings used by every script.
# Change the repositories, date window or thresholds HERE only.
# =============================================================================

# Repositories to study (owner/name)
TARGET_REPOS <- c(
  "pandas-dev/pandas",
  "pola-rs/polars"
)

# Only PRs created inside this window are kept
START_DATE <- as.Date("2025-07-01")
END_DATE   <- as.Date("2026-06-30")

# Folders
DATA_DIR       <- "data"
CHECKPOINT_DIR <- file.path(DATA_DIR, "checkpoints")
dir.create(CHECKPOINT_DIR, recursive = TRUE, showWarnings = FALSE)

# --- Labelling contributors --------------------------------------------------

# Accounts that ARE AI coding agents
AI_ACCOUNTS <- c(
  "Copilot", "copilot-swe-agent[bot]", "devin-ai-integration[bot]",
  "claude[bot]", "chatgpt-codex-connector[bot]"
)

# Bot accounts that GitHub labels as normal users (e.g. pandas' backport bot)
KNOWN_BOTS <- c("meeseeksmachine")

# Words in a PR description that suggest an AI tool was used
AI_PATTERN <- paste(
  "copilot", "claude", "chatgpt", "gpt-4", "gpt-5", "codex", "devin", "gemini",
  "ai[- ]generated", "generated (with|by|using) (an )?(ai|llm)", "\\bllm\\b",
  sep = "|"
)

# "Unusual" contributor = at least this many finished PRs ...
UNUSUAL_MIN_PRS <- 2
# ... and a merge rate below this
UNUSUAL_MAX_MERGE_RATE <- 0.30

# --- Co-editing layer --------------------------------------------------------

# Drop files touched by more than this share of a repo's PRs (they connect everyone)
HOT_FILE_SHARE <- 0.05
# Always drop release-note / changelog files
EXCLUDED_FILE_PATTERN <- "whatsnew/|^CHANGELOG|^NEWS"

# --- Small CSV helpers (UTF-8 so names and titles survive on Windows) --------

save_csv <- function(df, name) {
  write.csv(df, file.path(DATA_DIR, name), row.names = FALSE, fileEncoding = "UTF-8")
  message("Saved ", nrow(df), " rows to ", file.path(DATA_DIR, name))
}

load_csv <- function(name) {
  path <- file.path(DATA_DIR, name)
  if (!file.exists(path)) stop(path, " not found. Run the earlier scripts first.")
  read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
}

# Append rows to a CSV, writing the header only the first time
append_csv <- function(df, path) {
  write.table(df, path, sep = ",", qmethod = "double", row.names = FALSE,
              col.names = !file.exists(path), append = file.exists(path),
              fileEncoding = "UTF-8")
}

# Convert GitHub timestamps ("2025-07-01T07:55:04Z") to R date-times
parse_time <- function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
