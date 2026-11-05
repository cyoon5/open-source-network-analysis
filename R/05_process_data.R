# =============================================================================
# 05_process_data.R - clean the raw data and label contributors.
# No API calls; runs in seconds. Re-run any time you change config.R.
#
# Outputs (all in data/):
#   cleaned_prs.csv       one row per PR, + author_group, is_merged, is_finished
#   cleaned_reviews.csv   REVIEW layer edges: reviewer -> pr_author
#   cleaned_comments.csv  DISCUSSION layer edges: commenter -> pr_author
#   cleaned_edits.csv     CO-EDITING layer input: author + file + PR date
#   removed_hot_files.csv files dropped from co-editing (mention in the report)
#   contributors.csv      one row per contributor per repo, with counts,
#                         merge rate, group (human/bot/ai) and is_unusual
# =============================================================================

source("R/config.R")
library(dplyr)

prs      <- load_csv("raw_prs.csv")
reviews  <- load_csv("raw_reviews.csv")
comments <- load_csv("raw_comments.csv")
edits    <- load_csv("raw_edits.csv")

# --- Who is a bot / AI agent? -------------------------------------------------
# An account is a bot if GitHub says so in ANY row, its name ends in [bot],
# or it is in KNOWN_BOTS. AI agent accounts (Copilot etc.) form their own group.
account_types <- bind_rows(
  prs      |> select(login = author,    type = author_type),
  reviews  |> select(login = reviewer,  type = reviewer_type),
  comments |> select(login = commenter, type = commenter_type)
) |>
  filter(!is.na(login)) |>
  group_by(login) |>
  summarise(api_bot = any(type == "Bot", na.rm = TRUE), .groups = "drop") |>
  mutate(account_group = case_when(
    login %in% AI_ACCOUNTS                                          ~ "ai_agent",
    api_bot | grepl("\\[bot\\]$", login) | login %in% KNOWN_BOTS    ~ "bot",
    TRUE                                                            ~ "human"
  )) |>
  select(login, account_group)

group_of <- function(df, col, new_col) {
  df |>
    left_join(account_types, by = setNames("login", col)) |>
    rename(!!new_col := account_group)
}

# --- PRs ---------------------------------------------------------------------
cleaned_prs <- prs |>
  filter(!is.na(author)) |>
  mutate(
    is_merged   = !is.na(merged_at),
    is_finished = state == "closed",            # merged or closed; open PRs are undecided
    created_date = as.Date(parse_time(created_at))
  ) |>
  group_of("author", "author_group")

pr_authors <- cleaned_prs |> select(repo, pr_number, pr_author = author)

# --- Review layer: reviewer -> PR author --------------------------------------
cleaned_reviews <- reviews |>
  inner_join(pr_authors, by = c("repo", "pr_number")) |>
  filter(
    !is.na(reviewer),
    review_state != "PENDING",                  # unsubmitted drafts
    tolower(reviewer) != tolower(pr_author)     # no self-reviews
  ) |>
  group_of("reviewer", "reviewer_group") |>
  group_of("pr_author", "pr_author_group") |>
  select(repo, pr_number, reviewer, pr_author, review_state, submitted_at,
         reviewer_group, pr_author_group)

# --- Discussion layer: commenter -> PR author ---------------------------------
cleaned_comments <- comments |>
  inner_join(pr_authors, by = c("repo", "pr_number")) |>
  filter(
    !is.na(commenter),
    tolower(commenter) != tolower(pr_author)    # replying on your own PR is not an edge
  ) |>
  group_of("commenter", "commenter_group") |>
  group_of("pr_author", "pr_author_group") |>
  select(repo, pr_number, commenter, pr_author, comment_type, created_at,
         commenter_group, pr_author_group)

# --- Co-editing layer input ---------------------------------------------------
edits <- edits |>
  filter(!is.na(filename), filename != "") |>
  inner_join(cleaned_prs |> select(repo, pr_number, author, pr_created_at = created_at),
             by = c("repo", "pr_number"))

prs_per_repo <- edits |> distinct(repo, pr_number) |> count(repo, name = "repo_prs")

file_share <- edits |>
  distinct(repo, pr_number, filename) |>
  count(repo, filename, name = "file_prs") |>
  left_join(prs_per_repo, by = "repo") |>
  mutate(share = file_prs / repo_prs)

removed_hot_files <- file_share |>
  filter(share > HOT_FILE_SHARE | grepl(EXCLUDED_FILE_PATTERN, filename)) |>
  arrange(repo, desc(share))

cleaned_edits <- edits |>
  anti_join(removed_hot_files, by = c("repo", "filename")) |>
  select(repo, pr_number, author, pr_created_at, filename, status, additions, deletions)

# --- One row per contributor per repo -----------------------------------------
pr_stats <- cleaned_prs |>
  group_by(repo, login = author) |>
  summarise(
    n_prs            = n(),
    n_finished       = sum(is_finished),
    n_merged         = sum(is_merged),
    n_ai_mention_prs = sum(body_mentions_ai),
    .groups = "drop"
  )

feedback <- bind_rows(
  cleaned_reviews  |> select(repo, giver = reviewer,  receiver = pr_author),
  cleaned_comments |> select(repo, giver = commenter, receiver = pr_author)
)

all_logins <- bind_rows(
  cleaned_prs      |> select(repo, login = author),
  cleaned_reviews  |> select(repo, login = reviewer),
  cleaned_comments |> select(repo, login = commenter)
) |> distinct()

contributors <- all_logins |>
  left_join(pr_stats, by = c("repo", "login")) |>
  left_join(count(cleaned_reviews,  repo, login = reviewer,  name = "n_reviews_given"),     by = c("repo", "login")) |>
  left_join(count(cleaned_reviews,  repo, login = pr_author, name = "n_reviews_received"),  by = c("repo", "login")) |>
  left_join(count(cleaned_comments, repo, login = commenter, name = "n_comments_given"),    by = c("repo", "login")) |>
  left_join(count(cleaned_comments, repo, login = pr_author, name = "n_comments_received"), by = c("repo", "login")) |>
  left_join(feedback |> distinct(repo, receiver, giver) |> count(repo, login = receiver, name = "n_distinct_feedback_givers"),
            by = c("repo", "login")) |>
  left_join(cleaned_edits |> distinct(repo, author, filename) |> count(repo, login = author, name = "n_files_edited"),
            by = c("repo", "login")) |>
  mutate(across(starts_with("n_"), ~ coalesce(as.numeric(.x), 0))) |>
  left_join(account_types, by = "login") |>
  mutate(
    merge_rate = ifelse(n_finished > 0, n_merged / n_finished, NA_real_),
    # group: what kind of ACCOUNT this is (AI agent accounts like Copilot, bots, people)
    group = case_when(
      account_group == "ai_agent" ~ "ai_agent",
      account_group == "bot" ~ "bot",
      TRUE                   ~ "human"
    ),
    # ai_assisted: a person whose PR descriptions mostly mention an AI tool.
    # Kept separate from group so AI-assisted maintainers still count as humans.
    ai_assisted = n_prs > 0 & n_ai_mention_prs / n_prs >= 0.5,
    is_unusual = coalesce(n_finished >= UNUSUAL_MIN_PRS & merge_rate < UNUSUAL_MAX_MERGE_RATE, FALSE)
  ) |>
  select(-account_group) |>
  arrange(repo, desc(n_prs))

# --- Save --------------------------------------------------------------------
save_csv(cleaned_prs,       "cleaned_prs.csv")
save_csv(cleaned_reviews,   "cleaned_reviews.csv")
save_csv(cleaned_comments,  "cleaned_comments.csv")
save_csv(cleaned_edits,     "cleaned_edits.csv")
save_csv(removed_hot_files, "removed_hot_files.csv")
save_csv(contributors,      "contributors.csv")

# --- Sanity checks: read these before moving on ------------------------------
message("\nRows per repo (every repo should have reviews, comments and edits):")
print(bind_rows(
  count(cleaned_prs,      repo) |> mutate(table = "prs"),
  count(cleaned_reviews,  repo) |> mutate(table = "reviews"),
  count(cleaned_comments, repo) |> mutate(table = "comments"),
  count(cleaned_edits,    repo) |> mutate(table = "edits")
))

message("\nContributors by group:")
print(count(contributors, repo, group))

message("\nAI-assisted contributors (most PR descriptions mention an AI tool):")
print(count(contributors, repo, ai_assisted))

message("\nUnusual contributors (", UNUSUAL_MIN_PRS, "+ finished PRs, merge rate < ",
        UNUSUAL_MAX_MERGE_RATE, "):")
print(count(contributors, repo, is_unusual))

# If most PRs "mention AI", the repo's PR template probably contains an AI
# question; then tighten AI_PATTERN in config.R or ignore body_mentions_ai.
ai_share <- cleaned_prs |> group_by(repo) |> summarise(share = mean(body_mentions_ai))
message("\nShare of PRs whose description mentions an AI tool:")
print(ai_share)
if (any(ai_share$share > 0.3)) {
  warning("Over 30% of PRs mention AI in some repo. Check ai_keyword in cleaned_prs.csv ",
          "before trusting the ai_assisted column.", call. = FALSE)
}

message("\nMost common hot files removed from co-editing:")
print(head(removed_hot_files, 10))
