# Open Source Network Analysis

COSC 421 network science project by Chanyee Yoon and Carter Ludwig.

We test whether the structure of multilayer collaboration networks can spot unusual
contributors (e.g. people whose PRs keep getting closed without being merged) in
open-source GitHub repositories. Each repository gets a network of contributors with
three layers:

| Layer | Edge | Built from |
|---|---|---|
| Review (directed) | A → B if A reviewed B's PR | `data/cleaned_reviews.csv` |
| Discussion (directed) | A → B if A commented on B's PR | `data/cleaned_comments.csv` |
| Co-editing (undirected) | A — B if both changed the same file within 90 days | `data/cleaned_edits.csv` |

Repositories: `pandas-dev/pandas` and `pola-rs/polars`, PRs created 2025-07-01 to 2026-06-30.

## Setup

1. Install packages: `install.packages(c("httr2", "jsonlite", "dplyr", "igraph"))`
2. Put a GitHub token in `.Renviron` (never commit this file):
   `GITHUB_PAT=ghp_yourtokenhere`
3. Restart R, then open `open-source-network-analysis.Rproj` so paths work.

## Run order

All settings (repos, dates, thresholds) live in `R/config.R`.

| Script | What it does | API calls / time |
|---|---|---|
| `R/01_collect_prs.R` | PRs in the date window | ~1 per 100 PRs, minutes |
| `R/02_collect_reviews.R` | Reviews on each PR | 1 per PR, ~1 h per 3,500 PRs |
| `R/03_collect_comments.R` | Comments on each PR (repo-wide endpoints) | ~1 per 100 comments, minutes |
| `R/04_collect_edits.R` | Files changed by each PR (reuses old pandas data) | 1 per new PR |
| `R/05_process_data.R` | Cleans everything, labels contributors | none, seconds |

Scripts 02 and 04 save progress to `data/checkpoints/` and wait out GitHub's rate limit on
their own. If one stops, run it again and it continues where it left off. To start a
script completely fresh, delete its files in `data/checkpoints/`.

## Data files

**raw_\*.csv** come straight from the API. **cleaned_\*.csv** are what the analysis uses.

- `cleaned_prs.csv`: repo, pr_number, author, author_type, title, state, draft, created_at,
  closed_at, merged_at, labels, body_mentions_ai, ai_keyword, is_merged, is_finished,
  created_date, author_group
- `cleaned_reviews.csv`: repo, pr_number, reviewer, pr_author, review_state, submitted_at,
  reviewer_group, pr_author_group (self-reviews and unsubmitted reviews removed)
- `cleaned_comments.csv`: repo, pr_number, commenter, pr_author, comment_type
  (conversation / code), created_at, commenter_group, pr_author_group (comments on your own PR removed)
- `cleaned_edits.csv`: repo, pr_number, author, pr_created_at, filename, status, additions,
  deletions (release-note files and files in >5% of PRs removed)
- `removed_hot_files.csv`: the files removed from co-editing and why
- `contributors.csv`: one row per contributor per repo: n_prs, n_finished, n_merged,
  merge_rate, n_reviews_given / received, n_comments_given / received,
  n_distinct_feedback_givers, n_files_edited, group (human / bot / ai), is_unusual

**Labels.** `group` is `bot` for GitHub bot accounts, names ending in `[bot]` and known bots;
`ai` for AI agent accounts (e.g. Copilot) or authors whose PR descriptions mostly mention an
AI tool; otherwise `human`. `is_unusual` means 2+ finished PRs with a merge rate under 30%.
These are proxy labels and will contain some noise.
