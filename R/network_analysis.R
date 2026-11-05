library(data.table)
library(igraph)

# Multilayer contributor networks, one per repository.
#
# Nodes are contributors: anyone who opened, reviewed or commented on a PR in
# the repository. Every repository gets three layers over the same node set:
#
#   review      (directed)    A -> B if A reviewed B's PR.   weight = reviews
#   discussion  (directed)    A -> B if A commented on B's PR. weight = comments
#   coedit      (undirected)  A -- B if both changed the same file within
#                             `window` days of each other.   weight = shared files
#
# A contributor is "unusual" if they opened at least 2 PRs and fewer than 30%
# of them were merged. Bot accounts (login ending in "[bot]") are labelled.
#
# Usage (from the project root):
#   source("R/network_analysis.R")
#   prs     <- read.csv("data/cleaned_prs.csv")
#   reviews <- read.csv("data/cleaned_reviews.csv")
#   edits   <- read.csv("data/cleaned_edits.csv")
#   nets    <- build_all_multilayers(prs, reviews, comments = NULL, edits)
#   pandas  <- nets[["pandas-dev/pandas"]]
#   layer_summary(pandas)
#   node_metrics(pandas)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

is_bot <- function(login) grepl("\\[bot\\]$", login, ignore.case = TRUE)

check_columns <- function(dt, required, arg) {
  missing <- setdiff(required, names(dt))
  if (length(missing)) {
    stop("`", arg, "` is missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

# Undirected view of a layer: A -> B and B -> A become one edge, weights summed.
collapse_layer <- function(g) {
  as_undirected(g, mode = "collapse", edge.attr.comb = list(weight = "sum", "ignore"))
}

# Number of distinct people each node is connected to, ignoring direction.
n_neighbours <- function(g) degree(collapse_layer(g))

empty_edges <- function() data.table(from = character(), to = character(), weight = integer())

#' Per-author PR outcomes for one repository
#'
#' @param prs data.table of PRs with `author` and `merged_at`.
#' @param min_prs,max_merge_rate An author is unusual if they opened at least
#'   `min_prs` PRs and fewer than `max_merge_rate` of them were merged.
#' @return data.table with `name`, `n_prs`, `n_merged`, `merge_rate`, `unusual`.
contributor_stats <- function(prs, min_prs = 2, max_merge_rate = 0.3) {
  stats <- prs[, .(n_prs = .N, n_merged = sum(!is.na(merged_at) & merged_at != "")),
               by = .(name = author)]
  stats[, merge_rate := n_merged / n_prs]
  stats[, unusual := n_prs >= min_prs & merge_rate < max_merge_rate]
  stats[]
}

# ---------------------------------------------------------------------------
# Network construction
# ---------------------------------------------------------------------------

#' Build the three-layer contributor network for one repository
#'
#' @param repo_slug Repository to build, e.g. "pandas-dev/pandas".
#' @param prs data.frame with `repo`, `pr_number`, `author`, `created_at`,
#'   `merged_at`; one row per PR. Only reviews, comments and edits on these PRs
#'   are used, so filter PRs by date before calling.
#' @param reviews Optional data.frame with `repo`, `pr_number`, `reviewer`;
#'   one row per review.
#' @param comments Optional data.frame with `repo`, `pr_number`, `commenter`;
#'   one row per comment (issue and review comments together).
#' @param edits Optional data.frame with `repo`, `pr_number`, `filename`;
#'   one row per file changed by a PR. The edit date is the PR's creation date.
#' @param window Maximum days between two PRs touching the same file for their
#'   authors to count as co-editors. Default 90.
#' @param max_file_share Files changed by more than this share of the
#'   repository's PRs are dropped from the co-editing layer. Default 0.10.
#' @param exclude_files Regex of files to drop from the co-editing layer
#'   (release notes and changelogs by default). NULL to keep all files.
#' @param drop_bots If TRUE, remove bot accounts from the network.
#' @param min_prs,max_merge_rate Passed to [contributor_stats()].
#' @return A named list of three igraph objects (`review`, `discussion`,
#'   `coedit`) sharing the same vertices in the same order. Vertex attributes:
#'   `n_prs`, `n_merged`, `merge_rate`, `unusual`, `is_bot`. Edge attribute:
#'   `weight`. Graph attributes: `repo`, `layer`. Self-loops are dropped.
build_multilayer <- function(repo_slug, prs, reviews = NULL, comments = NULL, edits = NULL,
                             window = 90, max_file_share = 0.10,
                             exclude_files = "whatsnew/|release[-_ ]?notes|(^|/)(news|changelog|changes)(\\.[a-z]+)?$",
                             drop_bots = FALSE, min_prs = 2, max_merge_rate = 0.3) {
  prs <- as.data.table(prs)
  check_columns(prs, c("repo", "pr_number", "author", "created_at", "merged_at"), "prs")
  prs <- prs[repo == repo_slug & !is.na(author)]
  if (!nrow(prs)) stop("No PRs found for repo '", repo_slug, "'.", call. = FALSE)
  pr_info <- prs[, .(pr_number, author, date = as.Date(substr(created_at, 1, 10)))]

  # Directed layer: actor -> PR author, weight = number of reviews/comments
  feedback_layer <- function(dt, actor, arg) {
    if (is.null(dt)) return(list(edges = empty_edges(), actors = character()))
    dt <- as.data.table(dt)
    check_columns(dt, c("repo", "pr_number", actor), arg)
    dt <- dt[repo == repo_slug & !is.na(get(actor)), .(pr_number, from = get(actor))]
    e <- merge(dt, pr_info[, .(pr_number, to = author)], by = "pr_number")
    list(edges = e[tolower(from) != tolower(to), .(weight = .N), by = .(from, to)],
         actors = unique(e$from))
  }

  # Undirected layer: authors whose PRs changed the same file within `window` days
  coedit_layer <- function(dt) {
    if (is.null(dt)) return(empty_edges())
    dt <- as.data.table(dt)
    check_columns(dt, c("repo", "pr_number", "filename"), "edits")
    e <- unique(dt[repo == repo_slug & !is.na(filename) & filename != "", .(pr_number, filename)])
    e <- merge(e, pr_info, by = "pr_number")

    # Drop files that nearly every PR touches; they would connect everyone
    n_prs <- uniqueN(e$pr_number)
    share <- e[, .(share = uniqueN(pr_number) / n_prs), by = filename]
    drop <- share[share > max_file_share, filename]
    if (!is.null(exclude_files)) {
      drop <- union(drop, share[grepl(exclude_files, filename, ignore.case = TRUE), filename])
    }
    e <- unique(e[!filename %in% drop, .(user = author, filename, date)])

    e[, `:=`(lo = date - window, hi = date + window)]
    pairs <- e[e, on = .(filename, date >= lo, date <= hi),
               .(a = x.user, b = i.user, filename), allow.cartesian = TRUE, nomatch = NULL]
    pairs[a < b, .(weight = uniqueN(filename)), by = .(from = a, to = b)]
  }

  review <- feedback_layer(reviews, "reviewer", "reviews")
  discussion <- feedback_layer(comments, "commenter", "comments")
  edges <- list(review = review$edges, discussion = discussion$edges, coedit = coedit_layer(edits))

  # Nodes: everyone who opened, reviewed or commented on a PR
  nodes <- data.table(name = unique(c(prs$author, review$actors, discussion$actors)))
  nodes <- merge(nodes, contributor_stats(prs, min_prs, max_merge_rate), by = "name", all.x = TRUE)
  nodes[is.na(n_prs), `:=`(n_prs = 0L, n_merged = 0L, unusual = FALSE)]
  nodes[, is_bot := is_bot(name)]
  if (drop_bots) {
    nodes <- nodes[is_bot == FALSE]
    edges <- lapply(edges, function(el) el[from %in% nodes$name & to %in% nodes$name])
  }

  directed <- c(review = TRUE, discussion = TRUE, coedit = FALSE)
  layers <- lapply(names(edges), function(layer) {
    g <- graph_from_data_frame(edges[[layer]], directed = directed[[layer]], vertices = nodes)
    g$repo <- repo_slug
    g$layer <- layer
    g
  })
  setNames(layers, names(edges))
}

#' Build one multilayer network per repository found in `prs`
#'
#' @inheritParams build_multilayer
#' @param ... Further arguments passed to [build_multilayer()].
#' @return A named list (by repo) of multilayer networks.
build_all_multilayers <- function(prs, reviews = NULL, comments = NULL, edits = NULL, ...) {
  repos <- unique(prs$repo)
  setNames(lapply(repos, build_multilayer, prs = prs, reviews = reviews,
                  comments = comments, edits = edits, ...), repos)
}

# ---------------------------------------------------------------------------
# Network metrics
# ---------------------------------------------------------------------------

#' Size and connectivity of each layer
#'
#' Computed on the contributors active in the layer (at least one edge), so a
#' layer is not padded with people who only appear in other layers.
#'
#' @param ml A multilayer network from [build_multilayer()].
#' @return data.table, one row per layer: `nodes`, `edges`, `avg_degree`
#'   (mean number of distinct people connected to), `density`, `components`
#'   (weak, for directed layers) and `giant_share` (share of nodes in the
#'   largest component).
layer_summary <- function(ml) {
  rbindlist(lapply(ml, function(g) {
    g <- induced_subgraph(g, which(degree(g) > 0))
    comp <- components(g, mode = "weak")
    data.table(repo = g$repo, layer = g$layer, directed = is_directed(g),
               nodes = vcount(g), edges = ecount(g),
               avg_degree = mean(n_neighbours(g)), density = edge_density(g),
               components = comp$no, giant_share = max(comp$csize, 0) / vcount(g))
  }))
}

#' Per-contributor metrics across all layers
#'
#' For each layer, columns are prefixed with the layer name:
#'   `_in_degree`, `_out_degree` (directed layers only): distinct people who
#'     gave feedback on this contributor's PRs / whose PRs they gave feedback on.
#'   `_degree`: distinct people connected to, ignoring direction.
#'   `_betweenness`: normalized, unweighted, ignoring direction (bridging).
#'   `_pagerank`: weighted; in directed layers rank flows from reviewer or
#'     commenter to PR author.
#'   `_eigenvector`: weighted, ignoring direction.
#' Plus `n_feedback_givers` (distinct people who reviewed or commented on the
#' contributor's PRs, across both layers) and `n_layers` (layers the
#' contributor has at least one edge in).
#'
#' @param ml A multilayer network from [build_multilayer()].
#' @return data.table, one row per contributor.
node_metrics <- function(ml) {
  nodes <- as.data.table(as_data_frame(ml[[1]], what = "vertices"))
  nodes[, repo := ml[[1]]$repo]

  for (layer in names(ml)) {
    g <- ml[[layer]]
    u <- collapse_layer(g)
    col <- function(x) paste0(layer, "_", x)
    has_edges <- ecount(g) > 0
    if (is_directed(g)) {
      nodes[, (col("in_degree")) := degree(g, mode = "in")]
      nodes[, (col("out_degree")) := degree(g, mode = "out")]
    }
    nodes[, (col("degree")) := degree(u)]
    nodes[, (col("betweenness")) := betweenness(u, weights = NA, normalized = TRUE)]
    nodes[, (col("pagerank")) := if (has_edges) page_rank(g)$vector else NA_real_]
    nodes[, (col("eigenvector")) := if (has_edges) eigen_centrality(u)$vector else NA_real_]
  }

  feedback <- rbindlist(lapply(ml[c("review", "discussion")], as_data_frame, what = "edges"))
  givers <- feedback[, .(n_feedback_givers = uniqueN(from)), by = .(name = to)]
  nodes <- merge(nodes, givers, by = "name", all.x = TRUE, sort = FALSE)
  nodes[is.na(n_feedback_givers), n_feedback_givers := 0L]

  nodes[, n_layers := rowSums(nodes[, paste0(names(ml), "_degree"), with = FALSE] > 0)]
  nodes[]
}

#' Cross-layer degree correlation
#'
#' @param ml A multilayer network from [build_multilayer()].
#' @param method Correlation method passed to [cor()]. Default "spearman".
#' @return Correlation matrix of each contributor's degree in each layer.
layer_degree_correlation <- function(ml, method = "spearman") {
  cor(sapply(ml, n_neighbours), method = method)
}
