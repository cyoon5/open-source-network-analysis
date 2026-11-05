library(data.table)
library(igraph)

# Multilayer contributor networks, one per repository.
#
# Nodes are contributors: anyone who opened, reviewed or commented on a PR in
# the repository (one row of contributors.csv). Every repository gets three
# layers over the same node set:
#
#   review      (directed)    A -> B if A reviewed B's PR.   weight = reviews
#   discussion  (directed)    A -> B if A commented on B's PR. weight = comments
#   coedit      (undirected)  A -- B if both changed the same file within
#                             `window` days of each other.   weight = shared files
#
# Input is the output of R/05_process_data.R, which already removes self-reviews,
# comments on your own PR and hot files, and labels each contributor (group,
# is_unusual, merge_rate, ...). Those labels become vertex attributes here, so
# thresholds live only in R/config.R.
#
# Usage (from the project root):
#   source("R/config.R")
#   source("R/network_analysis.R")
#   nets <- build_all_multilayers(
#     contributors = load_csv("contributors.csv"),
#     reviews      = load_csv("cleaned_reviews.csv"),
#     comments     = load_csv("cleaned_comments.csv"),
#     edits        = load_csv("cleaned_edits.csv")
#   )
#   pandas <- nets[["pandas-dev/pandas"]]
#   layer_summary(pandas)
#   node_metrics(pandas)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

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

# ---------------------------------------------------------------------------
# Network construction
# ---------------------------------------------------------------------------

#' Build the three-layer contributor network for one repository
#'
#' @param repo_slug Repository to build, e.g. "pandas-dev/pandas".
#' @param contributors contributors.csv: `repo`, `login`, plus any labels
#'   (`group`, `is_unusual`, `merge_rate`, ...). One row per contributor.
#' @param reviews cleaned_reviews.csv: `repo`, `reviewer`, `pr_author`.
#' @param comments cleaned_comments.csv: `repo`, `commenter`, `pr_author`.
#' @param edits cleaned_edits.csv: `repo`, `author`, `pr_created_at`,
#'   `filename`. The edit date is the PR's creation date.
#' @param window Maximum days between two PRs touching the same file for their
#'   authors to count as co-editors. Default 90.
#' @param drop_groups Values of `contributors$group` to remove from the
#'   network, e.g. "bot". NULL keeps everyone.
#' @return A named list of three igraph objects (`review`, `discussion`,
#'   `coedit`) sharing the same vertices in the same order. Every column of
#'   `contributors` (except `login`, which becomes `name`) is a vertex
#'   attribute. Edge attribute: `weight`. Graph attributes: `repo`, `layer`.
build_multilayer <- function(repo_slug, contributors, reviews, comments, edits,
                             window = 90, drop_groups = NULL) {
  for_repo <- function(dt, required, arg) {
    dt <- as.data.table(dt)
    check_columns(dt, c("repo", required), arg)
    dt[repo == repo_slug]
  }
  nodes    <- for_repo(contributors, "login", "contributors")
  reviews  <- for_repo(reviews, c("reviewer", "pr_author"), "reviews")
  comments <- for_repo(comments, c("commenter", "pr_author"), "comments")
  edits    <- for_repo(edits, c("author", "pr_created_at", "filename"), "edits")
  if (!nrow(nodes)) stop("No contributors found for repo '", repo_slug, "'.", call. = FALSE)

  # Directed layer: actor -> PR author, weight = number of reviews/comments
  feedback_edges <- function(dt, actor) {
    e <- dt[!is.na(get(actor)), .(from = get(actor), to = pr_author)]
    e[tolower(from) != tolower(to), .(weight = .N), by = .(from, to)]
  }

  # Undirected layer: authors whose PRs changed the same file within `window` days
  coedit_edges <- function(dt) {
    e <- unique(dt[, .(user = author, filename, date = as.Date(substr(pr_created_at, 1, 10)))])
    e[, `:=`(lo = date - window, hi = date + window)]
    pairs <- e[e, on = .(filename, date >= lo, date <= hi),
               .(a = x.user, b = i.user, filename), allow.cartesian = TRUE, nomatch = NULL]
    pairs[a < b, .(weight = uniqueN(filename)), by = .(from = a, to = b)]
  }

  edges <- list(review     = feedback_edges(reviews, "reviewer"),
                discussion = feedback_edges(comments, "commenter"),
                coedit     = coedit_edges(edits))

  if (length(drop_groups)) {
    check_columns(nodes, "group", "contributors")
    dropped <- nodes[group %in% drop_groups, login]
    nodes <- nodes[!login %in% dropped]
    edges <- lapply(edges, function(el) el[!from %in% dropped & !to %in% dropped])
  }

  unknown <- setdiff(unlist(lapply(edges, function(el) c(el$from, el$to))), nodes$login)
  if (length(unknown)) {
    stop(length(unknown), " login(s) have edges but no row in `contributors` (e.g. ",
         paste(head(unknown, 3), collapse = ", "), "). Re-run R/05_process_data.R.", call. = FALSE)
  }

  # login first so igraph uses it as the vertex name
  setcolorder(nodes, "login")
  setnames(nodes, "login", "name")

  directed <- c(review = TRUE, discussion = TRUE, coedit = FALSE)
  layers <- lapply(names(edges), function(layer) {
    g <- graph_from_data_frame(edges[[layer]], directed = directed[[layer]], vertices = nodes)
    g$repo <- repo_slug
    g$layer <- layer
    g
  })
  setNames(layers, names(edges))
}

#' Build one multilayer network per repository in `contributors`
#'
#' @inheritParams build_multilayer
#' @param ... Further arguments passed to [build_multilayer()].
#' @return A named list (by repo) of multilayer networks.
build_all_multilayers <- function(contributors, reviews, comments, edits, ...) {
  repos <- unique(contributors$repo)
  setNames(lapply(repos, build_multilayer, contributors = contributors, reviews = reviews,
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
#' Plus `n_layers` (layers the contributor has at least one edge in).
#'
#' @param ml A multilayer network from [build_multilayer()].
#' @return data.table, one row per contributor: the contributors.csv labels
#'   (including `n_distinct_feedback_givers` for RQ3) followed by the metrics.
node_metrics <- function(ml) {
  nodes <- as.data.table(as_data_frame(ml[[1]], what = "vertices"))

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
