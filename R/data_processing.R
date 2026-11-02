library(data.table)
library(igraph)
set.seed(123)

# Given a data table, returns bipartite network for user to repo relationship
# REQURIES
# user AS character
# repo AS character
build_bipartite_network <- function(dt) {
  # parameter and type check
  if (!data.table::is.data.table(dt)) {
    stop("`dt` must be a data.table.", call. = FALSE)
  }
  
  required <- c(user = "character", repo = "character")
  
  missing <- setdiff(names(required), names(dt))
  if (length(missing)) {
    stop("Missing column(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  
  # handling
  
  # Prefix IDs so a user and a repo can't share a name
  e <- data.table(from = paste0("u:", edges$user),
                  to   = paste0("r:", edges$repo))
  
  nodes <- data.table(
    name = c(unique(e$from), unique(e$to)),
    type = c(rep(FALSE, uniqueN(e$from)), rep(TRUE, uniqueN(e$to)))  # FALSE = user, TRUE = repo
  )
  
  g <- graph_from_data_frame(e, directed = FALSE, vertices = nodes)
  g$partitions <- c("user", "repo")   # type FALSE side, type TRUE side
  g
}

#' Build a one-mode projection of a bipartite network
#'
#' @param bipartite_network A bipartite igraph object with a logical `type`
#'   vertex attribute and a `partitions` graph attribute naming each side.
#' @param target_column The side to project onto; must be one of
#'   `bipartite_network$partitions` (e.g. "user" or "repo").
#' @return An igraph object containing only the target nodes. Two nodes are
#'   connected if they share a neighbour, with edge weight = number of shared neighbours.
build_onemodeprojection_network <- function(bipartite_network, target_column) {
  if (!inherits(bipartite_network, "igraph")) {
    stop("`bipartite_network` must be an igraph object.", call. = FALSE)
  }
  if (!is_bipartite(bipartite_network)) {
    stop("`bipartite_network` is not bipartite (missing logical `type` vertex attribute).", call. = FALSE)
  }
  
  partitions <- bipartite_network$partitions
  if (is.null(partitions)) {
    stop("`bipartite_network` has no `partitions` attribute. Build it with build_bipartite().", call. = FALSE)
  }
  if (!is.character(target_column) || length(target_column) != 1 ||
      !target_column %in% partitions) {
    stop("`target_column` must be one of: ", paste(partitions, collapse = ", "), call. = FALSE)
  }
  
  side <- if (target_column == partitions[1]) "false" else "true"
  bipartite_projection(bipartite_network, which = side, multiplicity = TRUE)
}


#' Build a multilayer contributor network
#'
#' @param proj A contributor one-mode projection (igraph, names prefixed "u:"),
#'   with edge weight = number of shared repos.
#' @param prs Optional data.table with `author` and `closer`; one row per
#'   merged or closed PR. Creates A -> B edges where B closed A's PR.
#' @param reviews Optional data.table with `reviewer` and `author`; one row per
#'   review. Creates A -> B edges where A reviewed B's PR.
#' @param edits Optional data.table with `user`, `file`, and `date`; one row per
#'   file edit. Connects users who edited the same file within `window` days.
#' @param window Maximum days between edits to count as co-editing. Default 90.
#' @return A directed igraph object whose nodes are the contributors in `proj`.
#'   Each edge has a `layer` ("co_repo", "pr", "review", "discussion", "coedit")
#'   and a `weight`. Symmetric layers are stored in both directions. Self-loops,
#'   and edges involving anyone not in `proj`, are dropped.
build_multilayer <- function(proj, prs = NULL, reviews = NULL, edits = NULL, window = 90) {
  nodes <- sub("^u:", "", V(proj)$name)
  
  # Count A -> B interactions, dropping self-loops
  directed_layer <- function(from, to, layer) {
    data.table(from, to)[from != to, .(weight = .N), by = .(from, to)][, layer := layer][]
  }
  # Turn undirected pairs into A -> B and B -> A
  mirror <- function(dt) rbind(dt, dt[, .(from = to, to = from, weight, layer)])
  
  # Co-repo layer, straight from the projection
  co <- as.data.table(as_data_frame(proj, what = "edges"))
  co <- co[, .(from = sub("^u:", "", from), to = sub("^u:", "", to),
               weight, layer = "co_repo")]
  layers <- list(mirror(co))
  
  # 1. PR layer: author -> merger/closer
  if (!is.null(prs)) layers$pr <- directed_layer(prs$author, prs$closer, "pr")
  
  # 2. Review layer: reviewer -> PR author
  if (!is.null(reviews)) layers$review <- directed_layer(reviews$reviewer, reviews$author, "review")
  
  # 3. Co-editing layer: same file within `window` days (weight = shared files)
  if (!is.null(edits)) {
    e <- unique(edits[, .(user, file, date = as.Date(date))])
    e[, `:=`(lo = date - window, hi = date + window)]
    pairs <- e[e, on = .(file, date >= lo, date <= hi),
               .(a = x.user, b = i.user, file), allow.cartesian = TRUE, nomatch = 0]
    coedit <- pairs[a < b, .(weight = uniqueN(file)), by = .(from = a, to = b)][, layer := "coedit"]
    layers$coedit <- mirror(coedit)
  }
  
  el <- rbindlist(layers, use.names = TRUE)
  el <- el[from %in% nodes & to %in% nodes]   # keep only contributors from the projection
  
  graph_from_data_frame(el, directed = TRUE, vertices = data.table(name = nodes))
}



