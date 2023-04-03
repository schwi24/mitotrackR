#' Detecting branches
#' 
#' This function detects all the branches of a graph. It takes a graph and returns a list of vectors, one for
#' each branch, containing the node IDs within the branch (including the start
#' and end).
#' @param graph A tidygraph or igraph object.
#' @return The function returns the graph in which the branches have been labeled. The
#' identifier for the branches is stored in a new edge attribute `branch_id`.
#' @export
find_branches <- function(graph){
  
  if (!any(class(graph) %in% c("igraph", "tbl_graph"))) {
    stop("Invalid input! The graph must be of class 'igraph' (igraph package) or 'tbl_graph' (tidygraph package)")
  }
  
  if ("igraph" %in% class(graph)) {
    g <- graph
  } else {
    g <- igraph::as.igraph(graph)
  }
  
  ## BFS
  bfs_res <- igraph::bfs(
    graph = g,
    root = 1L,
    mode = "all",
    order = TRUE,
    rank = FALSE,
    father = TRUE,
    pred = FALSE,
    succ = FALSE,
    dist = FALSE
  )
  
  ## BFS dataframe
  df <- data.frame(
    father = as.vector(bfs_res$father),
    nid = as.vector(igraph::V(g)),
    order = as.vector(bfs_res$order),
    degree = as.vector(igraph::degree(g))
  ) %>%
    mutate(., grp = -1)
  
  ## Father dataframe
  df_father <- data.frame(
    father = as.vector(igraph::V(g)),
    degree_father = as.vector(igraph::degree(g))
  )
  
  ## Join Father's degree, reorder, and fix na
  df <- fast_left_join(left = df, right = df_father, by = c("father"))
  df <- df[order(df$nid), ]
  df[is.na(df$degree_father), "degree_father"] <- 0 ## fix na
  
  ## Groups
  n <- length(which(df$degree_father > 2))  ## number of branchpoints
  m <- length(which(is.na(df$father)))      ## number of roots
  
  df[is.na(df$father), "grp"] <- 0:(m - 1)          ## rooted branches are numbered
  if(n > 0){
    df[df$degree_father>2, "grp"] <- m:(n + m - 1)  ## other branches are numbered
  }
  
  ## Reorder
  df <- df[order(df$nid), ]
  
  ## Loop over Groups in order
  for (i in df$order){
    if(df[i, "grp"]<0){
      df[i, "grp"] <- df$grp[df$father[i]]
    }
  }

  ## Get edge id and update graph with branch
  grp <- df$grp  ## Save for closing loops later
  df <- df[!is.na(df$father),]
  eid <- c(rbind(df$father, df$nid))
  
  g <- igraph::set_edge_attr(
    graph = g,
    name = "branch_id",
    index = igraph::E(graph = g, P = eid, directed = FALSE),
    value = df$grp
  )
  
  ## Close up loop ends (from merges)
  es <- which(is.na(igraph::edge_attr(graph = g, name = "branch_id", index = igraph::E(g))))
  nends <- igraph::ends(
    graph = g,
    es = es,
    names = FALSE
  )
  nends <- nends[, 1]
  g <- igraph::set_edge_attr(
    graph = g,
    name = "branch_id",
    index = es,
    value = grp[nends]
  )
  
  if ("tbl_graph" %in% class(graph)) {
    g <- tidygraph::as_tbl_graph(g)
  }
  
  ## Return
  return(g)
  
}

#' Fast left join for large tables
#' 
#' This function was taken from https://rdrr.io/github/JGCRI/gcamdata/src/R/pipeline-helpers.R
#' 
#' @param left The left-side table to join.  Any class inheriting from
#' `data.frame` is acceptable.
#' @param right The right-side table to join.  Any class inheriting from
#' `data.frame` is acceptable.
#' @param by Character vector of column names to join by.
#' @return The left join of `left` and `right`.  It will be returned
#' as a `tbl_df`, irrespective of the type of the inputs.
#' @importFrom data.table data.table
#' @importFrom assertthat assert_that
#' @importFrom tibble as_tibble
#' @export
fast_left_join <- function(left, right, by) {
  assertthat::assert_that(is.data.frame(left))
  assertthat::assert_that(is.data.frame(right))
  
  ## To key or not to key?  A key is required for the right table, but it is
  ## optional for the left, *provided* that the join columns are in order and
  ## come before the non-join columns.  Keying takes time, but it makes the
  ## join eventually go a little faster.  In the one example we have, it
  ## keying the left table doesn't seem to pay for itself in the join, but
  ## it's possible that depends on the specifics of the input.  For now we
  ## *won't* key, instead opting to reorder the columns of the left table.
  dtl <- data.table::data.table(left[ , union(by, names(left))])
  dtr <- data.table::data.table(right, key=by)
  
  as_tibble(dtr[dtl, allow.cartesian=TRUE])
}