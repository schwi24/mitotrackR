#' Bind a list of track graphs together
#' 
#' This is the reverse of the `split_by_tracks` function. It binds a list of
#' graphs together, e.g. to join multiple tracks into one graph. The individual
#' tracks do not need to be connected (share nodes).
#' @param graph_list A list with an graph object in each element. The graphs
#' need to be of class `'igraph'` or `'tidygraph'`.
#' @return A single combined graph of class `'igraph'` and`'tbl_graph'`
#' compatible with the tidygraph package.
#' @export
bind_tracks <- function(graph_list) {
  
  ## Check inputs
  
  if (!("list" %in% class(graph_list))) {
    stop("Invalid input! Please check that 'graph_list' is a list of graph.")
  }
  
  test <- lapply(graph_list, class)
  if (!all(unlist(lapply(test, function(x) {x %in% c("igraph", "tbl_graph")})))) {
    stop("Invalid list element! The elements must be graphs of class 'igraph' or 'tbl_graph' from the igraph or tidygraph packages.")
  }
  if (!all(unlist(lapply(test, function(x) {x %in% "tbl_graph"})))){
    graph_list <- lapply(graph_list, tidygraph::as_tbl_graph)
  }
  
  names(graph_list) <- NULL  #the function tidygraph::bind_graphs does not work with named lists...
  graph_list <- tidygraph::bind_graphs(graph_list)
  return(graph_list)
  
}
