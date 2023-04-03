#' Add attribute node terminus
#' 
#' Adds a node attribute called `node_terminus` to the nodes of a graph.
#' For tracing organelles, fusion and fission events are interesting to record
#' as well as the start and end position of the organelles. These can be
#' identified based on the degrees of the nodes.
#' 
#' @param graph A tidygraph or igraph object
#' @param check_node_attributes Logical value that determines whether the node 
#' attributes are checked for an existing `node_terminus` attribute.
#' @return The function returns the graph with a new node attribute called 
#' `node_terminus`. It has the values `start` and `end` for the terminal nodes 
#' in a graph, `fusion` and `fission` for merges and splits, and `internal` for
#' linearily connected nodes.
#' @export
add_node_terminus <- function(graph, check_node_attributes = FALSE) {
  
  ## Check input
  
  if (!any(class(graph) %in% c("igraph", "tbl_graph"))) {
    stop("Invalid input! The graph must be of class 'igraph' (igraph package) or 'tbl_graph' (tidygraph package)")
  }
  
  if (!is.logical(check_node_attributes)) {
    stop("Invalid value! The parameter 'check_columns' must be logical.")
  }
  
  ##
  
  if (check_node_attributes) {
    
    graph_columns <-  graph %>% tidygraph::activate(., what = nodes) %>% tibble::as_tibble(.) %>% colnames(.)
    if ("node_terminus" %in% graph_columns) {
      return(graph)
    }
    
  }
  
  graph <- graph %>%
    tidygraph::activate(., what = nodes) %>%
    dplyr::mutate(
      .,
      node_terminus = case_when(
        igraph::degree(graph = graph, mode = "all") == 0 ~ "isolate",
        igraph::degree(graph = graph, mode = "in") == 0 ~ "start",
        igraph::degree(graph = graph, mode = "out") == 0 ~ "end",
        igraph::degree(graph = graph, mode = "all") > 3 ~ "complex",
        igraph::degree(graph = graph, mode = "in") > 1 ~ "fusion",
        igraph::degree(graph = graph, mode = "out") > 1 ~ "fission",
        TRUE ~ "internal"
      )
    ) %>%
    dplyr::mutate(., node_terminus = factor(node_terminus, levels = c("start", "end", "fusion", "fission", "complex", "internal", "isolate")))
  
  return(graph)
}
