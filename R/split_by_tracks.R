#' Split a graph by tracks
#' 
#' This function splits a graph with unconnected tracks obtained from TrackMate
#' into a list of graphs with the individual tracks. This is useful for some 
#' calculations and to further investigate interesting tracks.
#' The function takes a single graph or a node and an edge list as input.
#' The tracks can also be specified optionally.
#' @param graph A graph object of class `'igraph'` or `'tbl_graph'` from the 
#' igraph or tidygraph packages.
#' @param tracks A data.frame or vector with a column `'track_id'` that 
#' identifies the tracks.
#' @param nodes A data.frame or tibble with the node list. It must start with a 
#' column `'node_id'`
#' @param edges A data.frame or tibble with the edge list. It must contain 
#' columns `'source_id'`, `'target_id'` to connect nodes, and a column
#' `'track_id'` to reference the tracks.
#' @return A list of individual graphs for each track.
#' @export
split_by_tracks <- function(graph, tracks, nodes, edges) {
  
  ## Check the inputs
  
  if ((any(missing(nodes), missing(edges))) & missing(graph)) {
    stop("Input missing! Please provide nodes and edges as dataframes or graph")
  }
  
  if (missing(graph)) {
    
    if (!all(is.data.frame(nodes), is.data.frame(edges))) {
      stop("Invalid input! Nodes and edges must be data.frames or tibbles.")
    }
    if (!missing(tracks) & any(!is.data.frame(tracks), !is.vector(tracks))) {
      warning("Tracks are invalid type. Use 'track_id' from edges instead.")
    }
  }
  
  if (all(!missing(nodes), !missing(edges), !missing(graph))) {
    message("Superfluous inputs for nodes and edges. The arguments 'nodes' and 'edges' are ignored and 'graph' is split up instead.")
  }
  
  if (any(missing(nodes), missing(edges))) {
    
    if (missing(graph)) {
      stop("Input missing! Please provide nodes and edges as dataframes or graph")
    }
    
    ## Extract nodes and edges from graph.
    
    if (any(class(graph) %in% c("igraph", "tbl_graph"))) {
      
      nodes <- tidygraph::as_tbl_graph(graph) %>%
        tidygraph::activate(., what = nodes) %>%
        tibble::as_tibble(.) %>%
        dplyr::mutate(., node_id = as.character(node_id))
      
      edges <- tidygraph::as_tbl_graph(graph) %>%
        tidygraph::activate(., what = edges) %>%
        tibble::as_tibble(.) %>%
        dplyr::mutate(
          .,
          edge_id = as.character(edge_id),
          source_id = as.character(source_id),
          target_id = as.character(target_id),
          from = source_id,
          to = target_id,
          track_id = as.character(track_id)
        )
      
    } else {
      stop("graph is wrong type. Please provide as class 'igraph' or 'tbl_graph'.")
    }
    
  }
  
  if (!missing(tracks)) {
    
    ## If no tracks are supplied, take 'track_ids' from edges
    track_ids <- tracks %>% dplyr::select(., track_id) %>% dplyr::pull(.) %>% as.character(.)
    message("Track names (column 'track_id') from argument 'tracks'are used.")
    
  } else {
    
    ## If tracks are supplied, take 'track_ids' from them instead.
    track_ids <- edges %>% dplyr::select(., track_id) %>% dplyr::distinct(.) %>% dplyr::pull(.) %>% as.character(.)
    
  }
  
  ## Select nodes that are connected by edges in tracks
  
  nodes_tracked <- dplyr::bind_rows(
    # source
    dplyr::select(edges, track_id, source_id) %>%
      dplyr::rename(., node_id = source_id),
    # target
    dplyr::select(edges, track_id, target_id) %>%
      dplyr::rename(., node_id = target_id )
  ) %>%
    dplyr::distinct(.) %>%  # Like unique() but faster
    dplyr::left_join(
      nodes,
      .,
      by = "node_id"
    ) %>%
    dplyr::relocate(., track_id, .before = node_frame) %>%
    dplyr::arrange(., track_id) %>%
    dplyr::group_by(., track_id)
  
  ## Create create lists with elements for each track, for both nodes (node_list) and edges (edge_list)
  node_list <- split(nodes_tracked, f = nodes_tracked$track_id)
  edge_list <- split(edges, f = edges$track_id )
  
  ## Test node and edge lists
  if (!all(names(node_list) %in% track_ids)) {
    stop("Not all nodes found for the tracks!")    
  }
  if (!all(names(edge_list) %in% track_ids)) {
    stop("Not all edges found for the tracks!")
  }
  if (!all(names(node_list) %in% names(edge_list))) {
    stop("More tracks for edges than for nodes found!")
  }
  if (!all(names(edge_list) %in% names(node_list))) {
    stop("More tracks for nodes than for edges found!")
  }
  
  ## Put node and edge lists in the same order
  node_list <- node_list[track_ids]
  edge_list <- edge_list[track_ids]
  
  ## Test the reordered node and edge lists
  if (!all(names(node_list[track_ids]) == track_ids)) {
    stop("Not all nodes found for the tracks!")    
  }
  if (!all(names(edge_list[track_ids]) == track_ids)) {
    stop("Not all edges found for the tracks!")
  }
  if (!all(names(node_list) == track_ids)) {
    stop("Not all nodes found for the tracks!")
  }
  if (!all(names(edge_list) == track_ids)) {
    stop("Not all edges found for the tracks!")
  }
  if (!all(names(node_list) == names(edge_list))) {
    stop("Nodes and edges do not match!")
  }
  
  ## Create list and populate with "subgraphs"
  node_terminus_attribute <-  any("node_terminus" %in% colnames(tibble::as_tibble(tidygraph::activate(graph, what = nodes))))
  g_list <- lapply(
    track_ids,
    function(i, nodes, edges){
      g <- tidygraph::tbl_graph(nodes[[i]], edges[[i]], directed = TRUE)
      if (!node_terminus_attribute) {
        g <- add_node_terminus(g, check_node_attributes = FALSE)
      }
      return(g)
    },
    nodes = node_list,
    edges = edge_list
  )
  
  ## Name list elements by tracks
  names(g_list) <- track_ids
  
  ## Return
  return(g_list)
  
}
