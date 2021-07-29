#' Read TrackMate save files (xml)
#' 
#' Read xml files saved with TrackMate.
#' 
#' @param filepath Path to the xml file.
#' @param import Import options to select the retrieved data.
#' `'tracks'` will return a tibble with the tracks statistics.
#' `'nodes'` will return the nodeslist in a tibble.
#' `'edges'` will return the edgeslist in a tibble.
#' `'settings'` will return a list of settings from TrackMate.
#' `'graph'` will return an graph object.
#' `'full'` will return a list containing the settings, tracks, nodelist, and
#' edgelist.
#' @return The value is a tibble with tracks, nodes (node list), or edges 
#' (edge list) if the respective import option was selected.
#' The import option `'settings'` will return a list with the TrackMate
#' settings.
#' For full import, the function returns a list with tibbles for the tracks,
#' nodes, and edges, and with a list with the TrackMate settings.
#' If the graph was selected, a graph of classes `'tbl_graph'` and `'igraph'` 
#' will be returned. It is compatible with the `igraph` and `tidygraph`
#' packages and can be plotted with the `ggraph` package.
#' @export
read_TrackMate <- function(
  filepath,
  import = c("tracks", "nodes", "edges", "settings", "graph", "full")
  ) {
  
  ## Check file
  if(!endsWith(filepath, ".xml")) {
    stop("Cannot load file! Please check the TrackMate xml file and filepath")
  }
  
  ## Check import option
  if(!any(c("tracks", "nodes", "edges", "settings", "graph", "full") %in% import)){
    stop("Invalid import! Choose section from c('tracks', 'nodes', 'edges', 'settings', 'graph', 'full').")
  }
  if(length(import) > 1){
    stop("Invalid import! Choose section from c('tracks', 'nodes', 'edges', 'settings', 'graph', 'full').")
  }
  
  ## Try to parse .xml file
  doc <- try(XML::xmlParse(filepath), silent = TRUE)
  if(any(class(doc) %in% "try-error")) {
    stop("Cannot laod file! Please check the TrackMate xml file and filepath.")
  }
  
  ## Nodes (called spots in TrackMate)
  
  if(any(c("nodes", "graph", "full") %in% import)) {
    
    ns <- XML::getNodeSet(doc, '//Spot')
    nodes <- t(XML::xmlSApply(ns, XML::xmlAttrs)) %>%
      tibble::as_tibble(.) %>%
      dplyr::rename(
        .,
        node_id = ID, # the node
        quality = QUALITY, # detection quality
        node_t = POSITION_T, # time
        intensity_max = MAX_INTENSITY, # intensity maximum
        node_frame = FRAME, # the image frame
        intensity_med = MEDIAN_INTENSITY,  # intensity median
        display_visibility = VISIBILITY, # display visibility
        intensity_avg = MEAN_INTENSITY, # intensity mean
        intensity_total = TOTAL_INTENSITY, # intensity total
        diameter_estimate = ESTIMATED_DIAMETER, # diameter estimate
        radius = RADIUS, # radius
        snr = SNR, # detection signal to noise ratio
        node_x = POSITION_X, # position x
        node_y = POSITION_Y, # position y
        intensity_stdev = STANDARD_DEVIATION, # intensity standard deviation
        contrast = CONTRAST, # detection contratst
        display_color = MANUAL_COLOR, # display manual color
        intensity_min = MIN_INTENSITY, # intensity minimum
        node_z = POSITION_Z, # position z
      ) %>%
      dplyr::mutate(
        .,
        node_frame = as.integer(node_frame), # location
        node_t = as.double(node_t), 
        node_x = as.double(node_x),
        node_y = as.double(node_y),
        node_z = as.double(node_z), 
        radius = as.double(radius), # size
        diameter_estimate = as.double(diameter_estimate), 
        intensity_total = as.double(intensity_total), # intensity
        intensity_avg = as.double(intensity_avg),
        intensity_med = as.double(intensity_med),
        intensity_min = as.double(intensity_min),
        intensity_max = as.double(intensity_max),
        intensity_stdev = as.double(intensity_stdev),
        quality = as.double(quality), # detection quality
        contrast = as.double(contrast),
        snr = as.double(snr),
        display_visibility = as.integer(display_visibility),
        display_color = as.integer(display_color)
      ) %>%
      dplyr::relocate(
        .,
        node_id, name, node_frame, node_t, node_x, node_y, node_z, # location
        radius, diameter_estimate, # size
        intensity_total, intensity_avg, intensity_med, intensity_min, intensity_max, intensity_stdev, # intensity
        quality, contrast, snr # detection quality
      ) %>%
      dplyr::select(., -display_visibility, -display_color, -name)  # drop display options, and superfluous spot name
    
  }
 
  ## Edges (called edges in TrackMate)
  
  if(any(c("edges", "graph", "full") %in% import)){
    
    ns <- XML::getNodeSet(doc, '//Track')
    edges <- data.table::rbindlist(
      lapply(
        ns,
        function(x) {
          TRACK_ID <- XML::xmlGetAttr(x, "TRACK_ID")
          data.frame(TRACK_ID, t(XML::xpathSApply(x, ".//Edge", XML::xmlAttrs)))
        }
      ),
      fill=TRUE
    ) %>%
      tibble::as_tibble(.) %>%
      tibble::rowid_to_column(var = "edge_id", .) %>% # id for edges
      dplyr::rename(
        .,
        track_id = TRACK_ID, # id for tracks
        source_id = SPOT_SOURCE_ID, # id for source node
        target_id = SPOT_TARGET_ID, # id for target node
        link_cost = LINK_COST, # edge weight
        edge_t = EDGE_TIME, # time (between source and target)
        edge_x = EDGE_X_LOCATION, # position x (between source and target)
        edge_y = EDGE_Y_LOCATION, # position y (between source and target)
        edge_z = EDGE_Z_LOCATION, # position z (between source and target)
        tm_velocity = VELOCITY, # incremental velocity calcuated by TrackMate
        tm_displacement = DISPLACEMENT # incremental displacement (step) calculated by TrackMate
      ) %>%
      dplyr::mutate(
        .,
        edge_id = as.character(edge_id),
        from = as.character(source_id),
        to = as.character(target_id),
        link_cost = as.double(link_cost),
        edge_t = as.double(edge_t),
        edge_x = as.double(edge_x),
        edge_y = as.double(edge_y),
        edge_z = as.double(edge_z),
        tm_velocity = as.double(tm_velocity),
        tm_displacement = as.double(tm_displacement)
      ) %>%
      dplyr::relocate(
        .,
        edge_id, from, to, link_cost, # edge list basics
        track_id, source_id, target_id, # the subgraph
        edge_t, edge_x, edge_y, edge_z, # locations
        tm_velocity, tm_displacement # increments calculated by TrackMate
      )
    
  }
  
  ## Tracks (a collection of disconnected graphs)
  
  if(any(c("tracks", "graph", "full") %in% import)){
    
    ns <- XML::getNodeSet(doc, '//Track')
    tracks <- t(XML::xmlSApply(ns, XML::xmlAttrs)) %>%
      tibble::as_tibble(.) %>%
      dplyr::rename(
        .,
        track_id = TRACK_ID, # id for tracks
        number_spots = NUMBER_SPOTS,
        number_gaps = NUMBER_GAPS,
        longest_gap = LONGEST_GAP,
        number_splits = NUMBER_SPLITS,
        number_merges = NUMBER_MERGES,
        number_complex = NUMBER_COMPLEX,
        track_duration = TRACK_DURATION, # duration
        track_start_time = TRACK_START,
        track_stop_time = TRACK_STOP,
        track_displacement = TRACK_DISPLACEMENT, # sum of steps
        track_index = TRACK_INDEX, # same as track_id
        track_x = TRACK_X_LOCATION, # location
        track_y = TRACK_Y_LOCATION,
        track_z = TRACK_Z_LOCATION,
        tm_track_velocity_avg = TRACK_MEAN_SPEED, # velocity
        tm_track_velocity_max = TRACK_MAX_SPEED,
        tm_track_velocity_min = TRACK_MIN_SPEED,
        tm_track_velocity_med = TRACK_MEDIAN_SPEED,
        tm_track_velocity_stdev = TRACK_STD_SPEED,
        tm_track_quality_avg = TRACK_MEAN_QUALITY, # quality
        tm_track_quality_max = TRACK_MAX_QUALITY,
        tm_track_quality_min = TRACK_MIN_QUALITY,
        tm_track_quality_med = TRACK_MEDIAN_QUALITY,
        tm_track_quality_stdev = TRACK_STD_QUALITY,
      ) %>%
      dplyr::mutate(
        .,
        track_id = as.character(track_id),
        name = as.character(name)
      ) %>%
      dplyr::mutate_at(
        .,
        dplyr::vars(-track_id, -name),
        as.double
      ) %>%
      dplyr::select(., -name, -track_index)  # drop display options, and superfluous spot name
    
  }
  
  ## Settings
  
  if(any(c("settings", "full") %in% import)){
    
    ns <- XML::getNodeSet(doc, "//Settings/ImageData")
    settings_imagedata <- t(XML::xmlSApply(ns, XML::xmlAttrs))
    
    ns <- XML::getNodeSet(doc, "//Settings/BasicSettings")
    settings_basic <- t(XML::xmlSApply(ns, XML::xmlAttrs))
    
    ns <- XML::getNodeSet(doc, "//Settings/DetectorSettings")
    settings_detector <- t(XML::xmlSApply(ns, XML::xmlAttrs))
    
    ns <- XML::getNodeSet(doc, "//Settings/InitialSpotFilter")
    settings_initial_spotfilter <- t(XML::xmlSApply(ns, XML::xmlAttrs))
    
    ns <- XML::getNodeSet(doc, "//Settings/SpotFilterCollection")
    settings_spotfilter_collection <- t(XML::xmlSApply(ns, XML::xmlAttrs))
    
    ns <- XML::getNodeSet(doc, "//Settings/TrackFilterCollection")
    settings_trackfilter_collection <- t(XML::xmlSApply(ns, XML::xmlAttrs))
    
    ##Tracker Settings
    ns <- XML::getNodeSet(doc, "//Settings/TrackerSettings/Linking")
    settings_tracker_linking <- t(XML::xmlSApply(ns, XML::xmlAttrs))
    
    ns <- XML::getNodeSet(doc, "//Settings/TrackerSettings/GapClosing")
    settings_tracker_gapclosing <- t(XML::xmlSApply(ns, XML::xmlAttrs))
    
    ns <- XML::getNodeSet(doc, "//Settings/TrackerSettings/TrackSplitting")
    settings_tracker_tracksplitting <- t(XML::xmlSApply(ns, XML::xmlAttrs))
    
    ns <- XML::getNodeSet(doc, "//Settings/TrackerSettings/TrackMerging")
    settings_tracker_trackmerging <- t(XML::xmlSApply(ns, XML::xmlAttrs))
  }
  
  ## Graph
  
  if(import == "graph"){
    
    if (
        !(
          all(
            (edges$from %in% nodes$node_id), 
            all(edges$to %in% nodes$node_id)
          )
        )
    ) {
      stop("Not all nodes in the edge list are found! Please check the TrackMate export.")
    }
      
    routes <- try(
      tidygraph::tbl_graph(
        nodes = nodes,
        edges = edges,
        directed = TRUE
      )
    )
    if(any(class(routes) %in% "try-error")) {
      stop("Cannot create graph! Please check the TrackMate export.")
    }
    
    ## Annotate nodes for start, end, fusion, and fission
    routes <- routes %>% tidygraph::activate(nodes) %>%
      dplyr::mutate(., node_terminus = NA) %>%
      dplyr::mutate(., node_terminus = replace(node_terminus, igraph::degree(., mode = "in") == 0, "start")) %>%
      dplyr::mutate(., node_terminus = replace(node_terminus, igraph::degree(., mode = "out") == 0, "end")) %>%
      dplyr::mutate(., node_terminus = replace(node_terminus, igraph::degree(., mode = "in") > 1, "fusion")) %>%
      dplyr::mutate(., node_terminus = replace(node_terminus, igraph::degree(., mode = "out") > 1, "fission")) %>%
      dplyr::mutate(., node_terminus = as.factor(node_terminus)) %>%
      dplyr::mutate(., node_terminus = forcats::fct_relevel(node_terminus, "start", "end", "fusion", "fission"))
  
  }
  
  ## Return
  
  if(import == "tracks") {
    return(tracks)
  } else if(import == "nodes") {
    return(nodes)
  } else if(import == "edges") {
    return(edges)
  } else if(import == "settings") {
    return("Work in progress...")
    # [construction_site] missing settings
  } else if(import == "full") {
    # [construction_site] missing settings
    full <- list(tracks, nodes, edges)
    names(full) <- c("tracks", "nodes", "edges")
    return(full)
  } else if(import == "graph") {
    return(routes)
  }
}

