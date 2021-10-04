

# screening, simple time lapse and multiple phases
get_cell_lines <- function(object, ...){
  
  check_object(object)
  assign_default(object)
  
  getMetaDf(object) %>%
    dplyr::pull(cell_line) %>%
    base::levels()
  
}

# time lapse
get_frame_time_seq <- function(object){
  
  check_object(object)
  assign_default(object)
  
  getTracksDf(object) %>% 
    dplyr::arrange(frame_num) %>% 
    dplyr::pull(var = "frame_time") %>% 
    base::unique()
  
}

# multiple phases
get_frame_time_seq_mp <- function(object, phase = "all", ...){
  
  check_object(object)
  assign_default(object)
  
  phases <- check_phase(object, phase = phase)
  
  getTracksDf(object, with_grouping = FALSE, phase = phases) %>% 
    dplyr::arrange(frame_num) %>% 
    dplyr::pull(var = "frame_time") %>% 
    base::unique()
  
}

# screening and simple time lapse
get_group_names <- function(object, grouping_variable, ...){
  
  check_object(object)
  
  assign_default(object)
  
  confuns::is_value(grouping_variable, "character")
  
  group_vec <- 
    getGroupingDf(object = object, verbose = FALSE) %>% 
    dplyr::pull(var = {{grouping_variable}}) 
  
  if(base::is.factor(group_vec)){
    
    group_vec <- base::levels(x = group_vec)
    
  } else if(base::is.character(group_vec)){
    
    group_vec <- base::unique(group_vec)
    
  } else {
    
    msg <- glue::glue("The result of grouping variable '{option}' must be a character vector or a factor.")
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop")
    
  }
  
  res <- confuns::vselect(input = group_vec, ...)
  
  base::return(res)
  
}

# multiple phases
get_group_names_mp <- function(object, grouping_variable, ..., phase = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  confuns::is_value(grouping_variable, "character")
  
  group_vec <- 
    getGroupingDf(object = object, phase = phase, verbose = FALSE) %>% 
    dplyr::pull(var = {{grouping_variable}}) 
  
  if(base::is.factor(group_vec)){
    
    group_vec <- base::levels(x = group_vec)
    
  } else if(base::is.character(group_vec)){
    
    group_vec <- base::unique(group_vec)
    
  } else {
    
    msg <- glue::glue("The result of grouping variable '{option}' must be a character vector or a factor.")
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop")
    
  }
  
  res <- confuns::vselect(input = group_vec, ...)
  
  base::return(res)
  
}


# screening, simple time lapse and multiple phases
get_grouping_variable_names <- function(group_df, ..., named = FALSE, verbose = TRUE){
  
  all_var_names <- 
    base::colnames(group_df)
  
  if(base::isTRUE(named)){
    
    sources <- base::vector("character", base::length(all_var_names))
    
    cluster_names <-
      getClusterVariableNames(object, phase = phase, verbose = verbose)
    
    meta_names <- getMetaVariableNames(object, phase = phase)
    
    wp_names <- getWellPlateVariableNames(object)
    
    for(i in base::seq_along(all_var_names)){
      
      var <- all_var_names[i]
      
      if(var %in% cluster_names){
        
        sources[i] <- "cluster"
        
      } else if(var %in% meta_names){
        
        sources[i] <- "meta"
        
      } else if(var %in% wp_names){
        
        sources[i] <- "well_plate"
        
      }
      
    }
    
    base::names(all_var_names) <- sources
    
  }
  
  base::return(all_var_names)
  
}

# screening and simple time lapse
get_missing_values_df <- function(object, ...){
  
  check_object(object)
  
  na_count_df <- object@qcheck$na_count
  
  return(na_count_df)
  
}

# multiple phases
get_missing_values_df_mp <- function(object, phase = NULL,...){
  
  check_object(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  na_count_df <- object@qcheck$na_count[[phase]]
  
  return(na_count_df)
  
}

# time lapse
get_stats_df <- function(object, 
                         drop_na = TRUE,
                         with_grouping = NULL,
                         with_cluster = NULL,
                         with_meta = NULL,
                         with_well_plate = NULL, 
                         verbose = NULL, 
                         ...){
  
  check_object(object)
  assign_default(object)
  
  if(base::isFALSE(with_grouping)){
    
    with_cluster <- FALSE
    with_meta <- FALSE
    with_well_plate <- FALSE
    
  }
  
  df <- 
    getCdata(object) %>% 
    getStatsDf()
  
  # add cluster
  if(base::isTRUE(with_cluster) | base::isTRUE(with_grouping)){
    
    cluster_df <- getClusterDf(object, verbose = FALSE)  
    
    df <- dplyr::left_join(x = df, y = cluster_df, by = "cell_id")
    
  }
  
  # add meta
  if(base::isTRUE(with_meta) | base::isTRUE(with_grouping)){
    
    meta_df <- getMetaDf(object)
    
    df <- dplyr::left_join(x = df, y = meta_df, by = "cell_id")
    
  }
  
  # add well plate info
  if(base::isTRUE(with_well_plate) | base::isTRUE(with_grouping)){
    
    wp_df <- getWellPlateDf(object)
    
    df <- dplyr::left_join(x = df, y = wp_df, by = "cell_id")
    
  }
  
  if(base::isTRUE(drop_na)){
    
    df <- tidyr::drop_na(df)
    
  }
  
  base::return(df)  
  
}

# multiple phases
get_stats_df_mp <- function(object, 
                            drop_na = TRUE,
                            with_grouping = NULL,
                            with_cluster = NULL,
                            with_meta = NULL,
                            with_well_plate = NULL, 
                            phase = NULL, 
                            verbose = NULL, 
                            ...){
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  if(base::isFALSE(with_grouping)){
    
    with_cluster <- FALSE
    with_meta <- FALSE
    with_well_plate <- FALSE
    
  }
  
  df <- 
    getCdata(object) %>% 
    getStatsDf(phase = phase)
  
  # add cluster
  if(base::isTRUE(with_cluster) | base::isTRUE(with_grouping)){
    
    cluster_df <- getClusterDf(object, phase = phase, verbose = FALSE)  
    
    df <- dplyr::left_join(x = df, y = cluster_df, by = "cell_id")
    
  }
  
  # add meta
  if(base::isTRUE(with_meta) | base::isTRUE(with_grouping)){
    
    meta_df <- getMetaDf(object, phase = phase)
    
    df <- dplyr::left_join(x = df, y = meta_df, by = "cell_id")
    
  }
  
  # add well plate info
  if(base::isTRUE(with_well_plate) | base::isTRUE(with_grouping)){
    
    wp_df <- getWellPlateDf(object)
    
    df <- dplyr::left_join(x = df, y = wp_df, by = "cell_id")
    
  }
  
  if(base::isTRUE(drop_na)){
    
    df <- tidyr::drop_na(df)
    
  }
  
  base::return(df)  
}

# time lapse
get_tracks_df <- function(object,
                          with_grouping = NULL, 
                          with_cluster = NULL,
                          with_meta = NULL,
                          with_well_plate = NULL,
                          verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(base::isFALSE(with_grouping)){
    
    with_cluster <- FALSE
    with_meta <- FALSE
    with_well_plate <- FALSE
    
  }
  
  df <- 
    getCdata(object) %>% 
    getTracksDf()
  
  if(base::isTRUE(with_meta) | base::isTRUE(with_grouping)){
    
    meta_df <- getMetaDf(object)
    
    df <- 
      dplyr::left_join(x = df, y = meta_df, by = "cell_id")
    
  }
  
  if((base::isTRUE(with_cluster) | base::isTRUE(with_grouping)) & base::length(phase) == 1){
    
    cluster_df <- getClusterDf(object, verbose = FALSE)
    
    df <- dplyr::left_join(x = df, y = cluster_df, by = "cell_id")
    
  }
  
  if(base::isTRUE(with_well_plate) | base::isTRUE(with_grouping)){
    
    wp_df <- getWellPlateDf(object)
    
    df <- dplyr::left_join(x = df, y = wp_df, by = "cell_id")
    
  }
  
  base::return(df)
  
}

# multiple phases
get_tracks_df_mp <- function(object, 
                             with_grouping = NULL, 
                             with_cluster = NULL,
                             with_meta = NULL,
                             with_well_plate = NULL,
                             phase = "all", 
                             phase_add = TRUE,
                             verbose = NULL){
  
  assign_default(object)
  
  phases <- check_phase(object, phase = phase)
  
  if(base::isFALSE(with_grouping)){
    
    with_cluster <- FALSE
    with_meta <- FALSE
    with_well_plate <- FALSE
    
  }
  
  df <- purrr::map_df(.x = phases, .f = function(p){
    
    track_df <- 
      getCdata(object) %>% 
      getTracksDf(phase = p)
    
    if(base::isTRUE(phase_add)){
      
      track_df <- 
        dplyr::mutate(track_df, phase = {{p}}) %>% 
        dplyr::select(cell_id, phase, dplyr::everything())
      
    }
    
    if(base::isTRUE(with_meta) | base::isTRUE(with_grouping)){
      
      meta_df <- getMetaDf(object, phase = p)
      
      track_df <- dplyr::left_join(x = track_df, y = meta_df, by = "cell_id")
      
    }
    
    if((base::isTRUE(with_cluster) | base::isTRUE(with_grouping))){
      
      cluster_df <- getClusterDf(object, phase = p, verbose = FALSE)
      
      track_df <- dplyr::left_join(x = track_df, y = cluster_df, by = "cell_id")
      
    }
    
    base::return(track_df)
    
  }
  ) 
  
  if(base::isTRUE(with_well_plate) | base::isTRUE(with_grouping)){
    
    wp_df <- getWellPlateDf(object)
    
    df <- dplyr::left_join(x = df, y = wp_df, by = "cell_id")
    
  }
  
  return(df)
  
}

# screening, time lapse and multiple phases
get_well_plate_variable_names <- function(object, ...){
  

  
}



