# NOT Exported ------------------------------------------------------------

# cdata set up

set_up_cdata_meta <- function(object, verbose = TRUE){
  
  if(multiplePhases(object)){
    
    all_phases <- getPhases(object)
    
    # PATCH: Propagate well plate info to tracks before building meta ----
    wp_name <- names(object@well_plates)[1]
    wp <- object@well_plates[[wp_name]][["wp_df_eval"]]
    for (phase_i in seq_along(all_phases)) {
      phase <- all_phases[phase_i]
      tracks <- object@cdata[["tracks"]][[phase]]
      # Extract well from cell_id (after _WROI_, e.g. A5, B2)
      tracks$well <- stringr::str_extract(tracks$cell_id, "(?<=_WROI_)[A-H][0-9]+")
      # For each well, parse out the Nth phase's condition from the full condition string
      wp_tmp <- wp %>%
        dplyr::select(well, cell_line, condition) %>%
        dplyr::mutate(
          phase_condition = stringr::str_trim(
            stringr::str_split_fixed(condition, "->", length(all_phases))[, phase_i]
          ),
          phase_condition = stringr::str_remove(phase_condition, "^\\d+\\.")  # remove "1."/"2."
        )
      tracks <- dplyr::left_join(
        tracks,
        wp_tmp %>% dplyr::select(well, cell_line, phase_condition),
        by = "well"
      ) %>%
        dplyr::mutate(
          cell_line = as.factor(cell_line),
          condition = as.factor(phase_condition)
        ) %>%
        dplyr::select(-phase_condition)
      tracks$well <- NULL
      object@cdata[["tracks"]][[phase]] <- tracks
    }
    # END PATCH ---------------------------------------------------------
    
    object@cdata$meta <- 
      purrr::map(.x = all_phases,
                 .f = function(phase){
                   object@cdata[["tracks"]][[phase]] %>% 
                     dplyr::select(
                       cell_id, cell_line, condition
                     ) %>% 
                     dplyr::distinct() %>% 
                     dplyr::mutate(
                       cell_line = base::as.factor(cell_line), 
                       condition = base::as.factor(condition)
                     )
                 }) %>% 
      purrr::set_names(nm = all_phases)
    
  } else {
    
    if(!isTimeLapseExp(object)){
      grouping_variables <- 
        object@information$variable_denotation$additional$grouping_variables
    } else {
      grouping_variables <- character(0)
    }
    
    wp_name <- names(object@well_plates)[1]
    wp <- object@well_plates[[wp_name]][["wp_df_eval"]]
    tracks <- object@cdata[["tracks"]][[1]]
    tracks$well <- stringr::str_extract(tracks$cell_id, "(?<=_WROI_)[A-H][0-9]+")
    tracks <- dplyr::left_join(
      tracks,
      wp %>% dplyr::select(well, condition, cell_line, dplyr::any_of(grouping_variables)),
      by = "well"
    ) %>%
      dplyr::mutate(
        cell_line = as.factor(cell_line),
        condition = as.factor(condition)
      )
    tracks$well <- NULL
    object@cdata[["tracks"]][[1]] <- tracks
    
    object@cdata$meta <-
      tracks %>%
      dplyr::select(
        cell_id, cell_line, condition,
        dplyr::any_of(grouping_variables)
      ) %>%
      dplyr::distinct()
    
  }
  
  base::return(object)
  
}

set_up_cdata_cluster <- function(object, verbose){
  
  
  if(multiplePhases(object)){
    
    all_phases <- getPhases(object)
    
    object@cdata$cluster <- 
      purrr::map(.x = all_phases,
                 .f = function(phase){
                   
                   dplyr::select(object@cdata[["tracks"]][[phase]], cell_id) %>% 
                     dplyr::distinct()
                   
                   
                 }) %>% 
      purrr::set_names(nm = all_phases)
    
  } else {
    
    object@cdata$cluster <- 
      dplyr::select(object@cdata[["tracks"]][[1]], cell_id) %>% 
      dplyr::distinct()
    
  }
  
  base::return(object)
  
}

set_up_cdata_tracks <- function(object, verbose = TRUE){
  
  confuns::give_feedback(
    msg = "--------------- Setting up cell track data.",
    verbose = verbose
    )
  
  if(isTimeLapseExp(object)){
    
    if(multiplePhases(object)){
      
      object@cdata$tracks <- 
        purrr::map2(
          .x = object@cdata$tracks,
          .y = getPhases(object),
          object = object,
          verbose = verbose,
          .f = complete_tracks
        ) %>% 
        purrr::set_names(nm = getPhases(object))
      
    } else {
      
      object@cdata$tracks <- 
        purrr::map_df(
          .x = object@cdata$tracks,
          phase = "first",
          object = object,
          verbose = verbose,
          .f = complete_tracks
        )
      
    }
    
  } else {
    
    object@cdata$tracks <- 
      object@cdata$tracks$only %>% 
      dplyr::select(cell_id, where(base::is.numeric))
    
    confuns::give_feedback(
      msg = "----- Computing analysis module related variables.",
      verbose = verbose
    )
    
    object@cdata$tracks <-
      compute_module_variables(
        track_df = object@cdata$tracks,
        object = object,
        verbose = verbose
      ) %>% 
      dplyr::ungroup()
    
  }
  
  return(object)
}

#' @export
set_up_cdata_stats <- function(object, summarize_with, verbose = TRUE){
  
  confuns::give_feedback(
    msg = "--------------- Setting up cell stat data.",
    verbose = verbose
    )
  
  confuns::check_one_of(
    input = summarize_with, 
    against = base::names(stat_funs)
  )
  
  # set up empty stat data.frame(s) with only cell ids (fill with left_join() later on)
  if(multiplePhases(object)){
    
    object@cdata$stats <- 
      purrr::map2(
        .x = object@cdata$tracks,
        .y = getPhases(object),
        .f = complete_stats, 
        object = object, 
        summarize_with = summarize_with, 
        verbose = verbose
        )
    
  } else {
    
    object@cdata$stats <- 
      complete_stats(
        track_df = object@cdata$tracks,
        summarize_with = summarize_with,
        phase = NULL, 
        verbose = verbose, 
        object = object
        )
    
    
  }
  
  return(object)
  
}

set_up_cdata_tracks_and_stats <- function(object, verbose = TRUE){
  
  confuns::give_feedback(msg = "Creating track- & stat data.", verbose = verbose)
  
  # if time lapse experiment way of processing depends on phase set up
  if(isTimeLapseExp(object)){
    
    if(multiplePhases(object)){
      # multiple phases => named list of data.frames
      
      # process tracks
      object@cdata$tracks <- 
        purrr::map2(
          .x = object@cdata$tracks,
          .y = getPhases(object),
          object = object,
          verbose = verbose,
          .f = hlpr_process_tracks
        ) %>% 
        purrr::set_names(nm = getPhases(object))
      
      # compute statistics 
      object@cdata$stats <- 
        purrr::map2(
          .x = object@cdata$tracks,
          .y = getPhases(object),
          object = object,
          verbose = verbose,
          .f = compute_cell_stats
        ) %>% 
        purrr::set_names(nm = getPhases(object))
      
    } else {
      
      # one phase => single data.frame 
      
      # process tracks
      object@cdata$tracks <- 
        purrr::map_df(
          .x = object@cdata$tracks,
          phase = NULL,
          object = object,
          verbose = verbose,
          .f = hlpr_process_tracks
        )
      
      # compute statistics 
      object@cdata$stats <- 
        compute_cell_stats(
          df = object@cdata$tracks,
          phase = NULL,
          verbose = verbose, 
          object = object
        )
      
    }
    
    # if not time lapse only one way (not much processing necessary)
  } else {
    
    # data stored in slot stats as list of one slot "only"
    df <- 
      object@cdata$stats$only %>% 
      dplyr::select(cell_id, where(base::is.numeric))
    
    # convert to data.frame
    object@cdata$stats <- df
    
    cnames <- base::colnames(df)
    
    # if available shift cell location info to track data 
    if(base::all(c("x_coords", "y_coords") %in% cnames)){
      
      object@cdata$tracks <- 
        dplyr::select(df, cell_id, x_coords, y_coords) %>% 
        dplyr::mutate(frame_num = 1)
      
    } else {
      
      object@cdata$tracks <- data.frame()
      
    }
    
    if("x_coords" %in% cnames){
      
      object@cdata$stats$x_coords <- NULL
      
    }
    
    if("y_coords" %in% cnames){
      
      object@cdata$stats$y_coords <- NULL
      
    }
    
    
  }
  
  base::return(object)
  
}

# wpdata 
set_up_cdata_well_plate <- function(object, verbose = TRUE){
  
  confuns::give_feedback(msg = "Setting up cell well plate data.", verbose = verbose)
  
  object@cdata$well_plate <- 
    dplyr::select(object@cdata[["tracks"]][[1]], dplyr::all_of(x = c("cell_id", well_plate_vars))) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(
      dplyr::across(.cols = dplyr::starts_with("well"), .fns = base::as.factor)
    )
  
  base::return(object)
  
}

# vdata set up 

#' @title Computes variable summaries
#' 
#' @inherit argument_dummy  
#'
#' @inherit updated_object return
#' @export
#'
set_up_vdata <- function(object, verbose = TRUE){
  
  confuns::give_feedback(
    msg = "--------------- Setting up variable data.",
    verbose = verbose
  )
  
  vdata <- list()
  
  if(multiplePhases(object)){
    
    vdata$summary <- 
      purrr::map(.x = getPhases(object), 
                 .f = function(p){
                   
                   msg <- 
                     glue::glue(
                       "Computing variable statistics and summary of {p} phase."
                       )
                   
                   confuns::give_feedback(msg = msg, verbose = verbose)
                   
                   stats_mtr <- 
                    getStatsDf(object, phase = p, with_grouping = FALSE) %>% 
                     dplyr::select(where(base::is.numeric)) %>% 
                     base::as.matrix()
                   
                   vdf <- 
                     base::suppressWarnings({
                       
                       psych::describe(stats_mtr, IQR = TRUE)
                       
                     }) %>% 
                     base::as.data.frame() %>% 
                     tibble::rownames_to_column(var = "variable") %>% 
                     tibble::as_tibble() 
                   
                   confuns::give_feedback(msg = "Counting NAs by variable.", verbose = verbose)
                   
                   vdf$count_na <- 
                     getStatsDf(object, phase = p) %>% 
                     dplyr::select_if(base::is.numeric) %>% 
                     purrr::map_int(.f = ~ base::is.na(.x) %>% base::sum())
                   
                   base::return(vdf)
                   
                 }) %>% 
      purrr::set_names(nm = getPhases(object))
    
  } else {
    
    msg <- "Computing variable statistics and summary."
    
    confuns::give_feedback(msg = msg, verbose = verbose)
    
    stats_mtr <- 
      getStatsDf(object, phase = NULL, with_grouping = FALSE) %>% 
      dplyr::select_if(.predicate = base::is.numeric) %>% 
      base::as.matrix() 
    
    vdata$summary <- 
      
      base::suppressWarnings({
        
        psych::describe(stats_mtr, IQR = TRUE)
        
      }) %>% 
      base::as.data.frame() %>% 
      tibble::rownames_to_column(var = "variable") %>% 
      tibble::as_tibble() 
    
    confuns::give_feedback(msg = "Counting NAs by variable.", verbose = verbose)
    
    vdata$summary$count_na <- 
      getStatsDf(object, with_grouping = FALSE) %>% 
      dplyr::select_if(base::is.numeric) %>% 
      purrr::map_int(.f = ~ base::is.na(.x) %>% base::sum())
    
  }
  
  object@vdata <- vdata
  
  base::return(object)
  
}



# EXPORTED ----------------------------------------------------------------

#' @title Create data.frame representing a well plate
#' 
#' @description Sets up a data.frame in which each observation refers
#' to a well.
#'
#' @param type Character value. One of \emph{'2x3 (6)', '3x4 (12)', '4x6 (24)', '6x8 (48)', '8x12 (96)'}
#'
#' @export
#'

setUpWellPlateDf <- function(type = "8x12 (96)", phases = NULL){
  
  # row- and column number of current well plate
  well_plate_used <- 
    dplyr::filter(well_plate_info, type == {{type}})
  
  # data.frame (obs => well)
  well_plate_df_new <- 
    tidyr::expand_grid(row_num = 1:well_plate_used$rows, 
                       col_num = 1:well_plate_used$cols) %>%
    dplyr::group_by(row_num, col_num) %>% 
    dplyr::mutate(
      row_letter = base::LETTERS[row_num],
      well = stringr::str_c(row_letter, col_num, sep = ""), 
      group = "well_plate",
      information_status = base::factor(x = "Missing",
                                        levels = c("Complete", "Incomplete", "Missing")),
      cell_line = "unknown",
      condition = "unknown", 
      cl_condition = "unknown & unknown", 
      type = {{type}}
    )
  
  if(!base::is.null(phases)){
    
    phases_names <- 
      english::ordinal(x = base::seq_along(phases)) %>%
      confuns::make_capital_letters(collapse.with = NULL) %>% 
      stringr::str_c(., "Phase:", sep = " ")
    
    well_plate_df_new$condition_df <- 
      purrr::map(.x = base::seq_along(well_plate_df_new$well), 
                 .f = function(x){
                   
                   base::matrix(ncol = base::length(phases_names), nrow = 1) %>% 
                   base::as.data.frame() %>% 
                   magrittr::set_colnames(value = phases_names)
                   
                 })
    
  }
  
  base::return(well_plate_df_new)
  
}
