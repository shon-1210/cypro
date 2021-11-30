
#' Completes track data.frame:
#' 
#' - inserts NA observations for every cell id/frame combination that is missing 
#' - adds frame_itvl and frame_time variables
#' - computes all track variables that derive from modules
#' 
#' outputs a data.frame that contains variables cell id, frame variables and 
#' all additional numeric variables found 
#' 
#' to be used as input for .f in purrr::map()

complete_tracks <- function(df, phase, object, verbose){
  
  # ensure that phase is default with as a character
  phase <- check_phase(object, phase = phase)
  
  msg <- 
    glue::glue(
      "---------- Completing track data.frame{ref_phase}.",
      ref_phase = hlpr_glue_phase(object, phase, FALSE, "of")
    )
  
  confuns::give_feedback(msg = msg, verbose = verbose)
  
  itvl <- object@set_up$itvl
  itvl_u <- object@set_up$itvl_u
  exp_type <- object@set_up$experiment_type
  
  all_cell_ids <- object@information$all_cell_ids
  
  frame_range <- base::range(df$frame_num)
  
  all_frames <- frame_range[1]:frame_range[2]
  
  df <- dplyr::ungroup(df)
  
  # split df to avoid NAs in grouping variables
  group_df <- 
    dplyr::select(df, cell_id, dplyr::starts_with("well"), where(base::is.factor))
  
  numeric_df <-
    dplyr::select(df, cell_id, where(base::is.numeric)) %>% 
    dplyr::mutate(frame_added = FALSE)
  
  complete_num_df <- 
    tidyr::expand_grid(cell_id = {{all_cell_ids}}, frame_num = {{all_frames}}) %>% 
    dplyr::left_join(x = ., y = numeric_df, by = c("cell_id", "frame_num"))
  
  complete_df <- 
    dplyr::left_join(x = complete_num_df, y = group_df, by = "cell_id") %>% 
    dplyr::distinct() %>% # temporary solution to weird multiplying of observations
    dplyr::arrange(cell_id) %>% 
    dplyr::select(cell_id, frame_added, where(base::is.numeric)) %>%  # discard non numeric variables
    dplyr::mutate(
      frame_added = tidyr::replace_na(frame_added, replace = TRUE)
    )
    
  # allow computation of later phases to refer to the last position of the previous phase
  # ignored if only one phase exist as phase in this case can only be "first"
  if(phase != "first"){
    
    prev_phase <- base::which(getPhases(object) == phase) - 1
    
    prev_track_df <- 
      object@cdata$tracks[[prev_phase]] %>% 
      dplyr::group_by(cell_id) %>% 
      dplyr::slice_max(frame_num) %>% 
      dplyr::select(cell_id, where(base::is.numeric))
    
    complete_df <- 
      dplyr::bind_rows(complete_df, prev_track_df) %>% 
      dplyr::group_by(cell_id) %>% 
      dplyr::arrange(frame_num, .by_group = TRUE)
    
    prev_last_frame <- base::max(prev_track_df$frame_num)
    
  }
  
  track_df <- 
    dplyr::mutate(
      .data = complete_df,
      frame_time = frame_num * itvl,
      frame_itvl = stringr::str_c(frame_time, itvl_u, sep = " ")
    ) %>% 
    # in case of multiple phases this only affects the data.frame of 
    # the last phase as previous phases have lower max(frame_num)
    dplyr::filter(frame_num <= object@set_up$nom)
  
  confuns::give_feedback(
    msg = "----- Computing analysis module related variables.",
    verbose = verbose
  )
  
  track_df <-
    compute_module_variables(
      track_df = track_df,
      object = object,
      verbose = verbose, 
      phase = phase
    )
  
  # delete the added frame row
  if(phase != "first"){
    
    track_df <-
      dplyr::filter(track_df, frame_num > prev_last_frame)
    
  }
  
  track_df_numeric <- 
    dplyr::select(track_df, cell_id, dplyr::starts_with("frame"), where(base::is.numeric)) %>% 
    dplyr::ungroup()
  
  base::return(track_df_numeric)
  
} 


complete_stats <- function(track_df, phase, object, summarize_with, verbose){
  
  msg <- 
    glue::glue(
      "---------- Completing stat data.frame{ref_phase}.",
      ref_phase = hlpr_glue_phase(object, phase, FALSE, "of")
    )
  
  confuns::give_feedback(msg = msg, verbose = verbose)
  
  confuns::give_feedback(
    msg = "----- Summarizing all track variables.",
    verbose = verbose
    )
  
  stat_df <- 
    dplyr::group_by(track_df, cell_id) %>% 
    dplyr::select(-dplyr::any_of(x = non_data_track_variables)) %>% 
    dplyr::summarize(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = stat_funs[summarize_with],
        .names = "{.fn}_{.col}"
      )
    )
  
  confuns::give_feedback(
    msg = "----- Summarizing module related variables.",
    verbose = verbose
    )
  
  stat_df <- 
    summarize_module_variables(
      stat_df = stat_df, 
      track_df = track_df, 
      object = object, 
      verbose = verbose
    )
  
  return(stat_df)
  
}





#' @title Complete tracks data.frame
#' 
#' @description Completes the track data.frame by adding missing \emph{cell_id} +
#' \emph{frame} observations.
#'
#' @inherit argument_dummy params
#'
#' @return The input object. 
#' @export
setGeneric(name = "completeTracksDf", def = function(object, verbose = TRUE){
  
  standardGeneric(f = "completeTracksDf")
  
})

#' @rdname completeTracksDf
#' @export
setMethod(
  f = "completeTracksDf",
  signature = "CyproTimeLapse",
  definition = function(object, verbose = TRUE){
    
    give_feedback(msg = "Completing tracks data.frame by observation.", verbose = verbose)
    
    itvl <- getInterval(object)
    
    df <- getTracksDf(object, with_well_plate = TRUE)
    
    tracks_df <- 
      complete_tracks_hlpr(
        object = object,
        tracks_df = tracks_df, 
        all_frames = getFrames(object)
        )
    
    object <- setTracksDf(object, df = tracks_df)
    
    return(object)
    
  })


#' @rdname completeTracksDf
#' @export
setMethod(
  f = "completeTracksDf",
  signature = "CyproTimeLapseMP",
  definition = function(object, verbose = TRUE){
    
    give_feedback(
      msg = glue::glue("Completing tracks data.frames of {nPhases(object)} phases by observation."),
      verbose = verbose
      )
    
    phases <- getPhases(object)
    
    for(p in phases){
      
      give_feedback(
        msg = glue::glue("Working on phase {p}."), 
        verbose = verbose
      )
      
      tracks_df <- 
        getTracksDf(object, phase = p, with_well_plate = TRUE)
      
      all_frames <- getFrames(object, by_phase = TRUE)[[p]]
      
      if(p != 1){
        
        all_frames <- c(base::min(all_frames)-1, all_frames)
        
      }
      
      tracks_df_complete <- 
        complete_tracks_hlpr(
          object = object,
          tracks_df = tracks_df,
          all_frames = all_frames
          )
      
      object <- 
        setTracksDf(object, df = tracks_df_complete, phase = p)
      
    }
    
    return(object)
    
  })

complete_tracks_hlpr <- function(object, tracks_df, all_frames){ # tracks_df must contain well plate vars
  
  itvl <- getInterval(object)
  
  df <- dplyr::mutate(tracks_df, frame_added = FALSE)
  
  all_cell_ids <- base::unique(df$cell_id)
  
  id_df <- 
    dplyr::select(df, cell_id, well_plate_name, well_plate_index, well, roi) %>% 
    dplyr::distinct()
  
  incomplete_df <- dplyr::select(df, -well_plate_name, -well_plate_index, -well, -roi)
  
  complete_tracks_df <- 
    tidyr::expand_grid(cell_id = {{all_cell_ids}}, frame_num = {{all_frames}}) %>% 
    dplyr::left_join(x = ., y = incomplete_df, by = c("cell_id", "frame_num")) %>% 
    dplyr::left_join(x = id_df, y = ., by = "cell_id") %>% 
    dplyr::mutate(
      frame_added = tidyr::replace_na(frame_added, replace = TRUE), 
      frame_time = frame_num * itvl
    ) %>% 
    dplyr::select(-dplyr::any_of(var_names_dfs$well_plate)) %>% 
    dplyr::select(cell_id, dplyr::all_of(var_names_dfs$tracks), dplyr::everything())
  
  return(complete_tracks_df)
  
}




