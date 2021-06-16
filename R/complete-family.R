
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
    dplyr::select(df, cell_id, where(base::is.numeric))
  
  complete_num_df <- 
    tidyr::expand_grid(cell_id = {{all_cell_ids}}, frame_num = {{all_frames}}) %>% 
    dplyr::left_join(x = ., y = numeric_df, by = c("cell_id", "frame_num"))
  
  complete_df <- 
    dplyr::left_join(x = complete_num_df, y = group_df, by = "cell_id") %>% 
    dplyr::distinct() %>% # temporary solution to weird multiplying of observations
    dplyr::arrange(cell_id) %>% 
    dplyr::select(cell_id, where(base::is.numeric)) # discard non numeric variables
  
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
      verbose = verbose
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





summarize_module_variables <- function(stat_df, track_df, object, verbose){
  
  used_modules <- get_used_module_names(object)
  
  track_df <- dplyr::group_by(track_df, cell_id)
  
  for(i in base::seq_along(used_modules)){
    
    used_module <- used_modules[i]
    
    module_info <- 
      stringr::str_c("module", used_module, sep = "_") %>% 
      base::parse(text = .) %>% 
      base::eval()
    
    if(!base::is.null(module_info$summary_order)){
      
      confuns::give_feedback(
        msg = glue::glue("Module: '{module_info$pretty_name}'"),
        verbose = verbose
      )
      
      for(var_to_summarize in module_info$summary_order){
        
        variable_info <- module_info$variables_to_summarize[[var_to_summarize]]
        
        var_name_in_app <- variable_info$name_in_app
        
        confuns::give_feedback(
          msg = glue::glue("Variable: '{var_name_in_app}' ('{var_to_summarize}')"), 
          verbose = verbose
        )
        
        fn <- variable_info$summarize_with
        
        args <- list(track_df = track_df, stat_df = stat_df, object = object)
        
        summarized_df <- rlang::invoke(.fn = fn, .args = args)
        
        stat_df <- dplyr::left_join(x = stat_df, y = summarized_df, by = "cell_id")
        
      }
      
    }
    
  }
  
  return(stat_df)
  
}






