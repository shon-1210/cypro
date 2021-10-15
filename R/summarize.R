


#' @title Summarize time lapse data variables 
#' 
#' @description Sets up the stat data.frame based on the data variables 
#' of slot @@tracks. This includes summary via the denoted functions 
#' in argument \code{summarize_with} as well as all module based 
#' variables. 
#' 
#' @inherit argument_dummy params
#' 
#' @note As this function affects and changes the whole stat data.frame all analysis 
#' results are discarded.
#'
#' @inherit update_object return
#'
summarizeTrackDfWith <- function(object,
                                 summarize_with = c("max", "mean", "median", "min"),
                                 verbose = NULL){
  
  check_object(object, exp_type_req = "time_lapse")
  assign_default(object)
  
  object <-
    set_up_cdata_stats(
      object = object, 
      summarize_with = summarize_with, 
      verbose = verbose
    )
  
  object <- 
    set_up_vdata(object = object, verbose = verbose)
  
  object@analysis <- list()
  
  base::return(object)
  
}



# helper ------------------------------------------------------------------

summarize_track_df <- function(object,
                               enquos_input,
                               discard_variables = FALSE,
                               phase = NULL,
                               overwrite = FALSE,
                               verbose = TRUE){
  
  df <- 
    getTracksDf(object, phase = phase, with_grouping = FALSE, verbose = FALSE)
  
  if(base::is.character(discard_variables)){
    
    df <- dplyr::select(df, -dplyr::any_of(x = discard_variables))
    
  }
  
  confuns::give_feedback(msg = "Summarizing track data by cell id.", verbose = verbose)
  
  print(enquos_input)
  
  smrd_df <- 
    dplyr::group_by(df, cell_id) %>% 
    dplyr::summarize(!!!enquos_input)
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  new_vars <- base::colnames(smrd_df) %>% confuns::vselect(-cell_id)
  
  stat_vars <- getStatVariableNames(object, phase = phase)
  
  overlap <- base::intersect(new_vars, stat_vars)
  
  if(base::length(overlap) >= 1){
    
    if(base::isTRUE(overwrite)){
      
      confuns::give_feedback(msg = "Discarding overlapping variables.", verbose = verbose)
      
      object <- discardStatVariables(object, stat_variables = overlap, verbose = verbose)
      
    } else {
      
      msg <- 
        glue::glue(
          "Summarizing track data.frame resulted in already existing stat {ref_vars}:", 
          " '{confuns::scollapse(overlap)}' ", 
          "Set argument 'overwrite' to TRUE in order to allow overwriting.",
          ref_vars = confuns::adapt_reference(overlap, "variable", "variables")
        )
      
      confuns::give_feedback(
        msg = msg, 
        fdb.fn = "stop", 
        with.time = FALSE
      )
      
    }
    
  }
  print(head(smrd_df))
  assign(x = phase, value = smrd_df, envir = .GlobalEnv)
  object <-
    addStatVariables(
      object = object,
      input_df = smrd_df, 
      variable_names = new_vars,
      phase = phase, 
      overwrite = FALSE
      )
  
  base::return(object)
  
}