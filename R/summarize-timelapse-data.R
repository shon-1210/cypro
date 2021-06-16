


#' @title Summarize time lapse data variables 
#' 
#' @description This function allows to summarize time lapse data variables 
#' stored in slot @@tracks and to add the summarized variables to the 
#' data.frame in slot @@stats.
#' 
#' @inherit argument_dummy params
#' @param ... Name-value pairs according to the syntax of \code{dplyr::summarize()}. 
#' This can be single or several expressions as well as usage of \code{dplyr::across()} if several 
#' variables are supposed to be affected. See details for output requirements.
#' @param discard_variables If character, denotes variables that are discarded 
#' prior to the call to \code{dplyr::summarize()}. Might be useful in case of 
#' more complex input for argument \code{...}.
#' 
#' @details Prior to summarizing the tracks data.frame is grouped by \emph{cell_id}
#' via \code{dplyr::group_by()}. All data variables of the output data.frame of
#' \code{dplyr::summarize(...)} is joined to the stats data.frame via 
#' \code{addStatVariables()}.
#'
#' @inherit update_object return
#'
summarizeTrackVariables <- function(object,
                                    ...,
                                    discard_variables = NULL,
                                    overwrite = FALSE,
                                    phase = NULL,
                                    verbose = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  phases <- check_phase(object, phase = phase)
  
  enquos_input <- rlang::enquo(...)
  
  if(multiplePhases(object)){
    
    for(p in phases){
      
      object <-
        summarize_track_df(
          object = object, 
          enquos_input = enquos_input,
          discard_variables = discard_variables,
          phase = p, 
          overwrite = overwrite, 
          verbose = verbose
        )
      
    }
    
  } else {
    
    object <-
      summarize_track_df(
        object = object,
        enquos_input = enquos_input,
        discard_variables = discard_variables,
        overwrite = overwrite,
        verbose = verbose
        )
    
  }
  
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