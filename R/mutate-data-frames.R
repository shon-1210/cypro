



#' @title Create and modify data variables 
#' 
#' @description Implementation of \code{dplyr::mutate()} that allows 
#' to modify cell data.frames of the cypro object. 
#'
#' @inherit argument_dummy params
#' @param ... Name-value pairs according to the syntax of \code{dplyr::mutate()}. 
#' This can be single or several expressions as well as usage of \code{dplyr::across()} if several 
#' variables are supposed to be affected. See details for output requirements. 
#' @param group_by Character vector or NULL. If character, denotes the grouping variables 
#' according to which the cell data.frame is grouped via \code{dplyr::group_by()}
#' prior to the call to \code{dplyr::mutate()}.
#'
#' @details
#' 
#' Process: 
#' 
#' The data.frame that is given to \code{dplyr::mutate()} contains all variables
#' from slots \emph{cluster, meta, stats} and \emph{well_plate}. Meaning that you 
#' can refer to variables from slot cluster or well plate, for instance, if you
#' want to mutate the stats data.frame. Prior to setting the
#' mutated data.frame back in its original slot data variables from other slots are removed. 
#' 
#' Output requirements to ensure the cypro objects integrity:
#' 
#' Protected variables such as \emph{cell_id, cell_line, condition, well_plate_name} etc. 
#' must not be changed. Attempts result in an error message.
#' 
#' In case of the stats data.frame new variables must be numeric. In case 
#' of the other three slots new variables must be of class factor or character.
#' The latter is converted to factor automatically. 
#' 
#' Discarding variables with e.g. cluster_variable_x = NULL is not allowed. Use
#' the \code{discard*()}-functions for that matter.
#' 
#' @inherit updated_object return
#' @export
#'
mutateClusterDf <- function(object, ..., group_by = NULL, phase = NULL){
  
  enquos_input <- rlang::enquos(...)
  
  mutate_cdata_slot(
    object = object, 
    slot = "meta", 
    phase = phase, 
    enquos_input = enquos_input, 
    group_by = group_by
  )
  
}

#' @rdname mutateClusterDf
#' @export
mutateMetaDf <- function(object, ..., group_by = NULL, phase = NULL){
  
  enquos_input <- rlang::enquos(...)
  
  mutate_cdata_slot(
    object = object, 
    slot = "meta", 
    phase = phase, 
    enquos_input = enquos_input, 
    group_by = group_by
  )
  
}

#' @rdname mutateClusterDf
#' @export
mutateStatsDf <- function(object, ..., group_by = NULL){
  
  enquos_input <- rlang::enquos(...)
  
  mutate_cdata_slot(
    object = object, 
    slot = "stats", 
    enquos_input = enquos_input, 
    group_by = group_by,
    phase = "all"
  )
  
}

#' @title Create and modify data variables 
#'
#' @inherit mutateClusterDf params return
#' 
#' @details 
#' 
#' The data.frame that is given to \code{dplyr::mutate()} contains all variables
#' from slots \emph{cluster, meta} and \emph{well_plate}. Meaning that you 
#' can refer to these variables in more complex mutations. Prior to setting the
#' mutated data.frame back in its original slot data variables from other slots are removed. 
#' 
#' Output requirements to ensure the cypro objects integrity:
#' 
#' Protected variables such as \emph{cell_id, x_coords, y_coords} etc. 
#' must not be changed. Attempts result in an error message.
#' 
#' In case of the tracks data.frame new variables must be numeric.
#' 
#' Discarding variables with e.g. distance = NULL is not allowed. Use
#' the \code{discard*()}-functions for that matter.
#'
#' @export
#'
mutateTracksDf <- function(object, ..., group_by = NULL){
  
  enquos_input <- rlang::enquos(...)
  
  mutate_cdata_slot(
    object = object, 
    slot = "tracks", 
    enquos_input = enquos_input, 
    group_by = group_by,
    phase = "all"
  )
  
}

  
# helper ----------------------------------------------------------------

mutate_cdata_slot <- function(object, slot, phase = NULL, enquos_input, group_by = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  # stats and tracks have to be mutated across all phases
  if(slot %in% c("stats", "tracks")){
    
    phase <- "all"
    
  }
  
  phase <- check_phase(object, phase = phase)
  
  if(multiplePhases(object)){
    
    all_phases <- phase
    
    object@cdata[[slot]][all_phases] <- 
      purrr::map2(
        .x = object@cdata[[slot]][all_phases],
        .y = all_phases, 
        .f = mutate_cell_df, 
        object = object,
        slot = slot,
        enquos_input = enquos_input, 
        group_by = group_by
      )
    
  } else {
    
    df <- getCellDf(object, slot = slot, phase = phase)
    
    mdf <-
      mutate_cell_df(
        df = df,
        object = object,
        slot = slot,
        enquos_input = enquos_input, 
        group_by = group_by
        )
    
    object <- setCellDf(object, slot = slot, df = mdf, phase = phase)
    
  }
  
  base::return(object)
  
}


mutate_cell_df <- function(df, phase = NULL, object, slot, enquos_input, group_by = NULL){
  
  var_names <- NULL
  
  join_slots <- cdata_slots[!cdata_slots %in% c(slot, "tracks")]

  join_df <- 
    getStatsDf(object, phase = phase, drop_na = FALSE) %>%
    dplyr::select(cell_id)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  for(join_slot in join_slots){
    
    join_df <-
      dplyr::left_join(
        x = join_df,
        y = getCellDf(object, slot = join_slot, phase = phase),
        by = "cell_id")
    
    
  }
    
  joined_vars <-
    base::colnames(join_df) %>%
    confuns::vselect(-cell_id)
    
  if(slot == "meta"){
    
    core_vars <- c("cell_id", "cell_line", "condition")
    
    var_names <- core_vars
    
  } else if(slot == "well_plate"){
    
    core_vars <- c("cell_id", well_plate_vars)
    
    var_names <- core_vars
    
  } else if(slot == "stats"){
    
    core_vars <- c("cell_id")
    
    var_names <- getStatVariableNames(object)
    
  } else if(slot == "tracks"){
    
    core_vars <- c("cell_id")
    
    var_names <- getTrackVariableNames(object)
    
  }
  
  # module vars 
  used_modules <- get_used_module_names(object)
  
  for(i in base::seq_along(used_modules)){
  
    used_module <- used_modules[i]
    
    module_info <- cypro_modules[[used_module]]
    
    needed_vars <- 
      module_info$variables %>% 
      purrr::keep(.p = ~ .x$relevance == "needed") %>% 
      base::names()
    
    core_vars <- c(core_vars, needed_vars)
    
  }
  
  # extraction part
  cell_df <- 
    dplyr::left_join(x = df, y = join_df, by = "cell_id")
  
  original_vars <- base::colnames(cell_df)
  
  core_df <- dplyr::select(cell_df, dplyr::all_of(core_vars))
  
  if(base::is.character(group_by)){
    
    confuns::check_one_of(
      input = group_by, 
      against = base::colnames(cell_df)
    )
    
    cell_df <-
      dplyr::group_by(cell_df, dplyr::across(.cols = dplyr::all_of(group_by)))
    
    confuns::give_feedback(
      msg = glue::glue("Grouped data.frame by '{confuns::scollapse(group_by}'."),
      verbose = verbose
    )
    
  }
  
  
  
  # mutation part
  
  msg <- 
    glue::glue(
      "Mutating {slot}-data.frame{ref_phase}.",
      ref_phase = hlpr_glue_phase(object, phase, FALSE, "of")
      )
  
  confuns::give_feedback(msg = msg, verbose = verbose)
  
  mutated_df <-
    dplyr::mutate(cell_df, !!!enquos_input, .keep = "all", .before = NULL, .after = NULL) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-dplyr::all_of(joined_vars))
  
  mutated_vars <- base::colnames(mutated_df)
  
  new_vars <- mutated_vars[!mutated_vars %in% original_vars]
  
  if(slot %in% c("cluster", "meta", "well_plate")){
    
    # make sure that all vars except for cell_id are factors
    mutated_df <- 
      dplyr::mutate(
        dplyr::across(
          .cols = -cell_id, 
          .fns = function(var){
            
            if(base::is.character(var)){
              
              var <- base::as.factor(var)
              
            }
            
            base::return(var)
            
          }
        )
      )
    
  }
  
  mcore_df <- dplyr::select(mutated_df, dplyr::all_of(core_vars))
  
  # output check
  if(!base::identical(x = core_df, y = mcore_df)){
    
    msg <- 
      glue::glue(
        "While mutating a cell data.frame of slot {slot} the variables",
        "'{confuns::scollapse(core_vars)}' must not change."
        )
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  if(base::is.character(var_names)){
    
    if(!base::all(var_names %in% base::colnames(mutated_df))){
      
      msg <- 
        glue::glue("Some variables are missing in the mutated output data.frame. ",
                   "Discarding variables within cypros mutate-functions is not allowed. ",
                   "Please use the discard-functions for that matter.")
      
      confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
      
    }
    
  }
  
  if(base::length(new_vars) >= 1){
    
    new_df <- dplyr::select(mutated_df, dplyr::all_of(new_vars))
    
    # make sure that grouping vars are factors
    if(slot %in% c("meta", "well_plate", "cluster")){
      
      var.class <- 
        purrr::map(new_vars, function(var){ return("factor")}) %>%
        purrr::set_names(nm = new_vars)
      
      confuns::check_data_frame(
        df = new_df, 
        var.class = var.class, 
        ref = base::as.character(glue::glue("mutated {slot} data.frame"))
      )
      
      # and data vars are numeric
    } else {
      
      var.class <- 
        purrr::map(new_vars, function(var){ return("numeric")}) %>%
        purrr::set_names(nm = new_vars)
      
      confuns::check_data_frame(
        df = new_df, 
        var.class = var.class, 
        ref = base::as.character(glue::glue("mutated {slot} data.frame"))
      )
      
    }
    
  }
  
  base::return(mutated_df)
  
}






