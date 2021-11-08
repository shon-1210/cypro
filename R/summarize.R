


# m -----------------------------------------------------------------------

summarize_module_variables_hlpr <- function(object, tracks_df, stats_df, verbose, ...){
  
  modules <- 
    object@modules[getActiveModuleNames(object)] %>% 
    purrr::keep(.p = ~ containsSummaryVariables(.x))
  
  for(i in base::seq_along(modules)){
    
    module <- modules[[i]]
    
    vars_to_summarize <- module@variables_summary
    
    n_vts <- base::length(vars_to_summarize)
    
    ref <- adapt_reference(vars_to_summarize, "variable")
    
    give_feedback(
      msg = glue::glue("Summarizing {n_vts} {ref} for module {module@name_in_app}."),
      verbose = verbose
    )
    
    for(v in base::seq_along(vars_to_summarize)){
      
      vts <- vars_to_summarize[[v]]
      
      give_feedback(
        msg = glue::glue("Summarizing variable '{vts@name_in_app}' ({vts@name_in_cypro})."), 
        verbose = verbose
      )
      
      stats_df <- vts@summarize_with(tracks_df = tracks_df, stats_df = stats_df, object = object)
      
    }
    
  }
  
  return(stats_df)
  
}


# v -----------------------------------------------------------------------

# functions to summarize variables within summarize_module_variables()
# must take tracks_df, stats_df and ... as arguments. 
# must return the stats data.frame with the new variable as output
summarize_var_mgr_eff <- function(tracks_df, stats_df, object, ...){
  
  smrd_df <- 
    dplyr::left_join(
      x = tracks_df, 
      y = stats_df[,c("cell_id", "total_dist")],
      by = "cell_id"
    ) %>% 
    dplyr::group_by(cell_id) %>% 
    dplyr::summarize(
      mgr_eff = compute_migration_efficiency(x_coords, y_coords, total_dist)
    )
  
  df <- dplyr::left_join(x = stats_df, y = smrd_df, by = "cell_id")
  
  return(df)
  
}

summarize_var_total_dist <- function(tracks_df, stats_df, object, ...){
  
  smrd_df <-
    dplyr::group_by(tracks_df, cell_id) %>% 
    dplyr::summarize(total_dist = base::sum(dflp))
  
  df <- dplyr::left_join(x = stats_df, y = smrd_df, by = "cell_id")
  
  return(df)
  
}





# A -----------------------------------------------------------------------

# D -----------------------------------------------------------------------

#' @title Summarize time lapse data by cell id
#' 
#' @description Summarizes time lapse data (the \code{tracks_df}) by cell id and sets up 
#' the \code{stats_df} - the cellular statistics data.frame.
#'  
#' @inherit argument_dummy params
#' 
#' @return The input object. 
#' @export
#'
setGeneric(name = "summarizeDataByCell", def = function(object, verbose = TRUE){
  
  standardGeneric(f = "summarizeDataByCell")
  
})


#' @rdname summarizeDataByCell
#' @export
setMethod(f = "summarizeDataByCell", signature = "CyproTimeLapse", definition = function(object, verbose = TRUE){
  
  give_feedback(
    msg = "Summarizing time lapse data by cell id.", 
    verbose = verbose
  )
  
  stats_df <- 
    getTracksDf(object) %>% 
    dplyr::group_by(cell_id) %>%
    dplyr::select(-dplyr::all_of(var_names_dfs$tracks), -dplyr::any_of(var_names_non_features)) %>% 
    dplyr::summarize(
      dplyr::across(
        .cols = where(base::is.numeric), 
        .fns = stat_funs, 
        na.rm = TRUE
      )
    )
  
  object <- setStatsDf(object, df = stats_df)
  
  return(object)
  
})

#' @rdname summarizeDataByCell
#' @export
setMethod(f = "summarizeDataByCell", signature = "CyproTimeLapseMP", definition = function(object, verbose = TRUE){
  
  give_feedback(
    msg = "Summarizing time lapse data by cell id.", 
    verbose = verbose
  )
  
  phases <- getPhases(object)
  
  for(p in phases){
    
    give_feedback(
      msg = glue::glue("Working on phase {p}."), 
      verbose = verbose
    )
    
    tracks_df <- getTracksDf(object, phase = p)
    
    smrd_df <- 
      dplyr::group_by(tracks_df, cell_id) %>%
      dplyr::select(-dplyr::all_of(var_names_dfs$tracks), -dplyr::any_of(var_names_non_features)) %>% 
      dplyr::summarize(
        dplyr::across(
          .cols = where(base::is.numeric), 
          .fns = stat_funs, 
          na.rm = TRUE
        )
      ) %>% 
      magrittr::set_attr(which = "phase", value = p)
    
    object <- setStatsDf(object, df = smrd_df, phase = p)
    
  }
  
  return(object)
  
})


#' @title Summarize data by well
#' 
#' @description Summarizes cell based data by well for every well plate and 
#' adds the summarized variables to the layout data.frame of every well plate. 
#'
#' @inherit argument_dummy params
#' @param features Character or NULL. If character, selects the numeric 
#' variables that are summarized. If NULL, all are summarized.
#' 
#' @details Summarized data variables that already exist under the same name in the 
#' layout data.frames are overwritten without asking. 
#'
#' @return The input object.
#' 
#' @seealso \code{getLayoutVariableNames()}
#' 
#' @export
#'
setGeneric(name = "summarizeDataByWell", def = function(object, ...){
  
  standardGeneric(f = "summarizeDataByWell")
  
})

#' @rdname summarizeDataByWell
#' @export
setMethod(
  f = "summarizeDataByWell",
  signature = "CyproScreening",
  definition = function(object, summarize_with = "mean", features = NULL, verbose = TRUE){
    
    give_feedback(
      msg = "Summarizing data by well.", 
      verbose = verbose
    )
    
    check_summarize_with(summarize_with = summarize_with)
    
    df <- 
      getFeatureDf(object, with_well_plate = TRUE) %>% 
      dplyr::select(-dplyr::any_of(c("well_plat_index"))) %>% 
      dplyr::select(-dplyr::any_of(non_data_variables))
    
    if(base::is.character(features)){
      
      df <- 
        dplyr::select(
          .data = df,
          dplyr::all_of(var_names_dfs$well_plate), 
          dplyr::all_of(features)
        )
      
    }
    
    smrd_df <- 
      dplyr::group_by(df, well_plate_name, well) %>% 
      dplyr::select_if(.predicate = base::is.numeric) %>% 
      dplyr::summarise(
        dplyr::across(
          .fns = stat_funs[summarize_with], 
          na.rm = TRUE
        ), 
        .groups = "drop"
      )
    
    well_plates <- getWellPlates(object)
    
    smrd_features <- 
      dplyr::select_if(smrd_df, .predicate = base::is.numeric) %>% 
      base::colnames()
    
    for(wp in well_plates){
      
      wp_name <- wp@name
      
      wp_df <- dplyr::filter(smrd_df, well_plate_name == {{wp_name}})
      
      layout_df <- 
        getLayoutDf(wp) %>% 
        nestLayoutDf() %>% 
        dplyr::select(-dplyr::any_of(smrd_features))
      
      new_layout_df <- 
        dplyr::left_join(
          x = layout_df,
          y = smrd_df, 
          by = c("well_plate_name", "well")
        )
      
      wp <- setLayoutDf(object = wp, layout_df = new_layout_df)
      
      object <- setWellPlate(object, well_plate_object = wp)
      
    }
    
    return(object)
    
  })


















# M -----------------------------------------------------------------------

#' @title Summarize module variables 
#' 
#' @description Summarizes the summary variables of all active analysis modules
#' in the \code{Cypro} object and adds them to \code{Cypro} objects data.
#'
#' @inherit argument_dummy params
#'
#' @return The input object. 
#' @export
#'
setGeneric(name = "summarizeModuleVariables", def = function(object, ...){
  
  standardGeneric(f = "summarizeModuleVariables")
  
})

#' @rdname summarizeModuleVariables
#' @export
setMethod(
  f = "summarizeModuleVariables",
  signature = "CyproTimeLapse",
  definition = function(object, verbose = TRUE){
  
  give_feedback(
    msg = "Checking for analysis module related variables that need to be summarized.",
    verbose = verbose
  )
  
  tracks_df <- 
    getTracksDf(object) %>% 
    dplyr::group_by(cell_id)
  
  stats_df <- getStatsDf(object)
  
  stats_df <-
    summarize_module_variables_hlpr(
      object = object,
      stats_df = stats_df,
      tracks_df = tracks_df, 
      verbose = verbose
      )
  
  object <- setStatsDf(object, df = stats_df)
  
  return(object)
  
})

#' @rdname summarizeModuleVariables
#' @export
setMethod(
  f = "summarizeModuleVariables",
  signature = "CyproTimeLapseMP",
  definition = function(object, verbose = TRUE){
    
    give_feedback(
      msg = "Checking for analysis module related variables that need to be summarized.",
      verbose = verbose
    )
    
    phases <- getPhases(object)
    
    for(p in phases){
      
      tracks_df <- 
        getTracksDf(object, phase = p) %>% 
        dplyr::group_by(cell_id)
      
      stats_df <- getStatsDf(object, phase = p)
      
      stats_df <-
        summarize_module_variables_hlpr(
          object = object,
          stats_df = stats_df,
          tracks_df = tracks_df, 
          verbose = verbose
        )
      
      object <- setStatsDf(object, df = stats_df, phase = p)
      
    }
    
    return(object)
    
  })









