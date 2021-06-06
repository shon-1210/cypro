


#' @title Discard unwanted variables from your data
#' 
#' @description Discards variables from the provided object. They can not be restored apart
#' from adding them again artificially. Use with caution.
#'
#' @inherit argument_dummy 
#' @param cluster_variables,meta_variables Character vector. The variables you want to discard.
#'
#' @inherit updated_object return
#' @export
#'
discardClusterVariables <- function(object, cluster_variables, phase = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  confuns::check_one_of(
    input = cluster_variables, 
    against = getClusterVariableNames(object, phase = phase)
  )
  
  cluster_df <-
    getClusterDf(object, phase = phase) %>% 
    dplyr::select(-dplyr::all_of(cluster_variables))
  
  object <- setCellDf(object, slot = "cluster", df = cluster_df, phase = phase)
  
  base::return(object)
  
}

#' @rdname discardClusterVariables
#' @export
discardMetaVariables <- function(object, meta_variables, phase = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  all_meta_vars <- 
    getMetaVariableNames(object, phase = phase)

  valid_meta_vars <- all_meta_vars[!all_meta_vars %in% c("cell_line", "condition")]
    
  if(base::length(valid_meta_vars) == 0){
    
    base::stop("There are no meta variables to be discarded.")
    
  } else {
    
    confuns::check_one_of(
      input = meta_variables, 
      against = valid_meta_vars
    )
    
  }
  
  meta_df <-
    getMetaDf(object, phase = phase) %>% 
    dplyr::select(-dplyr::all_of(meta_variables))
  
  object <- setCellDf(object, slot = "meta", df = meta_df, phase = phase)
  
  base::return(object)
  
}



#' @title Discard unwanted variables from your data
#' 
#' @description This function discards unwanted variables from the cypro object 
#' and makes them unavailable for subsequent analysis steps. Might be useful for 
#' decreasing the overall object size. 
#'
#' @inherit argument_dummy 
#' @param cluster_variables Character vector. The cluster variables you want to discard.
#' @param stat_variables Character vector. The variables from the cell statistics data you want to discard
#' in case of \code{discardStatVariables()} or keep in case of \code{keepStatVariables()}. 

#' @details As clustering and dimensional reduction results base on stat variables 
#' or variable sets to be more precise all clustering results in slot @@analysis that included variables denoted
#' in argument \code{stat_variables} are discarded, too. This is because clustering
#' results based on variable x, y and z might differ from results based 
#' on only variables x and y. 
#' 
#' Defined variable sets including variables that are discarded are modified in a way that 
#' they only contain variables that remain in the cypro object.
#' 
#' Correlation results are not discarded but only filtered accordingly. This is because correlation 
#' results between variable x and variable y are not affected by discarding variable z. 
#'
#' @inherit updated_object return
#' @export
#'
discardStatVariables <- function(object, stat_variables, verbose = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  stat_variables <- base::unique(stat_variables)
  
  confuns::check_one_of(
    input = stat_variables, 
    against = getStatVariableNames(object)
  )
  
  # variable sets 
  affected_vsets <- 
    purrr::keep(.x = object@variable_sets, .p = ~ base::any(stat_variables %in% .x)) %>% 
    base::names()
  
  # subset variable sets
  object@variable_sets <- 
    purrr::map(object@variable_sets, .f = function(vset){
      
      subsetted_vset <- 
        vset[!vset %in% stat_variables]
      
      base::return(subsetted_vset)
      
    })
  
  if(multiplePhases(object)){
    
    object@cdata$stats <- 
      purrr::map(
        .x = object@cdata$stats, 
        .f = ~ dplyr::select(.x, -dplyr::all_of(x = stat_variables))
      )
    
    corr_names <- base::names(object@analysis$correlation)
    
    affected_corr_sets <- base::intersect(affected_vsets, corr_names)
    
    if(base::length(affected_corr_sets) >= 1){
      
      for(aff_corr in affected_corr_sets){
        
        object@analysis$correlation[[aff_corr]] <- 
          purrr::map(
            .x = object@analysis$correlation[[aff_corr]], 
            .f = confuns::discard_numeric_vars, 
            vars = stat_variables, 
            remove.data = FALSE
          )
        
      }
      
    }
    
  } else {
    
    object@cdata$stats <- 
      dplyr::select(object@cdata$stats, -dplyr::all_of(x = stat_variables))
    
    corr_names <- base::names(object@analysis$correlation)
    
    affected_corr_sets <- base::intersect(affected_vsets, corr_names)
    
    if(base::length(affected_corr_sets) >= 1){
      
      for(aff_corr in affected_corr_sets){
        
        object@analysis$correlation[[aff_corr]] <- 
          confuns::discard_numeric_vars(
            corr.obj = object@analysis$correlation[[aff_corr]], 
            vars = stat_variables, 
            discard.data = FALSE
          )
        
      }
      
    }
    
  }
  
  # discard dim red and clustering 
  for(vset in affected_vsets){
    
    for(slot in base::names(analysis_methods)){
      
      for(method in analysis_methods[[slot]]){
        
        object@analysis[[slot]][[method]][[vset]] <- NULL
        
      }
      
    }
    
  }
  
  object@vdata$summary <- 
    object@vdata$summary%>% dplyr::filter(!variable %in% {{stat_variables}})
  
  if(multiplePhases(object)){
    
    object@vdata$summary <- 
      purrr::map(.x = object@vdata$by_phase, 
                 .f = ~ dplyr::filter(.x, !variable %in% {{stat_variables}}))
    
  }
  
  n_vars <- base::length(stat_variables)
  
  msg <- 
    glue::glue(
      "Discarded {n_vars} {ref}.", 
      ref = confuns::adapt_reference(stat_variables, "variable", "variables")
      )
  
  confuns::give_feedback(msg = msg, verbose = verbose)
  
  base::return(object)
  
}

#' @rdname discardStatVariables
#' @export
keepStatVariables <- function(object, stat_variables, verbose = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  all_stat_variables <- getStatVariableNames(object)
  
  confuns::check_one_of(
    input = stat_variables, 
    against = all_stat_variables
  )
  
  discard_variables <-
    all_stat_variables[!all_stat_variables %in% stat_variables]
  
  object <-
    discardStatVariables(object, stat_variables = discard_variables, verbose = verbose)
  
  base::return(object)
  
  
}



