

# Documentations dummies --------------------------------------------------

#' dummy 
#' @return An updated cypro object that contains the data added.
add_family <- function(){}






# Functions ---------------------------------------------------------------


#' @title Add predefined set of variables 
#' 
#' @description Allows to store predefined set of variable names in the object that 
#' can be easily accessed via \code{getVariableSet()}.
#'
#' @inherit argument_dummy params
#' @param variable_names Character vector. Denotes the variable names that are part 
#' of the variable set. 
#' @param set_name Character value. The name of the set of variable names. 
#'
#' @inherit update_object return
#' @export
#'
addVariableSet <- function(object, variable_names, set_name, overwrite = FALSE){
  
  check_object(object)
  
  assign_default(object)
  
  set_names <- getVariableSetNames(object)
  
  if(!base::is.null(set_names)){
    
    confuns::check_none_of(
      input = set_name, 
      against = set_names, 
      ref.against = "defined variable sets", 
      overwrite = overwrite
    )
    
  }
  
  if(!isTimeLapseExp(object)){
    
    valid_variables <- 
      getTrackVariableNames(object)
    
  } else {
    
    valid_variables <- 
      getStatVariableNames(object)
    
  }
  
  confuns::check_one_of(
    input = variable_names, 
    against = valid_variables
  )
  
  object@variable_sets[[set_name]] <- variable_names
  
  # reset variable set based analysis results
  for(slot in c("dim_red", "clustering")){
    
    for(method in analysis_methods[[slot]]){
      
      object@analysis[[slot]][[method]][[set_name]] <- NULL
      
    }
    
  }
  
  object@analysis$correlation[[set_name]] <- NULL
  
  base::return(object)
  
}




#' @title Add discrete/categorical variables that group the cells
#' 
#' @description Allows to join new discrete/categorical variables that can be referred 
#' to via the \code{across}-argument of many functions.
#' 
#' @inherit argument_dummy params
#' @inherit check_object params 
#' @param by Character value. Denotes the variable by which the new informational 
#' variables specified in argument \code{variable_names} are supposed to be joined. 
#' In case of \code{addClusterVariables()} and \code{addMetaVariables()} this does not 
#' have to be \emph{'cell_id'}. 
#' 
#' E.g. if you want to add additional grouping options that refer to the conditions such 
#' as \emph{'mechanism_of_action'} again grouping the conditions you can specify
#' \code{by} = \emph{'condition'}.
#' 
#' @param input_df A data.frame that contains the variables denoted in argument \code{variable_names}
#' as well as a character variable named according to input of argument \code{by} that is used to match 
#' and join both data.frames. 
#' @param variable_names Character vector. The name of the variables that are to be joined.
#' 
#' @inherit add_family return
#'
#' @export

addClusterVariables <- function(object,
                                input_df,
                                variable_names,
                                phase = NULL,
                                overwrite = FALSE,
                                by = "cell_id",
                                verbose = TRUE){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  new_input_df <- 
    dplyr::select(input_df, dplyr::all_of(x = c(by, variable_names)))
  
  if(phase %in% base::colnames(new_input_df)){
    
    new_input_df$phase <- NULL
    
    msg <- "Discarding variable 'phase'. (invalid)"
    
    confuns::give_feedback(msg = msg, fdb.fn = "warning")
    
  }
  
  old_group_df <- getClusterDf(object, phase = phase, verbose = FALSE)
  
  updated_group_df <- 
    confuns::join_safely(
      old.df = old_group_df, 
      new.df = new_input_df, 
      ref.new.df = "input_df", 
      variable.names = variable_names, 
      valid.classes = "factor", 
      by = by,
      overwrite = overwrite, 
      verbose = verbose
    )
  
  object <- setCellDf(object, slot = "cluster", df = updated_group_df, phase = phase)
  
  base::return(object)
  
}

#' @rdname addClusterVariables
#' @export
addMetaVariables <- function(object,
                             input_df,
                             variable_names,
                             phase = NULL,
                             overwrite = FALSE,
                             with_well_plate = FALSE,
                             by = "cell_id",
                             verbose = TRUE){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  new_input_df <- 
    dplyr::select(input_df, dplyr::all_of(x = c(by, variable_names)))
  
  if(phase %in% base::colnames(new_input_df)){
    
    new_input_df$phase <- NULL
    
    msg <- "Discarding variable 'phase'. (invalid)"
    
    confuns::give_feedback(msg = msg, fdb.fn = "warning")
    
  }
                  
  old_group_df <- getMetaDf(object, phase = phase)
  
  if(base::isTRUE(with_well_plate)){
    
    old_group_df <- 
      dplyr::left_join(
        x = old_group_df, 
        y = getWellPlateDf(object), 
        by = "cell_id"
      )
    
  }
  
  updated_group_df <- 
    confuns::join_safely(
      old.df = old_group_df, 
      new.df = new_input_df, 
      ref.new.df = "input_df", 
      variable.names = variable_names, 
      valid.classes = "factor", 
      by = by,
      overwrite = overwrite, 
      verbose = verbose
    )
  
  updated_group_df <- 
    check_nrow(
      df = updated_group_df, 
      n_rows = nCells(object), 
      ref = "meta"
    )
  
  if(base::isTRUE(with_well_plate)){
    
    updated_group_df <- 
      dplyr::select(updated_group_df, -dplyr::any_of(well_plate_vars))
    
  }
  
  updated_group_df <- 
    dplyr::mutate(
      updated_group_df,
      dplyr::across(
        .cols = -cell_id & where(base::is.character), 
        .fns = base::as.factor
      )
    ) %>% 
    dplyr::mutate_if(.predicate = base::is.factor, .funs = base::droplevels)
  
  object <- setCellDf(object, slot = "meta", df = updated_group_df, phase = phase)
  
  base::return(object)
  
}


#' @title Add numeric variables
#' 
#' @description Allows to join numeric variables that can be referred 
#' to via the \code{variables}-argument of many functions.
#' 
#' @inherit addClusterVariables params
#' @inherit argument_dummy params
#' @param stat_df A data.frame that contains the variables denoted in argument \code{variable_names}
#' as well as a character variable named \emph{cell_id} matching the cell ids of the input object. 
#' @param variable_names Character vector. The name of the variables that are to be joined.
#' 
#' @inherit add_family return
#'
#' @export
addStatVariables <- function(object,
                             input_df,
                             variable_names,
                             phase = NULL,
                             verbose = TRUE){
  
  overwrite = FALSE
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  new_input_df <- 
    dplyr::select(input_df, dplyr::all_of(x = c("cell_id", variable_names)))
  
  new_vars <- base::colnames(input_df) %>% confuns::vselect(-cell_id)
  
  if(phase %in% new_vars){
    
    new_input_df$phase <- NULL
    
    msg <- "Discarding variable 'phase'. (invalid)"
    
    confuns::give_feedback(msg = msg, fdb.fn = "warning")
    
  }
  
  old_stat_df <-
    getStatsDf(
      object = object,
      phase = phase,
      with_grouping = FALSE,
      verbose = FALSE
      )
  
  stat_vars <- getStatVariableNames(object, phase = phase)
  
  overlap <- base::intersect(new_vars, stat_vars)
  
  if(base::length(overlap) >= 1){
    
    if(FALSE){ # currently not allowed
      
      confuns::give_feedback(msg = "Discarding overlapping variables.", verbose = verbose)
      
      object <- discardStatVariables(object, stat_variables = overlap, verbose = verbose)
      
    } else {
      
      msg <- 
        glue::glue(
          "Data variables of 'input_df' and stat data.frame overlap:", 
          " '{confuns::scollapse(overlap)}' ", 
          "This is not allowed."
        )
      
      confuns::give_feedback(
        msg = msg, 
        fdb.fn = "stop", 
        with.time = FALSE
      )
      
    }
    
  }
  
  updated_stat_df <- 
    confuns::join_safely(
      old.df = old_stat_df, 
      new.df = new_input_df, 
      ref.new.df = "input_df", 
      variable.names = variable_names, 
      valid.classes = "numeric", 
      by = "cell_id",
      overwrite = overwrite, 
      verbose = verbose
    )
  
  object <- setCellDf(object, slot = "stats", df = updated_stat_df, phase = phase)
  
  base::return(object)
  
}


#' @title Add hierarchical clustering results to overall data
#' 
#' @description Adds hierarchical clustering results in form of 
#' grouping variables to the object's overall data - making them available for the
#' \code{across}-argument.. 
#'
#' @inherit argument_dummy
#' @param k Numeric vector. Denotes the exact number of clusters in which the tree created 
#' according to the distance- and agglomeration method is supposed to be cut. 
#' @param h Numeric vector. Denotes the heights at which the hierarchical tree created 
#' according to the distance- and agglomeration method is supposed to be cut. 
#' 
#' @details The last step of the hierarchical clustering pipeline. This function iterates
#' over all combinations of \code{method_dist}, \code{method_aggl}, \code{k} and \code{h} and 
#' adds the respective clustering variables to the object's overall data named according to 
#' the following syntax: \emph{hcl_\code{method_dist}_\code{method_aggl}_k/h_\code{k}/\code{h}_\code{(variable_set)}}.
#' This naming concept results in somewhat bulky but unambiguous clustering names. You can always 
#' rename grouping variables with \code{renameClusterDf()}.
#' 
#' Use \code{getGroupingVariableNames()} afterwards to obtain all grouping variables.
#'
#' @inherit add_family return
#' @export
#'
addHierarchicalClusterVariables <- function(object,
                                            variable_set,
                                            phase = NULL,
                                            method_dist = NULL,
                                            method_aggl = NULL,
                                            k = NULL,
                                            h = NULL, 
                                            verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  cluster_object <- 
    getHclustConv(object, variable_set = variable_set, phase = phase, with_data = FALSE)
  
  new_cluster_df <- 
    confuns::get_hclust_df(
      hcl.obj = cluster_object, 
      methods.dist = method_dist, 
      methods.aggl = method_aggl, 
      k = k, 
      h = h
    ) %>% hlpr_add_vset_suffix(., variable_set = variable_set)
    
  
  new_cluster_names <- 
    dplyr::select(new_cluster_df, -cell_id) %>% 
    base::colnames()
  
  cluster_df <- getClusterDf(object, phase = phase, verbose = FALSE)
  
  existing_cluster_names <- 
    dplyr::select(cluster_df, -cell_id) %>% 
    base::colnames()
  
  if(base::length(existing_cluster_names) >= 1){
    
    new_cluster_names <- 
      confuns::discard_if(
        input = new_cluster_names, 
        one_of = existing_cluster_names, 
        ref.input = "cluster variables to be added", 
        ref.of = "already part of existing cluster variables", 
        v.empty = NULL,
        ref.empty = "Cluster data stays the same",
        verbose = TRUE
      )
    
  }
  
  if(!base::is.null(new_cluster_names)){
    
    cluster_df <-
      dplyr::left_join(x = cluster_df, y = new_cluster_df[, c("cell_id", new_cluster_names)], by = "cell_id")
    
    object <- setCellDf(object, slot = "cluster", df = cluster_df, phase = phase)
    
    msg <- glue::glue("Successfully added {n} cluster {ref_variables}: '{ref_new_cluster_names}'.", 
                      n = base::length(new_cluster_names), 
                      ref_variables = confuns::adapt_reference(new_cluster_names, sg = "variable", pl = "variables"),
                      ref_new_cluster_names = glue::glue_collapse(new_cluster_names, sep = "', '", last = "' and '"))
    
    confuns::give_feedback(msg = msg, verbose = verbose)
    
  }
  
  base::return(object)
  
}


#' @title Add kmeans clustering results to overall data
#' 
#' @description Adds the clustering results of \code{computeKmeansCluster()} in form
#' of grouping variables to the object's overall data - making them available for the \code{across}-
#' argument. 
#'
#' @inherit argument_dummy params
#' @param k Numeric vector. All k-values of interest. 
#' 
#' @details The last step of the kmeans clustering pipeline. This function iterates
#' over all combinations of \code{method_kmeans} and \code{k} and 
#' adds the respective clustering variables to the object's overall data named according to 
#' the following syntax: \emph{kmeans_\code{method_kmeans}_k_\code{k}_\code{(variable_set)}}.
#' This naming concept results in somewhat bulky but unambiguous clustering names. You can always 
#' rename grouping variables with \code{renameClusterDf()}.
#' 
#' Use \code{getGroupingVariableNames()} afterwards to obtain all grouping variables.
#' 
#' @inherit add_family return
#' @export
#'
addKmeansClusterVariables <- function(object,
                                      variable_set,
                                      k,
                                      phase = NULL,
                                      method_kmeans = NULL, 
                                      verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  cluster_object <-
    getKmeansConv(
      object = object,
      phase = phase,
      variable_set = variable_set, 
      with_data = FALSE
      )
  
  new_cluster_df <- 
    confuns::get_kmeans_df(
      kmeans.obj = cluster_object, 
      centers = k, 
      methods.kmeans = method_kmeans, 
      centers.string = "k"
    ) %>% 
    hlpr_add_vset_suffix(variable_set = variable_set)
  
  new_cluster_names <- 
    dplyr::select(new_cluster_df, -cell_id) %>% 
    base::colnames()
  
  cluster_df <- getClusterDf(object, phase = phase, verbose = FALSE)
  
  existing_cluster_names <- 
    dplyr::select(cluster_df, -cell_id) %>% 
    base::colnames()
  
  if(base::length(existing_cluster_names) >= 1){
    
    new_cluster_names <- 
      confuns::discard_if(
        input = new_cluster_names, 
        one_of = existing_cluster_names, 
        ref.input = "cluster variables to be added", 
        ref.of = "already part of existing cluster variables", 
        v.empty = NULL,
        ref.empty = "Cluster data stays the same",
        verbose = TRUE
      )
    
  }
  
  if(!base::is.null(new_cluster_names)){
    
    cluster_df <-
      dplyr::left_join(x = cluster_df, y = new_cluster_df[, c("cell_id", new_cluster_names)], by = "cell_id")
    
    object <- setCellDf(object, slot = "cluster", df = cluster_df, phase = phase)
    
    msg <- glue::glue("Successfully added {n} cluster {ref_variables}: '{ref_new_cluster_names}'.", 
                      n = base::length(new_cluster_names), 
                      ref_variables = confuns::adapt_reference(new_cluster_names, sg = "variable", pl = "variables"),
                      ref_new_cluster_names = glue::glue_collapse(new_cluster_names, sep = "', '", last = "' and '"))
    
    confuns::give_feedback(msg = msg, verbose = verbose)
    
  }
  
  base::return(object)
  
}


#' @title Add PAM clustering results to overall data 
#' 
#' @description Adds the clustering results of \code{computePamClusters()} in form 
#' of grouping variables to the object's overall data - making them available for the \code{across}-
#' argument. 
#'
#' @inherit argument_dummy params 
#' @param k Numeric vector. All k-values of interest. 
#' 
#' @details The last step of the PAM clustering pipeline. This function iterates
#' over all combinations of \code{method_pam} and \code{k} and 
#' adds the respective clustering variables to the object's overall data named according to 
#' the following syntax: \emph{pam_\code{method_pam}_k_\code{k}_\code{(variable_set)}}.
#' This naming concept results in somewhat bulky but unambiguous clustering names. You can always 
#' rename grouping variables with \code{renameClusterDf()}.
#' 
#' Use \code{getGroupingVariableNames()} afterwards to obtain all grouping variables.
#'
#' @inherit add_family return
#' 
#' @export
#'
addPamClusterVariables <- function(object, 
                                   variable_set,
                                   k,
                                   phase = NULL, 
                                   method_pam = NULL,
                                   verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  cluster_object <- getPamConv(object, phase = phase, variable_set = variable_set)
  
  new_cluster_df <-
    confuns::get_pam_df(
      pam.obj = cluster_object, 
      metric.pam = method_pam, 
      k = k
    ) %>% 
    hlpr_add_vset_suffix(variable_set = variable_set)
  
  new_cluster_names <- 
    dplyr::select(new_cluster_df, -cell_id) %>% 
    base::colnames()
  
  cluster_df <- getClusterDf(object, phase = phase, verbose = FALSE)
  
  existing_cluster_names <- 
    dplyr::select(cluster_df, -cell_id) %>% 
    base::colnames()
  
  if(base::length(existing_cluster_names) >= 1){
    
    new_cluster_names <- 
      confuns::discard_if(
        input = new_cluster_names, 
        one_of = existing_cluster_names, 
        ref.input = "cluster variables to be added", 
        ref.of = "already part of existing cluster variables", 
        v.empty = NULL,
        ref.empty = "Grouping data stays the same",
        verbose = TRUE
      )
    
  }
  
  if(!base::is.null(new_cluster_names)){

    cluster_df <-
      dplyr::left_join(x = cluster_df, y = new_cluster_df[, c("cell_id", new_cluster_names)], by = "cell_id")
    
    object <- setCellDf(object, slot = "cluster", df = cluster_df, phase = phase)
    
    msg <- glue::glue("Successfully added {n} cluster {ref_variables}: '{ref_new_cluster_names}'.", 
                      n = base::length(new_cluster_names), 
                      ref_variables = confuns::adapt_reference(new_cluster_names, sg = "variable", pl = "variables"),
                      ref_new_cluster_names = glue::glue_collapse(new_cluster_names, sep = "', '", last = "' and '"))
    
    confuns::give_feedback(msg = msg, verbose = verbose)
    
  }
  
  base::return(object)
  
}











# a -----------------------------------------------------------------------



# h -----------------------------------------------------------------------

#' @title Add helping text 
#'
add_helper <- function(shiny_tag, content, title = "What do I have to do here?", type = "inline", size = "s", ...){
  
  
  res <- 
    shinyhelper::helper(shiny_tag = shiny_tag, 
                        content = content, 
                        title = title, 
                        size = size,
                        type = type, 
                        ...)
  
  base::return(res)
  
}


# r -----------------------------------------------------------------------

add_roi_info_vars <- function(df, vars){
  
  attr <- base::attributes(df)
  
  attr$roi_info_vars <- c(attr$roi_info_vars, vars)
  
  base::attributes(df) <- attr
  
  return(df)
  
}



# A -----------------------------------------------------------------------


# W -----------------------------------------------------------------------

#' @title Add a well plate 
#' 
#' @description Creates and adds a new object of class \code{WellPlate} in the respective
#' slot of input for argument \code{object}.
#'
#' @inherit argument_dummy params
#' @param name,layout Input for the \code{WellPlate} object as described in documentation of class \code{WellPlate}.
#' See details for more information. 
#' 
#' @param ... Additional arguments given to \code{methods::new(Class = \emph{'WellPlate'}, ...)}.
#' 
#' @details Slot @@index of the new well plate is automatically defined by adding 1 to the sum of all well plates found.
#' Slot @@experiment of the new well plate is inherited by the input for argument \code{object}. Slot @@type
#' is defined by the attribute \emph{well_plate_type} of the input for argument \code{layout} (which has to be a \code{layout_df}).
#' 
#' Input for slot @@name must not be already used by another well plate.
#' @return The input object.
#' 
#' @seealso \code{WellPlate-class}
#' 
#' @export
#'
setGeneric(name = "addWellPlate", def = function(object, ...){
  
  standardGeneric(f = "addWellPlate")
  
})


#' @rdname addWellPlate
#' @export
setMethod(f = "addWellPlate", signature = "ExperimentDesign", function(object, name, layout, ... ){
  
  n_wps <- nWellPlates(object)
  
  if(n_wps != 0){
    
    confuns::check_none_of(
      input = name, 
      against = getWellPlateNames(object), 
      ref.against = "well plate names"
    )
    
  }
  
  well_plate <- 
    methods::new(
      Class = "WellPlate", 
      experiment = object@experiment, 
      name = name,
      layout = layout,
      index = n_wps + 1,
      type = getWellPlateType(layout),
      ...
    )
  
  object@well_plates[[name]] <- well_plate
  
  return(object)
  
})

