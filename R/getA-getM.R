#' @include S4-classes.R
NULL

#' @include S4-method-skeletons.R
NULL



# A -----------------------------------------------------------------------

#' @title Obtain names of active modules
#' 
#' @description Extracts the names of the modules for which variables
#' have been assigned during \code{assignVariables()}.
#'
#' @inherit argument_dummy params
#'
#' @return A character vector or NULL if no modules are active. 
#' @export
#'
setGeneric(name = "getActiveModuleNames", def = function(object){
  
  standardGeneric(f = "getActiveModuleNames")
  
})


#' @rdname getUsedModuleNames
#' @export
setMethod(f = "getActiveModuleNames", signature = "Cypro", definition = function(object){
  
  purrr::keep(.x = object@modules, .p = ~ base::isTRUE(.x@active)) %>% 
    base::names()
  
})

#' @title Obtain additional variable names
#' 
#' @description Extracts variable names that have been denoted during 
#' \code{assignVariables()} as unknown to \code{cypro} but of interest
#' to the user. 
#'
#' @inherit argument_dummy params 
#' @param with_grouping,with_numeric Logical values. Indicate the additional 
#' variable types that are supposed to be included in the output list. 
#'
#' @return A list.
#' 
#' @export
#'
setGeneric(name = "getAdditionalVariableNames", def = function(object, with_grouping = TRUE, with_numeric = TRUE){
  
  standardGeneric(f = "getAdditionalVariableNames")
  
})


#' @rdname getAdditionalVariableNames
#' @export
setMethod(
  f = "getAdditionalVariableNames",
  signature = "Cypro",
  definition = function(object, with_grouping = TRUE, with_numeric = TRUE){
  
  exp_design <- getExperimentDesign(object)
  
  out <- list()
  
  if(base::isTRUE(with_grouping)){
    
    out$grouping <- exp_design@variables_grouping
    
  }
  
  if(base::isTRUE(with_numeric)){
    
    out$numeric <- exp_design@variables_numeric
    
  }
  
  return(out)
  
})


# C -----------------------------------------------------------------------


#' @title Extract cell IDs
#' 
#' @description Obtain cell IDs in form of a character vector. 
#' Using the ... options the data can be subsetted in the style of \code{dplyr::filter()}. 
#'
#' @inherit argument_dummy params 
#' @inherit dplyr::filter params
#'
#' @return Character vector of cell ids.
#' 
#' @details Cell IDs are extracted from the feature data.frame in case 
#' of \code{CyproScreening} objects and from the stats data.frame in 
#' case of \code{CyproTimeLapse(MP)} objects.
#' 
#' @export
#'

setGeneric(name = "getCellIds", def = function(object, ...){
  
  standardGeneric(f = "getCellIds")
  
})

#' @rdname getCellIds
#' @export
setMethod(f = "getCellIds", signature = "CyproScreening", definition = function(object, ...){
  
  filtering <- rlang::enquos(...)
  
  cell_ids <- 
    getFeatureDf(object = object, with_grouping = TRUE) %>% 
    dplyr::filter(!!!filtering) %>% 
    dplyr::pull(cell_id) %>% 
    base::unique()
  
  return(cell_ids)
  
})

#' @rdname getCellIds
#' @export
setMethod(f = "getCellIds", signature = "CyproTimeLapse", definition = function(object, ...){
  
  filtering <- rlang::enquos(...)
  
  cell_ids <- 
    getStatsDf(object = object, with_grouping = TRUE) %>% 
    dplyr::filter(!!!filtering) %>% 
    dplyr::pull(cell_id) %>% 
    base::unique()
  
  return(cell_ids)
  
})

#' @rdname getCellIds
#' @export
setMethod(f = "getCellIds", signature = "CyproTimeLapseMP", definition = function(object, phase = NULL, ...){
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  filtering <- rlang::enquos(...)
  
  cell_ids <- 
    getStatsDf(object = object, phase = phase, with_grouping = TRUE) %>% 
    dplyr::filter(!!!filtering) %>% 
    dplyr::pull(cell_id) %>% 
    base::unique()
  
  return(cell_ids)
  
})


#' @title Extract cell line and condition names 
#' 
#' @description Obtain names of the \emph{cell_line}- or the \emph{condition}-variable
#' in form of a character vector..
#'
#' @inherit argument_dummy params 
#'
#' @return Character vector.
#' @export
#'

setGeneric(name = "getCellLines", def = function(object){
  
  standardGeneric(f = "getCellLines")
  
})

#' @rdname getCellLines
#' @export
setMethod(f = "getCellLines", signature = "Cypro", function(object){
  
  check_object(object)
  
  cell_lines <- 
    getMetaDf(object) %>% 
    pull(cell_line) %>% 
    base::levels()
  
  return(cell_lines)
  
})

#' @rdname getCellLines
#' @export
#' 

setGeneric(name = "getConditions", def = function(object, ...){
  
  standardGeneric(f = "getConditions")
  
})

#' @rdname getCellLines
#' @export
setMethod(f = "getConditions", signature = "Cypro", definition = function(object){
  
  check_object(object)
  
  getMetaDf(object) %>% 
    dplyr::pull(condition) %>% 
    base::levels()
  
})

#' @rdname getCellLines
#' @export
setMethod(f = "getConditions", signature = "CyproTimeLapseMP", definition = function(object, phase = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  getMetaDf(object, phase = phase) %>% 
    dplyr::pull(condition) %>% 
    base::levels()
  
})


#' @title Extract cell data
#' 
#' @description Obtain cell data in form of an S4-class called \code{Cdata}.
#'
#' @inherit argument_dummy params
#' 
#' @return An object of class \code{Cdata}.
#' 

setGeneric(name = "getCdata", def = function(object){
  
  standardGeneric(f = "getCdata")
  
})

#' @rdname getCdata
#' @export
setMethod(f = "getCdata", signature = "Cypro", definition = function(object){
  
  check_object(object)
  
  return(object@cdata)
  
})


#' @title Obtain cluster data (by cell id)
#' 
#' @description Obtain a data.frame that contains variables 
#' with which cells are grouped according to clustering results. 
#'
#' @inherit argument_dummy params
#' 
#' @return A data.frame.
#' @export
#'

setGeneric(name = "getClusterDf", def = function(object, ...){
  
  standardGeneric(f = "getClusterDf")
  
})

#' @rdname getClusterDf
#' @export
setMethod(f = "getClusterDf",
          signature = "Cdata",
          definition = function(object, ...){
  
  object@cluster
  
})

#' @rdname getClusterDf
#' @export
setMethod(f = "getClusterDf",
          signature = "CdataTimeLapseMP",
          definition = function(object, phase, ...){
  
  confuns::check_one_of(
    input = phase, 
    against = base::names(object@cluster)
  )
  
  return(object@cluster[[phase]])
  
})

#' @rdname getClusterDf
#' @export
setMethod(f = "getClusterDf",
          signature = "Cypro",
          definition = function(object, verbose = NULL, ...){
            
            assign_default(object = object)
            
            
            cluster_df <- 
              getCdata(object) %>% 
              getClusterDf()
            
            if(base::all(base::colnames(cluster_df) == "cell_id")){
              
              confuns::give_feedback(
                msg = "No cluster variables have been added yet.",
                verbose = verbose, 
                with.time = FALSE
              )
              
            }
            
            return(cluster_df)
            
          })

#' @rdname getClusterDf
#' @export
setMethod(f = "getClusterDf",
          signature = "CyproTimeLapseMP",
          definition = function(object, phase = NULL, verbose = NULL, ...){
            
            assign_default(object = object)
            
            phase <- check_phase(object, phase = phase, max_phases = 1)
            
            cluster_df <- 
              getCdata(object) %>% 
              getClusterDf(phase = phase)
            
            if(base::all(base::colnames(cluster_df) == "cell_id")){
              
              confuns::give_feedback(
                msg = glue::glue("No cluster variables have been added yet for {phase} phase."),
                verbose = verbose, 
                with.time = FALSE
              )
              
            }
            
            return(cluster_df)
            
          })



#' @title Extract cluster variable names of cell data
#' 
#' @description Obtain the names of the cluster variables of your cell data. Useful to 
#' obtain vectors of variable names as input for recurring arguments like \code{across} or
#' \code{grouping_variable}.
#'
#' @inherit argument_dummy params
#' @param named Logical value. If set to TRUE the grouping variables are named 
#' according to their grouping type (cluster, meta or well_plate).
#' @param ... Additional selection helpers from the \code{tidyselect} package that match 
#' variable names according to a given pattern. 
#' 
#' @return A character vector. 
#' 
#' @seealso starts_with(), ends_with(), contains(), matches()
#' 
#' @export

setGeneric(name = "getClusterVariableNames", def = function(object, ...){
  
  standardGeneric(f = "getClusterVariableNames")
  
})

#' @rdname getClusterVariableNames
#' @export
setMethod(
  f = "getClusterVariableNames",
  signature = "Cypro",
  definition = function(object, ..., verbose = NULL){
    
    check_object(object)
    
    assign_default(object)
    
    res <- 
      getClusterDf(object, verbose = TRUE) %>% 
      dplyr::select(-cell_id) %>% 
      dplyr::select(...) %>% 
      base::colnames()
    
    return(res)
    
  }
)

#' @rdname getClusterVariableNames
#' @export
setMethod(
  f = "getClusterVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, ..., named = FALSE, phase = NULL, verbose = NULL){
    
    check_object(object)
    
    assign_default(object)
    
    phase <- check_phase(object, phase = phase, max_phases = 1)

    res <- 
      getClusterDf(object, verbose = TRUE, phase = phase) %>% 
      dplyr::select(-cell_id) %>% 
      dplyr::select(...) %>% 
      base::colnames()
    
    return(res)
    
  }
)





# D -----------------------------------------------------------------------

#' @title Extract the object's default instructions
#' 
#' @description Obtain what is currently defined as the default in form of 
#' a list or the original S4-object. 
#'
#' @inherit argument_dummy params
#' @param return Character value. Either \emph{'list'} (default) or \emph{'S4'}.
#'
#' @return A list or an S4 object.
#' @export
#'

setGeneric(name = "getDefaultInstructions", def = function(object, ...){
  
  standardGeneric(f = "getDefaultInstructions")
  
})

#' @rdname getDefaultInstructions
#' @export

setMethod(f = "getDefaultInstructions", signature = "Cypro", definition = function(object, return = "list"){
  
  check_object(object)
  
  return(object@default)
  
})


# E -----------------------------------------------------------------------


#' @title Obtain example data.frame
#' 
#' @description Extracts the data.frame that has been loaded as an 
#' input example during \code{assingVariables()}.
#' 
#' @inherit argument_dummy params
#' 
#' @return A data.frame.
#' 
#' @export
#' 
setGeneric(name = "getExampleDf", def = function(object){
  
  standardGeneric(f = "getExampleDf")
  
})

#' @rdname getExampleDf
#' @export
setMethod(f = "getExampleDf", signature = "ExperimentDesign", definition = function(object){
  
  df <- object@example_df
  
  if(base::nrow(df) == 0){
    
    warning("Data.frame of slot @example_df contains 0 rows.")
    
  }
  
  return(df)
  
})

#' @rdname getExampleDf
#' @export
setMethod(f = "getExampleDf", signature = "Cypro", definition = function(object){
  
  df <- 
    getExperimentDesign(object) %>% 
    getExampleDf()
  
  return(df)
  
})


#' @title Extract experiment design
#' 
#' @description Obtain the experiment design in form of an S4-class called 
#' \code{ExperimentDesign.}
#'
#' @inherit argument_dummy params
#'
#' @return An object of class \code{ExperimentDesign}
#' @export
#'

setGeneric(name = "getExperimentDesign", def = function(object, ...){
  
  standardGeneric(f = "getExperimentDesign")
  
})

#' @rdname getExperimentDesign
#' @export
setMethod(f = "getExperimentDesign", signature = "Cypro", definition = function(object){
  
  object@experiment_design
  
})




# F -----------------------------------------------------------------------


#' @title Extract numeric cell data
#' 
#' @description Obtain numeric cell features of screening experiments in 
#' form of a data.frame. 
#' 
#' @inherit argument_dummy params
#' 
#' @seealso \code{getTracksDf()} and \code{getStatsDf()} to obtain numeric data
#' of \code{Cypro} objects from time lapse experiments.
#' 
#' @return A data.frame.
#' 
#' @seealso \code{getTracksDf()}, \code{getStatsDf()}
#' 
#' @export
#'

setGeneric(name = "getFeatureDf", def = function(object, ...){
  
  standardGeneric(f = "getFeatureDf")
  
})

#' @rdname getFeatureDf
#' @export
setMethod(f = "getFeatureDf", signature = "CdataScreening", definition = function(object, ...){
  
  return(object@features)
  
})

#' @rdname getFeatureDf
#' @export
setMethod(
  f = "getFeatureDf",
  signature = "CyproScreening", 
  definition = function(object,
                        drop_na = FALSE,
                        with_grouping = NULL,
                        with_cluster = NULL,
                        with_meta = NULL,
                        with_well_plate = NULL,
                        verbose = NULL,
                        ...){
    
    assign_default(object)
    
    if(base::isFALSE(with_grouping)){
      
      with_cluster <- FALSE
      with_meta <- FALSE
      with_well_plate <- FALSE
      
    }
    
    df <- 
      getCdata(object) %>% 
      getFeatureDf()
    
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
    
    return(df)  
    
  }
)

#' @rdname getFeatureDf
#' @export
setMethod(
  f = "getFeatureDf", 
  signature = "CyproTimeLapse", 
  definition = function(object, 
                        slot = "stats", 
                        drop_na = FALSE,
                        with_grouping = NULL, 
                        with_cluster = NULL, 
                        with_meta = NULL, 
                        with_well_plate = NULL, 
                        verbose = NULL
  ){
    
    check_object(object)
    assign_default(object)
    
    confuns::is_value(x = slot, mode = "character")
    
    confuns::check_one_of(
      input = slot, 
      against = c("stats", "tracks")
    )
    
    if(slot == "tracks"){
      
      df <- 
        getTracksDf(
          object = object, 
          with_grouping = with_grouping, 
          with_cluster = with_cluster, 
          with_meta = with_meta, 
          with_well_plate = with_well_plate, 
          verbose = verbose, 
          drop_na = drop_na
        )
      
    } else if(slott == "stats"){
      
      df <- 
        getStatsDf(
          object = object, 
          with_grouping = with_grouping, 
          with_cluster = with_cluster, 
          with_meta = with_meta, 
          with_well_plate = with_well_plate, 
          verbose = verbose, 
          drop_na = drop_na
        )
      
    }
    
    return(df)
    
  }
)

#' @rdname getFeatureDf
#' @export
setMethod(
  f = "getFeatureDf", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, 
                        slot = "stats", 
                        drop_na = FALSE,
                        with_grouping = NULL, 
                        with_cluster = NULL, 
                        with_meta = NULL, 
                        with_well_plate = NULL, 
                        phase = NULL, 
                        verbose = NULL
  ){
    
    check_object(object)
    assign_default(object)
    
    confuns::is_value(x = slot, mode = "character")
    
    confuns::check_one_of(
      input = slot, 
      against = c("stats", "tracks")
    )
    
    if(slot == "tracks"){
      
      df <- 
        getTracksDf(
          object = object, 
          with_grouping = with_grouping, 
          with_cluster = with_cluster, 
          with_meta = with_meta, 
          with_well_plate = with_well_plate, 
          phase = phase,
          verbose = verbose, 
          drop_na = drop_na
        )
      
    } else if(slott == "stats"){
      
      df <- 
        getStatsDf(
          object = object, 
          with_grouping = with_grouping, 
          with_cluster = with_cluster, 
          with_meta = with_meta, 
          with_well_plate = with_well_plate, 
          phase = phase, 
          verbose = verbose, 
          drop_na = drop_na
        )
      
    }
    
    return(df)
    
  }
)

#' @title Extract a feature set  
#' 
#' @description Obtain defined sets of numeric data variables in form of vectors. 
#' Useful to conveniently obtain vectors for recurring arguments like 
#' \code{variables}.
#'
#' @inherit argument_dummy params
#'
#' @return A character vector. 
#' @export
#'

setGeneric(name = "getFeatureSet", def = function(object, feature_set){
  
  standardGeneric(f = "getFeatureSet")
  
})

#' @rdname getFeatureSet 
#' @export 
setMethod(f = "getFeatureSet", signature = "Cypro", definition = function(object){
  
  if(base::length(base::names(object@feature_sets)) == 0){
    
    stop("No variable sets have been defined yet.")
    
  }
  
  confuns::check_one_of(
    input = feature_set, 
    against = base::names(object@feature_sets), 
    fdb.opt = 2, 
    ref.opt.2 = "defined variable sets"
  )
  
  fset <- object@feature_sets[[feature_set]]
  
  return(fset)
  
})

#' @title Obtain defined sets of data variables 
#' 
#' @description Obtain defined sets of data variables in form of a list of 
#' character vectors.
#' 
#' @inherit argument_dummy params
#' 
#' @return A list of character vectors.
#' 
#' @export

setGeneric(name = "getFeatureSets", def = function(object){
  
  standardGeneric(f = "getFeatureSets")
  
})

#' @rdname getFeatureSet 
#' @export 
setMethod(f = "getFeatureSets", signature = "Cypro", definition = function(object){
  
  return(object@feature_sets)
  
})

#' @title Extract names of defined sets of data variables 
#' 
#' @description Obtain the names off all currently defined feature sets.
#' 
#' @inherit argument_dummy params
#' 
#' @return A character vector.
#' 
#' @export

setGeneric(name = "getFeatureSetNames", def = function(object){
  
  standardGeneric(f = "getFeatureSetNames")
  
})

#' @rdname getFeatureSet 
#' @export 
setMethod(f = "getFeatureSetNames", signature = "Cypro", definition = function(object){
  
  return(base::names(object@feature_sets))
  
})



#' @title Obtain file directories
#' 
#' @description Extracts the directories that were used to load the cell data. 
#'
#' @inherit argument_dummy params
#'
#' @return Character vector
#' @export
#'
setGeneric(name = "getFileDirectories", def = function(object, ...){
  
  standardGeneric(f = "getFileDirectories")
  
})



#' @rdname getFileDirectories
#' @export

setMethod(f = "getFileDirectories", signature = "layout_df", function(object, ...){
  
  df <- unnestLayoutDf(object)
  
  dirs <- df$dir
  
  base::names(dirs) <- df$well_roi
  
  return(dirs)
  
})

#' @rdname getFileDirectories
#' @export

setMethod(f = "getFileDirectories", signature = "WellPlate", function(object){
  
  
  
})


#' @title Extract frame time sequence
#' 
#' @description Obtain the time line of the time lapse experiment as a numeric vector.
#'
#' @inherit argument_dummy params 
#'
#' @return A numeric vector.
#' @export
#'

setGeneric(name = "getFrameTimeSeq", def = function(object, ...){
  
  standardGeneric(f = "getFrameTimeSeq")
  
})

#' @rdname getFrameTimeSeq
#' @export
setMethod(f = "getFrameTimeSeq", signature = "CyproTimeLapse", definition = get_frame_time_seq)

#' @rdname getFrameTimeSeq
#' @export
setMethod(f = "getFrameTimeSeq", signature = "CyproTimeLapseMP", definition = get_frame_time_seq_mp)



# G -----------------------------------------------------------------------

#' @title Extract cell grouping data 
#' 
#' @description Obtain a data.frame that contains variables that group 
#' cells.
#'
#' @inherit argument_dummy params
#' 
#' @return A data.frame.
#' @export
#'

setGeneric(name = "getGroupingDf", def = function(object, ...){
  
  standardGeneric(f = "getGroupingDf")
  
})

#' @rdname getGroupingDf
#' @export
setMethod(
  f = "getGroupingDf",
  signature = "Cypro",
  definition = function(object,
                        verbose = NULL,
                        ...){
    
    group_df <- 
      dplyr::left_join(
        x = getWellPlateDf(object), 
        y = getMetaDf(object), 
        by = "cell_id"
      ) %>% 
      dplyr::left_join(
        x = ., 
        y = getClusterDf(object, verbose = verbose), 
        by = "cell_id"
      )
    
    return(group_df)
    
  }
)

#' @rdname getGroupingDf
#' @export
setMethod(
  f = "getGroupingDf",
  signature = "CyproTimeLapseMP",
  definition = function(object,
                        phase = NULL,
                        verbose = NULL,
                        ...){
    
    assign_default(object)
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    df <- 
      dplyr::left_join(
        x = getWellPlateDf(object), 
        y = getMetaDf(object, phase = phase), 
        by = "cell_id"
      ) %>% 
      dplyr::left_join(
        x = ., 
        y = getClusterDf(object, phase = phase, verbose = verbose)
      )
    
    return(df)
    
  }
)

#' @title Extract group names a grouping variable contains
#' 
#' @description Obtain the names of the groups in which a specific grouping
#' variable groups the cells. Useful to obtain input options for arguments like \code{across_subset}. 
#'
#' @inherit argument_dummy params
#'
#' @return A character vector. 
#' 
#' @export

setGeneric(name = "getGroupNames", def = function(object, ...){
  
  standardGeneric(f = "getGroupNames")
  
})

#' @rdname getGroupNames
#' @export
setMethod(f = "getGroupNames", signature = "Cypro", definition = get_group_names)

#' @rdname getGroupNames
#' @export
setMethod(f = "getGroupNames", signature = "CyproTimeLapseMP", definition = get_group_names_mp)


#' @title Extract grouping variable names of cell data
#' 
#' @description Obtain the names of variables that group cells. Useful to obtain 
#' valid input options for recurring arguments like \code{across} or \code{grouping_variable}.
#' 
#' @inherit getClusterVariableNames description params
#' 
#' @return A character vector. 
#' 
#' @seealso starts_with(), ends_with(), contains(), matches()
#' 
#' @export

setGeneric(name = "getGroupingVariableNames", def = function(object, ...){
  
  standardGeneric(f = "getGroupingVariableNames")
  
})

#' @rdname getGroupingVariableNames
#' @export
setMethod(
  f = "getGroupingVariableNames",
  signature = "Cypro",
  definition = function(object, ..., named = FALSE, verbose = NULL){
    
    check_object(object)
    
    assign_default(object)
    
    group_df <- 
      getGroupingDf(object, verbose = verbose) %>% 
      dplyr::select(-cell_id)
    
    res <- 
      get_grouping_variable_names(
        group_df = group_df, 
        named = named, 
        verbose = verbose, 
      ) %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  }
)


#' @rdname getGroupingVariableNames
#' @export
setMethod(
  f = "getGroupingVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, ..., named = FALSE, phase = NULL, verbose = NULL){
    
    check_object(object)
    
    assign_default(object)
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    group_df <- 
      getGroupingDf(object, verbose = verbose, phase = phase) %>% 
      dplyr::select(-cell_id)
    
    res <- 
      get_grouping_variable_names(
        group_df = group_df, 
        named = named, 
        verbose = verbose, 
        ...
      ) %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  }
)




# I -----------------------------------------------------------------------



#' @title Obtain variable assignment 
#' 
#' @inherit getVariableAssignment params description
#' 
#' @return A character vector. 
#' 
#' @export
setGeneric(name = "getVariableAssignmentID", def = function(object, drop_na = FALSE){
  
  standardGeneric(f = "getVariableAssignmentID")
  
})


#' @rdname getVariableAssignmentID
#' @export
setMethod(
  f = "getVariableAssignmentID", 
  signature = "CyproScreening", 
  definition = function(object, drop_na = FALSE){
    
    res <- 
      getVariableAssignment(
        object = object,
        modules = "identification",
        flatten = TRUE,
        drop_na = drop_na
        )
    
    return(res)
    
  }
)


#' @rdname getVariableAssignmentID
#' @export
setMethod(
  f = "getVariableAssignmentID", 
  signature = "CyproTimeLapse", 
  definition = function(object, drop_na = FALSE){
    
    res <- 
      getVariableAssignment(
        object = object,
        modules = "identification_timelapse",
        flatten = TRUE,
        drop_na = drop_na
        )
    
    return(res)
    
  }
)


# L -----------------------------------------------------------------------


#' @title Obtain layout attributes
#' 
#' @description Extracts the well plate layout specific attributes
#' from the input data.frame.
#' 
#' @param df A data.frame of class \code{layout_df}.
#' 
#' @return A named list. 
#' 
#' @export

getLayoutAttributes<- function(df){
  
  base::attributes(df)[layout_df_attributes] %>% 
    purrr::discard(.p = ~ base::is.null(.x))
  
}


#' @title Extract well plate layout data.frame
#' 
#' @description Obtain the well plate layout in form of a data.frame.
#' 
#' @inherit argument_dummy params
#'
#' @return A \code{layout_df} - data.frame.
#' @export
#'

setGeneric(name = "getLayoutDf", def = function(object, ...){
  
  standardGeneric(f = "getLayoutDf")
  
})

#' @rdname getLayoutDf
#' @export
setMethod(f = "getLayoutDf", signature = "WellPlate", function(object, ...){
  
  object@layout
  
})

#' @rdname getLayoutDf
#' @export
setMethod(f = "getLayoutDf", signature = "ExperimentDesign", function(object, well_plate, ...){
  
  confuns::check_one_of(
    input = well_plate, 
    against = base::names(object@well_plates)
  )
  
  layout_df <- 
    getWellPlate(object, well_plate = well_plate) %>%
    getLayoutDf()
  
  return(layout_df)
  
})

#' @rdname getLayoutDf
#' @export
setMethod(f = "getLayoutDf", signature = "Cypro", function(object, well_plate, ...){
  
  confuns::check_one_of(
    input = well_plate, 
    against = getWellPlateNames(object)
  )
  
  exp_design <- getExperimentDesign(object)
  
  layout_df <- 
    getWellPlate(exp_design, well_plate = well_plate) %>% 
    getLayoutDf()
  
  return(layout_df)
  
  
})




# M -----------------------------------------------------------------------


#' @title Extract cell meta data 
#' 
#' @description Obtain cell meta data in form of a data.frame that contains
#' data variables with which cells are grouped. 
#'
#' @inherit argument_dummy params
#' 
#' @return A data.frame.
#' 
#' @seealso \code{getClusterDf()}, \code{getWellPlateDf()}
#' 
#' @export

setGeneric(name = "getMetaDf", def = function(object, ...){
  
  standardGeneric(f = "getMetaDf")
  
})

#' @rdname getMetaDf
#' @export
setMethod(f = "getMetaDf", signature = "Cypro", definition = function(object){
  
  check_object(object)
  
  cdata_object <- getCdata(object)
  
  meta_df <- getMetaDf(cdata_object)
  
  return(meta_df)
  
})

#' @rdname getMetaDf
#' @export
setMethod(f = "getMetaDf", signature = "CyproTimeLapseMP", definition = function(object, phase = NULL){
  
  assign_default(object = object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cdata_object <- getCdata(object)
  
  meta_df <- getMetaDf(cdata_object, phase = phase)
  
  return(meta_df)
  
})

#' @rdname getMetaDf
#' @export
setMethod(f = "getMetaDf", signature = "Cdata", definition = function(object, ...){
  
  meta_df <- object@meta
  wp_df <- object@well_plate
  
  df <- dplyr::left_join(x = meta_df, y = wp_df, by = "cell_id")
  
  return(df)
  
})

#' @rdname getMetaDf
#' @export
setMethod(f = "getMetaDf", signature = "CdataTimeLapseMP", definition = function(object, phase, ...){
  
  confuns::check_one_of(
    input = phase, 
    against = base::names(object@meta)
  )
  
  meta_df <- object@meta[[phase]]
  wp_df <- object@well_plate
  
  df <- dplyr::left_join(x = meta_df, y = wp_df, by = "cell_id")
  
  return(df)
  
})


#' @title Extract meta variable names of cell data
#' 
#' @description Obtain the names of variables in the meta data.frame. Useful to obtain 
#' valid input options for recurring arguments like \code{across} or \code{grouping_variable}.
#' 
#' @return A character vector. 
#' 
#' @seealso starts_with(), ends_with(), contains(), matches()
#' 
#' @export

setGeneric(name = "getMetaVariableNames", def = function(object, ...){
  
  standardGeneric(f = "getMetaVariableNames")
  
})

#' @rdname getMetaVariableNames
#' @export
setMethod(
  f = "getMetaVariableNames",
  signature = "Cypro",
  definition = function(object, ..., named = FALSE, verbose = NULL){
    
    check_object(object)
    
    assign_default(object)
    
    res <- 
      getMetaDf(object) %>% 
      dplyr::select(-cell_id) %>% 
      base::colnames() %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  }
)


#' @rdname getMetaVariableNames
#' @export
setMethod(
  f = "getMetaVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, ..., named = FALSE, phase = NULL, verbose = NULL){
    
    check_object(object)
    
    assign_default(object)
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    res <- 
      getMetaDf(object, verbose = TRUE, phase = phase) %>% 
      dplyr::select(-cell_id) %>% 
      base::colnames() %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  }
)




#' @title Obtain analysis module
#' 
#' @description Extracts the complete S4-object of class \code{AnalysisModule} from 
#' the \code{Cypro} object with all its content. 
#' 
#' @inherit argument_dummy params
#' @param module_name Character value. The name of the analysis module. 
#' 
#' @return An object of class \code{AnalysisModule}.
#' 
#' @export
#' 
setGeneric(name = "getModule", def = function(object, module_name, ...){
  
  standardGeneric(f = "getModule")
  
})


#' @rdname getAnalysisModule
#' @export
setMethod(f = "getModule", signature = "Cypro", definition = function(object, module_name, ...){
  
  confuns::check_one_of(
    input = module_name, 
    against = base::names(object@modules), 
    fdb.opt = 2, 
    ref.opt.2 = "names of analysis modules", 
  )
  
  return(object@modules[[module_name]])
  
})





