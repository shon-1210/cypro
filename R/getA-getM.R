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


#' @title Obtain cell IDs
#' 
#' @description Extracts cell IDs in form of a character vector. 
#'
#' @inherit argument_dummy params 
#' @inherit dplyr::filter params
#'
#' @return A character vector.
#' 
#' @export
#'

setGeneric(name = "getCellIDs", def = function(object){
  
  standardGeneric(f = "getCellIDs")
  
})

#' @rdname getCellIds
#' @export
setMethod(f = "getCellIDs", signature = "CyproScreening", definition = function(object){
  
  cell_ids <- object@information$cell_ids
  
  if(base::is.null(cell_ids)){
    
    stop("No cell IDs set.")
    
  }
  
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
    dplyr::pull(cell_line) %>% 
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
    
    phase <- check_phase(object, phase = phase, max_phases = 1)

    res <- 
      getClusterDf(object, verbose = TRUE, phase = phase) %>% 
      dplyr::select(-cell_id) %>%
      dplyr::select() %>% 
      base::colnames() 
    
    return(res)
    
  }
)





# D -----------------------------------------------------------------------

#' @title Obtain loaded file as data.frame
#' 
#' @description Extracts the loaded content of slot @@content, 
#' converts it into a data.frame and adds the missing identification
#' variables depending on the loading modality. 
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame.
#' @export
#'
setGeneric(name = "getDataFileDf", def = function(object){
  
  standardGeneric(f = "getDataFileDf")
  
})


#' @rdname getDataFileDf
#' @export
setMethod(f = "getDataFileDf", signature = "DataFile", definition = function(object){
  
  df <- 
    purrr::discard(.x = object@content, .p = ~isOfClass(.x, "glue")) %>% 
    tibble::as_tibble()
  
  nr <- base::nrow(df)
  
  # extract id info  
  well_plate_name <- base::rep(object@well_plate, nr)
  
  ldm <- object@loading_modality
  
  if(ldm == "by_roi"){
    
    well <- 
      extractWellInfo(vec = object@name, cypro_nc = TRUE) %>% 
      base::rep(nr)
    
    roi <- 
      extractRoiInfo(vec = object@name, cypro_nc = TRUE) %>% 
      base::rep(nr)
    
    id_df <- 
      tibble::tibble(
        well_plate_name = well_plate_name, 
        well = well, 
        roi = roi
      )
    
  } else if(ldm == "by_well"){
    
    well <- 
      extractWellInfo(vec = object@name, cypro_nc = TRUE) %>% 
      base::rep(nr)
    
    id_df <- 
      tibble::tibble(
        well_plate_name = well_plate_name, 
        well = well
      )
    
  } else if(ldm == "by_well_plate"){
    
    id_df <- tibble::tibble(well_plate_name = well_plate_name)
    
  } 
  
  if(ldm == "by_file"){
    
    out_df <- df
    
  } else {
    
    out_df <- base::cbind(id_df, df) 
    
  }
  
  return(out_df)
  
})

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

#' @title Obtain loading error messages
#' 
#' @description Extracts the error messages that were thrown
#' during data loading. 
#' 
#' @inherit argument_dummy params
#' 
#' @return A list of \code{glue} objects or NULL if no errors were thrown.
#' 
#' @export
#' 
setGeneric(name = "getErrors", def = function(object, ...){
  
  standardGeneric(f = "getErrors")
  
})

#' @rdname getErrors
#' @export
setMethod(f = "getErrors", signature = "DataFile", definition = function(object){
  
  out <- purrr::keep(object@content, .p = ~ isOfClass(x = .x, valid_class = "glue"))
  
  if(isOfLength(out, l = 0)){
    
    out <- NULL
    
  } 
  
  return(out)
  
})

#' @rdname getErrors
#' @export
setMethod(f = "getErrors", signature = "Cypro", definition = function(object, well_plates = NULL){
  
  out <- 
    purrr::map(.x = getWellPlates(object, well_plates = well_plates), .f = ~ purrr::keep(.x = .x@files, .p = containsErrors)) %>% 
    purrr::map(.x = ., .f = ~ purrr::set_names(x = .x, nm = purrr::map_chr(.x = .x, .f = ~.x@directory))) %>% 
    purrr::map(.f = ~ purrr::map(.x = .x, .f = ~ getErrors(.x) %>% purrr::discard(.p = base::is.null))) %>% 
    purrr::keep(.p = ~ base::length(x = .x) >= 1)
  
  if(isOfLength(x = out, l = 0)){
    
    out <- NULL
    
  }
  
  return(out)
  
})

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
setMethod(
  f = "getFeatureDf",
  signature = "CdataScreening",
  definition = function(object, 
                        with_cluster = FALSE,
                        with_meta = FALSE,
                        with_well_plate = FALSE,
                        ...){
  
  df <- 
    joinWith(
      object = object, 
      df = object@features, 
      with_cluster = with_cluster, 
      with_meta = with_meta, 
      with_well_plate = with_well_plate, 
      ...
    )
  
  return(df)
  
})

#' @rdname getFeatureDf
#' @export
setMethod(
  f = "getFeatureDf",
  signature = "CyproScreening", 
  definition = function(object,
                        with_cluster = NULL,
                        with_meta = NULL,
                        with_well_plate = NULL,
                        ...){
    
    cdata <- getCdata(object)
    
    df <- 
      joinWith(
        object = cdata, 
        df = cdata@features,
        with_cluster = with_cluster, 
        with_meta = with_meta, 
        with_well_plate = with_well_plate, 
        ...
      )
    
    return(df)  
    
})


#' @title Obtain names of numeric features
#' 
#' @description Extracts the names of variables stored in 
#' slot @@features of the \code{Cypro} objects cell data. 
#' 
#' @inherit argument_dummy params
#' 
#' @return A character vector.
#' 
#' @export

setGeneric(name = "getFeatureNames", def = function(object, ...){
  
  standardGeneric(f = "getFeatureNames")
  
})

#' @rdname getFeatureNames
#' @export
setMethod(f = "getFeatureNames", signature = "CyproScreening", definition = function(object, ...){

  getFeatureDf(object) %>% 
    dplyr::select(-cell_id) %>% 
    base::colnames() %>% 
    vselect(...)
  
})


#' @title Obtain a feature set  
#' 
#' @description Extracts the names of features that were pooled 
#' under the name of the feature set. 
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

#' @title Obtain frame range
#' 
#' @description Extracts a numeric vector from one till the number of 
#' frames that was denoted during the experiment design steps. 
#'
#' @param object 
#'
#' @return Numeric vector.
#' @export
#'
setGeneric(name = "getFrames", def = function(object, ...){
  
  standardGeneric(f = "getFrames")
  
})

#' @rdname getFrames
#' @export
setMethod(f = "getFrames", signature = "ExperimentDesign", definition = function(object){
  
  1:object@n_frames
  
})

#' @rdname getFrames
#' @export
setMethod(f = "getFrames", signature = "CyproTimeLapse", definition = function(object){
  
  getExperimentDesign(object) %>% 
    getFrames()
  
})

#' @rdname getFrames
#' @export
setMethod(f = "getFrames", signature = "CyproTimeLapseMP", definition = function(object, by_phase = FALSE){
  
  if(base::isTRUE(by_phase)){
    
    phase_starts <- getPhaseStarts(object)
    phase_names <- base::names(phase_starts)
    
    n_frames <- nFrames(object)
    
    n_phases <- nPhases(object)
    
    out <- list()
    
    for(p in 1:n_phases){
      
      start <- phase_starts[p]
      
      if(p != n_phases){
        
        out[[phase_names[p]]] <- start:(phase_starts[p+1]-1)
        
      } else {
        
        out[[phase_names[p]]] <- start:n_frames
        
      }
      
    }
    
  } else {
    
    out <- 
      getExperimentDesign(object) %>% 
      getFrames()
    
  }
  
  return(out)
  
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
setMethod(f = "getFrameTimeSeq", signature = "CyproTimeLapse", definition = function(object){
  
  getFrames(object) * getInterval(object)
  
})

#' @rdname getFrameTimeSeq
#' @export
setMethod(f = "getFrameTimeSeq", signature = "CyproTimeLapseMP", definition = function(object, by_phase = FALSE, ...){
  
  if(base::isTRUE(by_phase)){
    
    warning("write that")
    
  } else{
    
    getFrames(object) * getInterval(object)
    
  }
  
})



# G -----------------------------------------------------------------------

#' @title Obtain cell grouping data 
#' 
#' @description Extract a data.frame that contains variables that group 
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
      ) %>% 
      purrr::discard(.p = base::is.numeric)
    
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
      ) %>% 
      purrr::discard(.p = base::is.numeric)
    
    return(df)
    
  }
)

#' @title Obtain group names a grouping variable contains
#' 
#' @description Extracts the names of the groups in which a specific grouping
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
setMethod(f = "getGroupNames", signature = "Cypro", definition = function(object, grouping_variable, ...){
  
  is_value(grouping_variable, mode = "character")
  
  group_vec <- 
    getGroupingDf(object = object, verbose = FALSE) %>% 
    dplyr::select(-cell_id) %>% 
    dplyr::pull(var = {{grouping_variable}}) 
  
  if(base::is.factor(group_vec)){
    
    group_vec <- base::levels(x = group_vec)
    
  } else if(base::is.character(group_vec)){
    
    group_vec <- base::unique(group_vec)
    
  } else {
    
    msg <- glue::glue("The result of grouping variable '{grouping_variable}' must be a character vector or a factor.")
    
    give_feedback(msg = msg, fdb.fn = "stop")
    
  }
  
  res <- vselect(input = group_vec, ...)
  
  return(res)
  
})

#' @rdname getGroupNames
#' @export
setMethod(
  f = "getGroupNames",
  signature = "CyproTimeLapseMP",
  definition = function(object, grouping_variable, ...){
    
    is_value(grouping_variable, "character")
    
    group_vec <- 
      getGroupingDf(object = object, phase = phase, verbose = FALSE) %>% 
      dplyr::select(-cell_id) %>% 
      dplyr::pull(var = {{grouping_variable}}) 
    
    if(base::is.factor(group_vec)){
      
      group_vec <- base::levels(x = group_vec)
      
    } else if(base::is.character(group_vec)){
      
      group_vec <- base::unique(group_vec)
      
    } else {
      
      msg <- glue::glue("The result of grouping variable '{grouping_variable}' must be a character vector or a factor.")
      
      give_feedback(msg = msg, fdb.fn = "stop")
      
    }
    
    res <- vselect(input = group_vec, ...)
    
    return(res)
    
  })


#' @title Obtain grouping variable names of cell data
#' 
#' @description Extracts the names of variables that group cells. Useful to obtain 
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
    
    group_df <- 
      getGroupingDf(object, verbose = verbose) %>% 
      dplyr::select(-cell_id)
    
    res <- 
      get_grouping_variable_names_hlpr(
        group_df = group_df, 
        named = named, 
        verbose = verbose, 
      ) %>% 
      vselect(input = ., ...)
    
    return(res)
    
  }
)


#' @rdname getGroupingVariableNames
#' @export
setMethod(
  f = "getGroupingVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, ..., named = FALSE, phase = NULL, verbose = NULL){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    group_df <- 
      getGroupingDf(object, verbose = verbose, phase = phase) %>% 
      dplyr::select(-cell_id)
    
    res <- 
      get_grouping_variable_names_hllpr(
        group_df = group_df, 
        named = named, 
        verbose = verbose, 
        ...
      ) %>% 
      vselect(input = ., ...)
    
    return(res)
    
  }
)

get_grouping_variable_names_hlpr <- function(group_df, ..., named = FALSE, verbose = TRUE){
  
  all_var_names <- 
    base::colnames(group_df)
  
  if(base::isTRUE(named)){
    
    sources <- base::vector("character", base::length(all_var_names))
    
    cluster_names <-
      getClusterVariableNames(object, phase = phase, verbose = verbose)
    
    meta_names <- getMetaVariableNames(object, phase = phase)
    
    wp_names <- getWellPlateVariableNames(object)
    
    for(i in base::seq_along(all_var_names)){
      
      var <- all_var_names[i]
      
      if(var %in% cluster_names){
        
        sources[i] <- "cluster"
        
      } else if(var %in% meta_names){
        
        sources[i] <- "meta"
        
      } else if(var %in% wp_names){
        
        sources[i] <- "well_plate"
        
      }
      
    }
    
    base::names(all_var_names) <- sources
    
  }
  
  return(all_var_names)
  
}



# I -----------------------------------------------------------------------



#' @title Obtain frame interval
#' 
#' @description Extracts the interval between the frames as the numeric 
#' value.
#'
#' @inherit argument_dummy params
#'
#' @return A numeric value. 
#' @export
#'
setGeneric(name = "getInterval", def = function(object){
  
  standardGeneric(f = "getInterval")
  
})

#' @rdname getInterval
#' @export
setMethod(f = "getInterval", signature = "ExperimentDesignTimeLapse", definition = function(object){
  
  object@interval
  
})

#' @rdname getInterval
#' @export
setMethod(f = "getInterval", signature = "CyproTimeLapse", definition = function(object){
  
  exp_design <- getExperimentDesign(object)
  
  out <- getInterval(exp_design)
  
  return(out)
  
})


#' @rdname getInterval
#' @export
setMethod(f = "getInterval", signature = "CyproTimeLapseMP", definition = function(object){
  
  exp_design <- getExperimentDesign(object)
  
  out <- getInterval(exp_design)
  
  return(out)
  
})

#' @title Obtain frame interval unit
#' 
#' @description Extracts the unit of the interval between the frames as
#' a character value.
#'
#' @inherit argument_dummy params
#'
#' @return A character value. 
#' @export
#'
setGeneric(name = "getIntervalUnit", def = function(object){
  
  standardGeneric(f = "getIntervalUnit")
  
})

#' @rdname getIntervalUnit
#' @export
setMethod(f = "getIntervalUnit", signature = "ExperimentDesignTimeLapse", definition = function(object){
  
  object@interval_unit
  
})

#' @rdname getIntervalUnit
#' @export
setMethod(f = "getIntervalUnit", signature = "CyproTimeLapse", definition = function(object){
  
  ed <- getExperimentDesign(object)
  
  out <- getIntervalUnit(ed)
  
  return(out)
  
})

#' @title Obtain image directories
#' 
#' @description Extracts a data.frame in which the added directories
#' to the original images are set by well-roi. 
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame.
#' @export
#'
setGeneric(name = "getImageDirDf", def = function(object){
  
  standardGeneric(f = "getImageDirDf")
  
})

#' @rdname getImageDirDf
#' @export

setMethod(f = "getImageDirDf", signature = "Cypro", definition = function(object){
  
  object@image_directories %>% 
    tibble::as_tibble()
  
})

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


#' @title Obtain well plate layout data.frame
#' 
#' @description Extracts the well plate layout in form of a data.frame.
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
setMethod(f = "getLayoutDf", signature = "WellPlate", definition = function(object, ...){
  
  df <- 
    tibble::as_tibble(object@layout) %>% 
    as_layout_df(well_plate_type = object@type)
  
  n_phases <- base::attr(df, which = "n_phases")
  
  if(base::is.numeric(n_phases)){
    
    df <- as_layout_df_mp(df = df, well_plate_type = object@type, n_phase = n_phases)
    
  }
  
  return(df)
  
})

#' @rdname getLayoutDf
#' @export
setMethod(f = "getLayoutDf", signature = "ExperimentDesign", definition = function(object, well_plate = NULL, ...){
  
  layout_df <- 
    getWellPlate(object, well_plate = well_plate) %>% 
    getLayoutDf()
  
  return(layout_df)
  
})

#' @rdname getLayoutDf
#' @export
setMethod(f = "getLayoutDf", signature = "Cypro", definition = function(object, well_plate = NULL, ...){
  
  well_plate <- check_wp_name(object, well_plate)
  
  layout_df <- 
    getExperimentDesign(object) %>% 
    getLayoutDf(well_plate = well_plate)
  
  return(layout_df)
  
})


#' @title Obtain layout variable names
#' 
#' @description Extracts the variable names of the layout data.frame
#' as a character vector.
#' 
#' @inherit argument_dummy params
#' @param named Logical. If TRUE, the returned vector of names is named by 
#' the class of the variable.
#' 
#' @return Character vector.
#' 
#'  @export 
#' 
setGeneric(name = "getLayoutVariableNames", def = function(object, named = TRUE, ...){
  
  standardGeneric(f = "getLayoutVariableNames")
  
})

#' @rdname getLayoutVariableNames
#' @export
setMethod(f = "getLayoutVariableNames", signature = "layout_df", definition = function(object, named = TRUE){
  
  layout_df <- object
  
  vnames <- base::names(layout_df)
  
  if(base::isTRUE(named)){
    
    base::names(vnames) <- 
      purrr::map_chr(.x = layout_df, .f = base::class)
    
  }
  
  return(vnames)
  
}) 

#' @rdname getLayoutVariableNames
#' @export
setMethod(f = "getLayoutVariableNames", signature = "Cypro", definition = function(object, named = TRUE, well_plate = NULL, ...){
  
  layout_df <- getLayoutDf(object, well_plate = well_plate)
  
  out <- getLayoutVariableNames(layout_df, named = named)
  
  return(out)
  
})


#' @title Obtain loading status summary
#' 
#' @description Extracts a data.frame that summarizes the loading status
#' of the data by well plate.
#' 
#' @inherit argument_dummy params 
#' 
#' @return A data.frame. 
#' 
#' @export

setGeneric(name = "getLoadingStatusDf", def = function(object, ...){
  
  standardGeneric(f = "getLoadingStatusDf")
  
})

#' @rdname getLoadingStatusDf
#' @export
setMethod(
  f = "getLoadingStatusDf",
  signature = "Cypro",
  definition = function(object,
                        well_plates = NULL,
                        with_transferred = TRUE,
                        n_letters = 40){
  
  well_plate_list <- getWellPlates(object, well_plates = NULL)
  
  out <- list()
  
  if(byFolder(object)){
    
    out$well_plate <- base::names(well_plate_list)
    
    out$folder <- getWellPlateDirectories(object, well_plates = well_plates)
    
    out$valid_files <- 
      purrr::map_int(.x = well_plate_list, .f = ~ base::length(.x@files)) 
    
    out$expected_files <- 
      purrr::map_int(.x = well_plate_list, .f = nExpectedFiles)
    
    out$ambiguous_files <- 
      purrr::map_int(.x = well_plate_list, .f = nAmbiguousFiles)
    
    out$loaded_files <- 
      purrr::map_int(
        .x = well_plate_list,
        .f = ~ purrr::keep(.x = .x@files, .p = ~ containsData(.x)) %>% base::length()
      )
    
    if(base::isTRUE(with_transferred)){
      
      out$transferred_files <- 
        purrr::map_int(
          .x = well_plate_list, 
          .f = ~ purrr::keep(.x = .x@files, .p = ~ base::isTRUE(.x@transferred)) %>% base::length()
        )
      
    }
    
    df <- 
      base::as.data.frame(out) %>% 
      tibble::remove_rownames() %>% 
      tibble::as_tibble()
    
  } else if(byWellPlate(object)){
    
    df <- 
      purrr::map(.x = well_plate_list, .f = ~ .x@files) %>% 
      purrr::flatten() %>% 
      purrr::map_df(.f = function(data_file){
      
        pattern <- stringr::str_c(".{1,", n_letters, "}$", sep = "")
        
        dir <- 
          purrr::map_chr(
            .x = data_file@directory,
            .f = ~ stringr::str_extract(.x, pattern = pattern)
            ) %>% 
          stringr::str_c("...~", .)
        
        df <- 
          tibble::tibble(
            well_plate = data_file@well_plate, 
            directory = dir,
            loaded = containsData(data_file)
            )
        
        if(base::isTRUE(with_transferred)){
          
          df$transferred = data_file@transferred
          
        }
        
        return(df)
      
    }) %>% 
      tibble::as_tibble()
    
  } else {
      
    df <- data.frame()
    
  }
  
  return(df)
  
})



# M -----------------------------------------------------------------------


#' @title Obtain merged data.frame
#' 
#' @description Merges @@slot content of all objects of class \code{DataFiles}
#' that contain data and that do not contain errors to a single data.frame. 
#' 
#' @inherit argument_dummy params
#' 
#' @details The returned data.frame contains the complete data set including 
#' id-, numeric-, -meta, -well plate and cluster variables. It is then split 
#' into the slots of \code{Cdata} object via \code{setCdataContent()}.
#' 
#' @return A data.frame of class \code{cypro_df}.
#' 
#' @export

setGeneric(name = "getMergedDf", def = function(object, verbose = TRUE){
  
  standardGeneric(f = "getMergedDf")
  
})

#' @rdname getMergedDf
#' @export
setMethod(f = "getMergedDf", signature = "Cypro", definition = function(object, verbose = TRUE){
  
  well_plate_list <- getWellPlates(object)
  
  data_list <- 
    purrr::map(
      .x = well_plate_list,
      .f = ~ purrr::keep(.x = .x@files, .p = ~ containsData(.x) & !containsErrors(.x))
    ) %>% 
    purrr::keep(.p = ~ !isOfLength(x = .x, l = 0))
  
  well_plates <- base::names(data_list)
  
  data_list <- 
    purrr::flatten(data_list) %>%
    base::unname() 
  
  n_files <- base::length(data_list)
  
  if(n_files == 0){
    
    stop("No data files found that contain data and do not contain error feedbacks.")
    
  } else {
    
    confuns::give_feedback(
      msg = glue::glue("Merging data of {n_files} files."), 
      verbose = verbose
    )
    
  }
  
  if(n_files > 250){
    
    pb <- create_progress_bar(total = n_files)
    
  } else {
    
    pb <- NULL
    
  }
  
  merged_df <- purrr::map_df(.x = data_list, .f = function(data_file){
    
    if(base::isTRUE(verbose) & !base::is.null(pb)){
      
      pb$tick()
      
    }
    
    df <- getDataFileDf(object = data_file)
    
    return(df)
    
  }) %>% 
    dplyr::left_join(x = ., y = getWellPlateIndices(object), by = "well_plate_name") %>% 
    dplyr::mutate(
      cell_id = make_cell_id(
        cell_id = cell_id, 
        well_plate_index = well_plate_index, 
        well = well, 
        roi = roi
      ), 
      well_roi = stringr::str_c(well, roi, sep = "_")
    ) %>%
    tibble::as_tibble() 
  
  numeric_vars <- getAdditionalVariableNames(object)$numeric
  
  if(base::length(numeric_vars) >= 1){
    
    merged_df <- 
      dplyr::mutate(
        .data = merged_df,
        dplyr::across(.cols = dplyr::all_of(numeric_vars), .fns = base::as.numeric)
        )  
    
  }
  
  grouping_vars <- getAdditionalVariableNames(object)$grouping
  
  if(base::length(grouping_vars) >= 1){
    
    merged_df <- 
      dplyr::mutate(
        .data = merged_df,
        dplyr::across(.cols = dplyr::all_of(grouping_vars), .fns = base::as.factor)
        )
    
  }
  
  merged_df <- merged_df
  
  return(merged_df)
  
  
})

#' @title Obtain cell meta data 
#' 
#' @description Extracts cell meta data in form of a data.frame that contains
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
  definition = function(object, ..., verbose = NULL){
    
    res <- 
      getMetaDf(object) %>% 
      dplyr::select(-cell_id) %>% 
      dplyr::select(...) %>% 
      base::colnames() 
    
    return(res)
    
  }
)


#' @rdname getMetaVariableNames
#' @export
setMethod(
  f = "getMetaVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, ..., named = FALSE, phase = NULL, verbose = NULL){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    res <- 
      getMetaDf(object, verbose = TRUE, phase = phase) %>% 
      dplyr::select(-cell_id) %>% 
      dplyr::select(...) %>% 
      base::colnames() 
    
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





