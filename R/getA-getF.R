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


#' @rdname getActiveModuleNames
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

#' @title Obtain analysis aspect
#' 
#' @description Extracts S4 object of class \code{AnalysisAspect} as described in 
#' package \code{confuns}.
#' 
#' @param aspect Character value. The analysis aspect.
#' @inherit argument_dummy params
#' 
#' @return An object of class \code{AnalysisAspect}.
#' 
setGeneric(name = "getAnalysisAspect", def = function(object, ...){
  
  standardGeneric(f = "getAnalysisAspect")
  
})

#' @rdname getAnalysisAspect
#' @export
setMethod(
  f = "getAnalysisAspect", 
  signature = "CyproScreening", 
  definition = function(object, aspect, fset){
    
    out <- object@analysis[[aspect]][[fset]]
    
    out@data <- 
      getFeatureDf(object, with_everything = TRUE) 
    
    out@data_scaled <- 
      getScaledDf(object) %>% 
      dplyr::select(cell_id, dplyr::all_of(out@variables_numeric))
    
    out@meta <- 
      getFeatureDf(object) %>% 
      dplyr::select(cell_id)
    
    out@variables_grouping <- getGroupingVariableNames(object)
    
    out@variables_logical <- 
      dplyr::select_if(out@data, .predicate = base::is.logical) %>% 
      base::colnames()
    
    return(out)
    
  }
)

#' @rdname getAnalysisAspect
#' @export
setMethod(
  f = "getAnalysisAspect", 
  signature = "CyproTimeLapse", 
  definition = function(object, aspect, fset){
    
    out <- object@analysis[[aspect]][[fset]]
    
    out@data <- 
      getStatsDf(object, with_everything = TRUE) 
    
    out@data_scaled <- 
      getScaledDf(object) %>% 
      dplyr::select(cell_id, dplyr::all_of(out@variables_numeric))
    
    out@variables_grouping <- getGroupingVariableNames(object)
    
    out@variables_logical <- 
      dplyr::select_if(out@data, .predicate = base::is.logical) %>% 
      base::colnames()
    
    return(out)
    
  }
)

#' @rdname getAnalysisAspect
#' @export
setMethod(
  f = "getAnalysisAspect", 
  signature = "CyproTimeLapse", 
  definition = function(object, aspect, fset, phase){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    out <- object@analysis[[aspect]][[fset]][[phase]]
    
    out@data <- 
      getStatsDf(object, with_everything = TRUE, phase = phase) 
    
    out@data_scaled <- 
      getScaledDf(object) %>% 
      dplyr::select(cell_id, dplyr::all_of(out@variables_numeric))
    
    out@variables_grouping <- getGroupingVariableNames(object, phase = phase)
    
    out@variables_logical <- 
      dplyr::select_if(out@data, .predicate = base::is.logical) %>% 
      base::colnames()
    
    return(out)
    
  }
)

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

#' @rdname getCellIDs
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


#' @rdname getClustering
#' @export
setMethod(
  f = "getClustering", 
  signature = "Cypro",
  definition = function(object, fset = "all_features", ...){
    
    check_one_of(
      input = fset, 
      against = getFeatureSetNames(object)
    )
    
    out <- getAnalysisAspect(object, aspect = "clustering", fset = fset)
    
    return(out)
    
  }
)

#' @rdname getClustering
#' @export
setMethod(
  f = "getClustering", 
  signature = "CyproTimeLapseMP",
  definition = function(object, fset = "all_features", phase = NULL){
    
    check_one_of(
      input = fset, 
      against = getFeatureSetNames(object)
    )
    
    out <- getAnalysisAspect(object, aspect = "clustering", fset = fset, phase = phase)
    
    return(out)
    
  }
)


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
  definition = function(object, verbose = NULL){
    
    check_object(object)
    
    assign_default(object)
    
    res <- 
      getClusterDf(object, verbose = TRUE) %>% 
      dplyr::select(-cell_id) %>% 
      base::colnames()
    
    return(res)
    
  }
)

#' @rdname getClusterVariableNames
#' @export
setMethod(
  f = "getClusterVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, named = FALSE, phase = NULL, verbose = NULL){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)

    res <- 
      getClusterDf(object, verbose = TRUE, phase = phase) %>% 
      dplyr::select(-cell_id) %>%
      base::colnames() 
    
    return(res)
    
  }
)



#' @rdname getCorrelation
#' @export
setMethod(
  f = "getCorrelation", 
  signature = "Cypro",
  definition = function(object, fset = "all_features"){
    
    check_one_of(
      input = fset, 
      against = getFeatureSetNames(object)
    )
    
    out <- getAnalysisAspect(object, aspect = "correlation", fset = fset)
    
    return(out)
    
  }
)

#' @rdname getCorrelation
#' @export
setMethod(
  f = "getCorrelation", 
  signature = "CyproTimeLapseMP",
  definition = function(object, fset = "all_features", phase = NULL){
    
    check_one_of(
      input = fset, 
      against = getFeatureSetNames(object)
    )
    
    out <- getAnalysisAspect(object, aspect = "correlation", fset = fset, phase = phase)
    
    return(out)
    
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


#' @title Obtain \code{DimRed} object
#' 
#' @description Extracts \code{DimRed} object of the respective
#' feature set. 
#' 
#' @inherit argument_dummy params
#' 
#' @return Object of class \code{DimRed}.
#' 
#' @export
#' 
setGeneric(name = "getDimRed", def = function(object, ...){
  
  standardGeneric(f = "getDimRed")
  
})

#' @rdname getDimRed
#' @export
setMethod(
  f = "getDimRed", 
  signature = "Cypro",
  definition = function(object, fset = "all_features"){
    
    check_one_of(
      input = fset, 
      against = getFeatureSetNames(object)
    )
    
    out <- getAnalysisAspect(object, aspect = "dimred", fset = fset)
    
    return(out)
    
  }
)

#' @rdname getDimRed
#' @export
setMethod(
  f = "getDimRed", 
  signature = "CyproTimeLapseMP",
  definition = function(object, fset = "all_features", phase = NULL){
    
    check_one_of(
      input = fset, 
      against = getFeatureSetNames(object)
    )
    
    out <- getAnalysisAspect(object, aspect = "dimred", fset = fset, phase = phase)
    
    return(out)
    
  }
)

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
                        with_everything = FALSE,
                        ...){
  
  df <- 
    joinWith(
      object = object, 
      df = object@features, 
      with_cluster = with_cluster, 
      with_meta = with_meta, 
      with_well_plate = with_well_plate, 
      with_everything = with_everything,
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
                        with_everything = NULL,
                        with_non_data = TRUE,
                        ...){
    
    cdata <- getCdata(object)
    
    df <- 
      joinWith(
        object = cdata, 
        df = cdata@features,
        with_cluster = with_cluster, 
        with_meta = with_meta, 
        with_well_plate = with_well_plate, 
        with_everything = with_everything,
        ...
      ) 
    
    if(base::isFALSE(with_non_data)){
      
      df <- 
        dplyr::select(df, -dplyr:any_of(x = non_data_variables))
      
    }
    
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

setGeneric(name = "getFeatureSet", def = function(object, ...){
  
  standardGeneric(f = "getFeatureSet")
  
})

#' @rdname getFeatureSet 
#' @export 
setMethod(f = "getFeatureSet", signature = "Cypro", definition = function(object, fset){
  
  if(base::length(base::names(object@feature_sets)) == 0){
    
    stop("No variable sets have been defined yet.")
    
  }
  
  confuns::check_one_of(
    input = fset, 
    against = base::names(object@feature_sets), 
    fdb.opt = 2, 
    ref.opt.2 = "defined feature sets"
  )
  
  fset <- object@feature_sets[[fset]]
  
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



