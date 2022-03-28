
#' @include S3-classes.R
NULL
#' @include S4-classes.R
NULL
#' @include S4-method-skeletons.R
NULL
#' @include imported-generics.R
NULL



# p -----------------------------------------------------------------------

get_phase <- function(df){
  
  base::attr(x = df, which = "phase")
  
}



# O -----------------------------------------------------------------------

#' @title Obtain object of class \code{OutlierDetection}
#' 
#' @description Extracts the complete \code{OutlierDetection} object from 
#' slot @@quality_checks.
#'
#' @inherit argument_dummy params
#' @param ... 
#'
#' @return An object of S4-class \code{OutlierDetection}.
#' @export
#'
setGeneric(name = "getOutlierDetection", def = function(object, ...){
  
  standardGeneric(f = "getOutlierDetection")
  
})

#' @rdname getOutlierDetection
#' @export
setMethod(
  f = "getOutlierDetection",
  signature = "CyproScreening",
  definition = function(object, verbose = TRUE, ...){
    
    outlier_obj <- object@quality_checks[["outliers"]]
    
    if(base::is.null(outlier_obj)){
      
      give_feedback(
        msg = "Creating new object of class 'OutlierDetection'.",
        verbose = verbose
      )
      
      outlier_obj <- methods::new("OutlierDetection")
      
    }
    
    df <- 
      getFeatureDf(object, with_everything = TRUE) %>% 
      dplyr::select(-dplyr::any_of(non_data_variables))
    
    outlier_obj@data <- df
    
    outlier_obj@meta <- dplyr::select(df, cell_id)
    
    outlier_obj@variables_grouping <-
      dplyr::select(df, -cell_id) %>% 
      dplyr::select_if(.predicate = ~ base::is.character(.x) | base::is.factor(.x)) %>% 
      base::colnames()
    
    outlier_obj@variables_numeric <- getFeatureNames(object)
    
    return(outlier_obj)
    
  })

#' @rdname getOutlierDetection
#' @export
setMethod(
  f = "getOutlierDetection",
  signature = "CyproTimeLapse",
  definition = function(object, verbose = TRUE, ...){
    
    outlier_obj <- object@quality_checks[["outliers"]]
    
    if(base::is.null(outlier_obj)){
      
      give_feedback(
        msg = "Creating new object of class 'OutlierDetection'.",
        verbose = verbose
      )
      
      outlier_obj <- methods::new("OutlierDetection")
      
    }
    
    df <- 
      getStatsDf(object, with_everything = TRUE) %>% 
      dplyr::select(-dplyr::any_of(non_data_variables))
    
    outlier_obj@data <- df
    
    outlier_obj@meta <- dplyr::select(df, cell_id)
    
    outlier_obj@variables_grouping <- getGroupingVariableNames(object)
    
    outlier_obj@variables_numeric <- getStatVariableNames(object)
    
    return(outlier_obj)
    
  })

#' @rdname getOutlierDetection
#' @export
setMethod(
  f = "getOutlierDetection",
  signature = "CyproTimeLapseMP",
  definition = function(object, phase, verbose = TRUE, ...){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    outlier_obj <- object@quality_checks[["outliers"]]
    
    if(base::is.null(outlier_obj)){
      
      give_feedback(
        msg = "Creating new object of class 'OutlierDetection'.",
        verbose = verbose
      )
      
      outlier_obj <- methods::new("OutlierDetection")
      
    }
    
    df <- 
      getStatsDf(object, with_everything = TRUE, phase = phase) %>% 
      dplyr::select(-dplyr::any_of(non_data_variables))
    
    outlier_obj@data <- df
    
    outlier_obj@meta <- dplyr::select(df, cell_id)
    
    outlier_obj@variables_grouping <- getGroupingVariableNames(object)
    
    outlier_obj@variables_numeric <- getStatVariableNames(object)
    
    return(outlier_obj)
    
  })


#' @importMethodsFrom confuns getOutlierIDs
#' @export
setMethod(
  f = "getOutlierIDs",
  signature = "OutlierDetection",
  definition = getMethod(f = "getOutlierIDs",
                         signature = "OutlierDetection", 
                         where = asNamespace(ns = "confuns"))
)

#' @rdname getOutlierIDs
#' @export
setMethod(
  f = "getOutlierIDs",
  signature = "Cypro",
  definition = function(object, method = "IQR", features = NULL, across = NULL, across_subset = NULL, ...){
    
    outlier_obj <- getOutlierDetection(object)
    
    out <- 
      getOutlierIDs(
        object = outlier_obj, 
        method = method, 
        variables = features,
        across = across, 
        across_subset = across_subset,
        flatten = flatten, 
        ...
      )
    
    return(out)
    
  })


#' @rdname getOutlierResults
#' @export
setMethod(
  f = "getOutlierResults", 
  signature = "Cypro", 
  definition = function(object, method = "IQR", across = NULL, verbose = TRUE, ...){
    
    outlier_obj <- getOutlierDetection(object)
    
    out  <- 
      getOutlierResults(
        object = outlier_obj, 
        method = method,
        across = across, 
        verbose = verbose, 
        ...
      )
    
    return(out)
    
  }
)

#' @rdname getOutlierResults
#' @importMethodsFrom confuns getOutlierResults
#' @export
setMethod(
  f = "getOutlierResults",
  signature = "OutlierDetection",
  definition = getMethod(f = "getOutlierResults",
                         signature = "OutlierDetection",
                         where = asNamespace(ns = "confuns"))
)


# P -----------------------------------------------------------------------

#' @title Extract phase names
#' 
#' @description Obtain a numeric vector of valid phases. 
#' 
#' @inherit argument_dummy params
#' 
#' @return Numeric vector.
#' 
#' @seealso \code{getPhaseNames()}
#' 
#' @export

setGeneric(name = "getPhases", def = function(object){
  
  standardGeneric(f = "getPhases")
  
})

#' @rdname getPhases
#' @export
setMethod(f = "getPhases", signature = "CyproTimeLapseMP", definition = function(object){
  
  base::seq_along(object@experiment_design@phases)
  
})

#' @title Obtain phase start points 
#' 
#' @description Extracts the frames with which the respective 
#' experiment phases started.
#'
#' @inherit argument_dummy params
#'
#' @return Numeric vector.
#' @export
#'
setGeneric(name = "getPhaseStarts", def = function(object, ...){
  
  standardGeneric(f = "getPhaseStarts")
  
})

#' @rdname getPhaseStarts
#' @export
setMethod(f = "getPhaseStarts", signature = "CyproTimeLapseMP", definition = function(object, numeric = TRUE){
  
  exp_design <- getExperimentDesign(object)
  
  phase_list <- exp_design@phases
  
  frame_time_vec <- make_frame_vec(n_frames = nFrames(object))
  
  out <- 
    purrr::map_int(
      .x = phase_list, 
      .f = function(phase){
        
        string <-
          stringr::str_extract(phase, pattern = "/.*$") %>% 
          stringr::str_remove_all(pattern = "/") %>% 
          remove_empty_space()
        
        frame <- base::which(x = frame_time_vec == string)
        
        return(frame)
        
      }
    )
  
  return(out)
  
})


#' @title Obtain phase start points 
#' 
#' @description Extracts the names of the phases.
#'
#' @inherit argument_dummy params
#'
#' @return Character vector.
#' @export
#'
setGeneric(name = "getPhaseNames", def = function(object, ...){
  
  standardGeneric(f = "getPhaseNames")
  
})

#' @rdname getPhaseNames
#' @export
setMethod(f = "getPhaseNames", signature = "CyproTimeLapseMP", definition = function(object){
  
  english::ordinal(x = 1:nPhases(object))
  
})



# S -----------------------------------------------------------------------


#' @rdname getScaledDf
#' @export
setMethod(
  f = "getScaledDf",
  signature = "Cypro",
  definition = function(object,
                        with_cluster = FALSE,
                        with_meta = FALSE, 
                        with_well_plate = FALSE,
                        with_everything = FALSE,
                        ...){
    
    sdf <- tibble::as_tibble(object@cdata@scaled)
    
    cdata <- getCdata(object)
    
    sdf <- 
      joinWith(
        object = cdata, 
        df = sdf, 
        with_cluster = with_cluster, 
        with_meta = with_meta, 
        with_well_plate = with_well_plate, 
        with_everything = with_everything
      )
    
    return(sdf)
    
  })

#' @rdname getScaledDf
#' @export
setMethod(
  f = "getScaledDf", 
  signature = "CyproTimeLapseMP", 
  definition = function(object,
                        phase,
                        with_cluster = FALSE,
                        with_meta = FALSE, 
                        with_well_plate = FALSE,
                        with_everything = FALSE,
                        ...){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    sdf <- object@cdata@scaled[[phase]] %>% tibble::as_tibble()
    
    cdata <- getCdata(object)
    
    sdf <- 
      joinWith(
        object = cdata, 
        df = sdf, 
        phase = phase,
        with_cluster = with_cluster, 
        with_meta = with_meta, 
        with_well_plate = with_well_plate, 
        with_everything = with_everything
      )
    
    return(sdf)
    
  })

#' @rdname getScaledMtr
#' @export
setMethod(
  f = "getScaledMtr", 
  signature = "Cypro",
  definition = function(object, ...){
    
    mtr <- 
      getScaledDf(object) %>% 
      tibble::column_to_rownames(var = "cell_id") %>% 
      base::as.matrix()
    
    return(mtr)
    
  }
)

#' @rdname getScaledMtr
#' @export
setMethod(
  f = "getScaledMtr", 
  signature = "CyproTimeLapseMP",
  definition = function(object, phase, ...){
    
    mtr <- 
      getScaledDf(object, phase = phase) %>% 
      tibble::column_to_rownames(var = "cell_id") %>% 
      base::as.matrix()
    
    return(mtr)
    
  }
)


#' @title Extract stats data.frame 
#' 
#' @description Obtain summarized numeric cell features of time lapse experiments in form of a data.frame.
#' 
#' @inherit argument_dummy params
#' @param with_cluster,with_meta,with_well_plate Logical value. Denoting 
#' if the respective grouping information should be joined to the stats data.frame
#' or not.
#'
#' @return A data.frame with all numeric variables summarizing the measurements of 
#' the track data.frame. 
#' 
#' @export
#'

setGeneric(name = "getStatsDf", def = function(object, ...){
  
  standardGeneric(f = "getStatsDf")
  
})

#' @rdname getStatsDf
#' @export
setMethod(f = "getStatsDf", signature = "CdataTimeLapse", definition = function(object,
                                                                                with_cluster = FALSE, 
                                                                                with_meta = FALSE, 
                                                                                with_well_plate = FALSE, 
                                                                                with_everything = NULL, 
                                                                                ...){
  
  df <- 
    tibble::as_tibble(object@features_stats) 
  
  df <- 
    joinWith(
      object = object, 
      df = df, 
      with_cluster = with_cluster, 
      with_meta = with_meta, 
      with_well_plate = with_well_plate, 
      with_everything = with_everything,
      ...
    )
  
  return(df)
  
})

#' @rdname getStatsDf
#' @export
setMethod(f = "getStatsDf", signature = "CdataTimeLapseMP", definition = function(object,
                                                                                  phase,
                                                                                  with_cluster = FALSE, 
                                                                                  with_meta = FALSE, 
                                                                                  with_well_plate = FALSE, 
                                                                                  with_everyting = NULL,
                                                                                  ...){
  
  confuns::check_one_of(
    input = phase, 
    against = base::names(object@features_stats[[phase]])
  )
  
  df <- object@features_stats[[phase]]
  
  df <- 
    joinWith(
      object = object, 
      df = df, 
      with_cluster = with_cluster, 
      with_meta = with_meta, 
      with_well_plate = with_well_plate, 
      with_everything = with_everything,
      ...
    )
  
  return(df)
  
})

#' @rdname getStatsDf
#' @export
setMethod(f = "getStatsDf", signature = "CyproTimeLapse", definition = function(object, 
                                                                                with_cluster = FALSE, 
                                                                                with_meta = FALSE, 
                                                                                with_well_plate = FALSE, 
                                                                                with_everyting = NULL,
                                                                                with_non_data = TRUE,
                                                                                ...){
  
  df <- 
    getCdata(object) %>% 
    getStatsDf(
      with_meta = with_meta, 
      with_cluster = with_cluster, 
      with_well_plate = with_well_plate, 
      with_everything = with_everything,
      ...
    )
  
  if(base::isFALSE(with_non_data)){
    
    df <- dplyr::select(df, -dplyr::any_of(non_data_variables))
    
  }
  
  return(df)
  
})

#' @rdname getStatsDf
#' @export
setMethod(f = "getStatsDf", signature = "CyproTimeLapseMP", definition = function(object,
                                                                                  phase = 1,
                                                                                  with_cluster = FALSE, 
                                                                                  with_meta = FALSE, 
                                                                                  with_well_plate = FALSE, 
                                                                                  with_everything = FALSE,
                                                                                  with_non_data = TRUE,
                                                                                  ...){
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cdata <- getCdata(object)
  
  stats_df <- 
    joinWith(
      object = cdata, 
      df = cdata@features_stats[[phase]], 
      with_cluster = with_cluster, 
      with_meta = with_meta, 
      with_well_plate = with_well_plate,
      with_everything = with_everything,
    )
  
  if(base::isFALSE(with_non_data)){
    
    stats_df <- dplyr::select(stats_df, -dplyr::any_of(non_data_variables))
    
  }
  
  return(stats_df)
  
})

#' @title Obtain numeric variables of cell stat data 
#'
#' @description Extracts the names of the numeric data variables of your 
#' cell data. Useful to obtain vectors of variable names as input for recurring
#' arguments like \code{variables}.
#'
#' @inherit argument_dummy params
#' @param ... Additional selection helpers from the \code{tidyselect} package that match 
#' variable names according to a given pattern. 
#' 
#' @return A character vector. 
#' 
#' @seealso starts_with(), ends_with(), contains(), matches()
#' 
#' @export
#'

setGeneric(name = "getStatVariableNames", def = function(object, ...){
  
  standardGeneric(f = "getStatVariableNames")
  
})

#' @rdname getStatVariableNames
#' @export
setMethod(
  f = "getStatVariableNames",
  signature = "CyproTimeLapse",
  definition = function(object, ...){
    
    check_object(object)
    
    res <- 
      getStatsDf(object, with_grouping = FALSE) %>% 
      dplyr::select(-cell_id) %>% 
      base::colnames() %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  })

#' @rdname getStatVariableNames
#' @export
setMethod(
  f = "getStatVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, phase = NULL, ...){
    
    check_object(object)
    
    assign_default(object)
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    res <- 
      getStatsDf(object, phase = phase, with_grouping = FALSE) %>% 
      dplyr::select(-cell_id) %>% 
      base::colnames() %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  }
)

#' @title Obtain storage directory
#' 
#' @description Extract the directory under which the \code{Cypro} object 
#' is currently stored by default using \code{saveCyproObject()}.
#' 
#' @inherit argument_dummy params
#' 
#' @return Character value. 
#' 
#' @export

setGeneric(name = "getStorageDirectory", def = function(object){
  
  standardGeneric(f = "getStorageDirectory")
  
})

#' @rdname getStorageDirectory
#' @export
setMethod(f = "getStorageDirectory", signature = "Cypro", definition = function(object){
  
  
  dir <- object@information$storage_directory
  
  if(base::length(dir) == 0){
    
    stop("Storage directory has not beend defined yet.")
    
  } else if(!stringr::str_detect(dir, pattern = "\\.{1}RDS")){
    
    stop("Invalid storage directory. Must end with '.RDS'.")
    
  } else {
    
    return(dir)
    
  }
  
})


#' @title Extract subset information 
#' 
#' @description Whenever a \code{Cypro} object is subsetted by one of it's 
#' \code{subsetBy*()} methods information about the subsetting is stored in 
#' the resulting object. This functions uses as an extractor for these 
#' information. 
#'
#' @param nth Numeric value. Denotes the ordinal number of the subsetting 
#' you are interested in.
#'
#' @return A list. 
#' @export
#'
setGeneric(name = "getSubsetList", def = function(object, nth){
  
  standardGeneric("getSubsetList")
  
})

#' @rdname getSubsetList
#' @export
setMethod(f = "getSubsetList", signature = "Cypro", definition = function(object, nth = 1){
  
  check_object(object)
  
  nth <- english::ordinal(x = nth)
  
  subset_list <- object@subset[[nth]]
  
  if(!base::is.list(subset_list)){
    
    msg <- glue::glue("Could not find info for a {nth} subsetting.")
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  return(subset_list)
  
})
