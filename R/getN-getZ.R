
#' @include S4-classes.R
NULL
#' @include S4-method-skeletons.R
NULL


# P -----------------------------------------------------------------------

#' @title Extract phase names
#' 
#' @description Obtain the names of all phases the experiment design 
#' contains in form of a vector.
#' 
#' @inherit argument_dummy params
#' 
#' @return Character vector.
#' @export

setGeneric(name = "getPhases", def = function(object){
  
  standardGeneric(f = "getPhases")
  
})

#' @rdname getPhases
#' @export
setMethod(f = "getPhases", signature = "CyproTimeLapseMP", definition = function(object){
  
  object@design@phases %>% base::names()
  
})



# S -----------------------------------------------------------------------

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
setMethod(f = "getStatsDf", signature = "CdataTimeLapse", definition = function(object, ...){
  
  return(object@features_stats)
  
})

#' @rdname getStatsDf
#' @export
setMethod(f = "getStatsDf", signature = "CdataTimeLapseMP", definition = function(object, phase, ...){
  
  confuns::check_one_of(
    input = phase, 
    against = base::names(object@features_stats)
  )
  
  return(object@features_stats)
  
})

#' @rdname getStatsDf
#' @export
setMethod(f = "getStatsDf", signature = "CyproTimeLapse", definition = get_stats_df)

#' @rdname getStatsDf
#' @export
setMethod(f = "getStatsDf", signature = "CyproTimeLapseMP", definition = get_stats_df_mp)

#' @title Extract numeric variables of cell stat data 
#'
#' @description Obtain the names of the numeric data variables of your 
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

#' @title Extract storage directory
#' 
#' @description Obtain the directory under which the \code{Cypro} object 
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
  
  check_object(object, set_up_req = "experiment_design")
  
  dir <- object@storage
  
  if(base::length(dir) == 0){
    
    base::stop("Storage directory has not beend defined yet.")
    
  } else if(!stringr::str_detect(dir, pattern = "\\.{1}RDS")){
    
    base::stop("Invalid storage directory. Must end with '.RDS'.")
    
  } else {
    
    base::return(dir)
    
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


# T -----------------------------------------------------------------------

#' @title Extract track data.frame
#' 
#' @description Obtain numeric cell features of time lapse experiments in 
#' form of a data.frame.
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame in which each observation refers to a cell at a given frame.
#' 
#' @export
#'

setGeneric(name = "getTracksDf", def = function(object, ...){
  
  standardGeneric(f = "getTracksDf")
  
})

#' @rdname getTracksDf
#' @export
setMethod(f = "getTracksDf", signature = "CdataTimeLapse", definition = function(object, ...){
  
  return(object@features_tracks)
  
})

#' @rdname getTracksDf
#' @export
setMethod(f = "getTracksDf", signature = "CdataTimeLapseMP", definition = function(object, phase, ...){
  
  confuns::check_one_of(
    input = phase, 
    against = base::names(object@features_tracks)
  )
  
  return(object@features_stats)
  
})

#' @rdname getTracksDf
#' @export
setMethod(f = "getTracksDf", signature = "CyproTimeLapse", definition = get_tracks_df)

#' @rdname getTracksDf
#' @export
setMethod(f = "getTracksDf", signature = "CyproTimeLapseMP", definition = get_tracks_df_mp)

#' @title Extract numeric variables of cell track data 
#' 
#' @description Obtain the names of cell features of time lapse experiments.
#'
#' @inherit getStatVariableNames params
#' 
#' @return A character vector. 
#' 
#' @seealso starts_with(), ends_with(), contains(), matches()
#' 
#' @export
#' 

setGeneric(name = "getTrackVariableNames", def = function(object, ...){
  
  standardGeneric(f = "getTrackVariableNames")
  
})

#' @rdname getTrackVariableNames
#' @export
setMethod(
  f = "getTrackVariableNames",
  signature = "CyproTimeLapse",
  definition = function(object, ...){
    
    check_object(object)
    
    res <- 
      getTracksDf(object, with_grouping = FALSE) %>% 
      dplyr::select(-cell_id) %>% 
      base::colnames() %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  })

#' @rdname getTrackVariableNames
#' @export
setMethod(
  f = "getTrackVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, phase = NULL, ...){
    
    check_object(object)
    
    assign_default(object)
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    res <- 
      getTracksDf(object, phase = phase, with_grouping = FALSE) %>% 
      dplyr::select(-cell_id) %>% 
      base::colnames() %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  }
)


# W -----------------------------------------------------------------------

#' @title Extract well plate
#' 
#' @description Obtain object of class \code{WellPlate}.
#'
#' @inherit argument_dummy params
#' 
#' @return A data.frame.
#' @export

setGeneric(name = "getWellPlate", def = function(object, well_plate){
  
  standardGeneric(f = "getWellPlate")
  
})


#' @rdname getWellPlate
#' @export
setMethod(f = "getWellPlate", signature = "ExperimentDesign", function(object, well_plate){
  
  confuns::check_one_of(
    input = well_plate, 
    against = base::names(object@well_plates)
  )
  
  wp <- object@well_plates[[well_plate]]
  
  return(wp)
  
})

#' @rdname getWellPlate
#' @export
setMethod(f = "getWellPlate", signature = "Cypro", function(object, well_plate){
  
  confuns::check_one_of(
    input = well_plate, 
    against = getWellPlateNames(object)
  )
  
  wp <- 
    getExperimentDesign(object) %>% 
    getWellPlate(well_plate = well_plate)
  
  return(wp)
  
})


#' @title Extract cell well plate data (by cell id)
#' 
#' @description Obtain cell grouping variables according to the experiment 
#' design.
#' 
#' Not to be confused with the well plate layout which is stored in data.frames
#' of class \code{layout_df} that can be obtained via \code{getLayoutDf()}.
#'
#' @inherit argument_dummy params
#' 
#' @return A data.frame.
#' @export

setGeneric(name = "getWellPlateDf", def = function(object, ...){
  
  standardGeneric(f = "getWellPlateDf")
  
})

#' @rdname getWellPlateDf
#' @export
setMethod(f = "getWellPlateDf", signature = "Cypro", definition = function(object, well_plate){
  
  check_object(object)
  
  cdata_object <- getCdata(object)
  
  wp_df <- getWellPlateDf(object = cdata_object)
  
  return(wp_df)
  
})


#' @title Extract well plate indices
#' 
#' @description Obtain the indices of well plates. Well plates are indexed
#' according to the order in which their layout is designed during the 
#' session in \code{designExperimen()}.
#'
#' @inherit argument_dummy params 
#' @param return Character value. Either \emph{'tibble'} or \emph{'vector'}.
#'
#' @return Either a tibble or a vector.
#' @export
#'

setGeneric(name = "getWellPlateIndices", def = function(object, ...){
  
  standardGeneric(f = "getWellPlateIndices")
  
})

#' @rdname getWellPlateIndices
#' @export
setMethod(f = "getWellPlateIndices", signature = "Cypro", definition = function(object, return = "tibble"){
  
  wps <- object@design@well_plates
  
  wp_names <- base::names(wps)
  
  wp_indices <- 
    purrr::map_int(.x = wps, .f = ~ methods::slot(.x, "index"))
  
  
  if(return == "tibble"){
    
    res <- 
      base::data.frame(
        well_plate_name = wp_names, 
        well_plate_index = wp_indices
      ) %>% 
      tibble::as_tibble()
    
  } else if(return == "vector"){
    
    res <- 
      purrr::set_names(wp_indices, wp_names)
    
  }
  
  return(res)
  
})


#' @title Extract well plate names
#' 
#' @description Obtain names of the well plate stored in the experiment design 
#' in form of a character vector.
#'
#' @inherit argument_dummy params
#'
#' @return A character vector. 
#' @export

setGeneric(name = "getWellPlateNames", def = function(object){
  
  standardGeneric(f = "getWellPlateNames")
  
})

#' @rdname getWellPlateNames
#' @export
setMethod(f = "getWellPlateNames", signature = "ExperimentDesign", def = function(object){
  
  return(base::names(object@well_plates))
  
})

#' @rdname getWellPlateNames
#' @export 
setMethod(f = "getWellPlateNames", signature = "Cypro", def = function(object){
  
  exp_design <- getExperimentDesign(object)
  
  wp_names <- getWellPlateNames(exp_design)
  
  return(wp_names)
  
})

#' @title Extract well plate variable names of cell data
#' 
#' @description Obtain names of variables that group cells according to 
#' the well plate layouts. 
#' 
#' @inherit getClusterVariableNames params
#' 
#' @return A character vector. 
#' 
#' @seealso starts_with(), ends_with(), contains(), matches()
#' 
#' @export

setGeneric(name = "getWellPlateVariableNames", def = function(object, ...){
  
  standardGeneric(f = "getWellPlateVariableNames")
  
})

#' @rdname getWellPlateVariableNames
#' @export
setMethod(f = "getWellPlateVariableNames", signature = "Cypro", definition = function(object, ...){
  
  check_object(object)
  
  wp_names <- 
    getWellPlateDf(object) %>% 
    dplyr::select(-cell_id) %>% 
    base::colnames() %>% 
    confuns::vselect(input = ., ...) 
  
  return(wp_names)
  
})
