


#' @title Test for used modules
#' 
#' @description Tests if the object contains module set ups.
#'
#' @inherit argument_dummy params
#' @param modules Character vector NULL. If character vector every mentioned
#' module is checked for.
#'
#' @return If \code{modules} is character vector a named logical vector 
#' of the same length. Else a single TRUE or FALSE.
#' @export
#'
setGeneric(name = "containsModules", def = function(object, ...){
  
  standardGeneric(f = "containsModules")
  
})


#' @rdname containsModules
#' @export
setMethod(f = "containsModules", signature = "Cypro", definition = function(object, modules = NULL){
  
  if(base::identical(object@modules, list())){
    
    res <- FALSE
    
  } else if(base::is.character(modules)){
    
    confuns::check_one_of(
      input = modules, 
      against = purrr::flatten_chr(cypro_module_names)
    )
    
    module_names <- getUsedModuleNames(object)
    
    res <- modules %in% module_names
    
    res <- purrr::set_names(x = res, modules)
    
  } else {
    
    res <- TRUE
    
  }
  
  return(res)
  
})




# C -----------------------------------------------------------------------

#' @title Tests for analysis modules
#' 
#' @description A variety of tests that check for certain module specifics.
#' 
#' @inherit argument_dummy params
#' 
#' @return TRUE or FALSE
#' 
#' @export
#' 

setGeneric(name = "containsComputableVariables", def = function(object){
  
  standardGeneric(f = "containsComputableVariables")
  
})

#' @rdname containsComputableVariables
#' @export

setMethod(f = "containsComputableVariables", signature = "AnalysisModule", definition = function(object){
  
  res <- base::length(object@variables_computable) >= 1
  
  return(res)
  
})


# D -----------------------------------------------------------------------


#' @title Test for valid directory slot
#' 
#' @description Tests if the object contains valid content in slot @@directory.
#'
#' @inherit argument_dummy params
#'
#' @return TRUE or FALSE
#' @export

setGeneric(name = "containsData", def = function(object){
  
  standardGeneric(f = "containsData")
  
})

#' @rdname containsData
#' @export
setMethod(f = "containsData", signature = "DataFile", definition = function(object){
  
  !base::identical(x = object@content, y = list())
  
})

#' @rdname containsData
#' @export
setMethod(f = "containsData", signature = "Cypro", definition = function(object){
  
  getWellPlates(object) %>% 
  purrr::map(.f = ~ purrr::keep(.x = .x@files, .p = containsData)) %>% 
  purrr::map_int(.f = base::length) %>% 
  base::any(. >= 1)
  
})

#' @title Test for valid directory slot
#' 
#' @description Tests if the object contains valid content in slot @@directory.
#'
#' @inherit argument_dummy params
#'
#' @return TRUE or FALSE
#' @export
#'
setGeneric(name = "containsDirectory", def = function(object){
  
  standardGeneric(f = "containsDirectory")
  
})

#' @rdname containsDirectory
#' @export
setMethod(f = "containsDirectory", signature = "DataFile", definition = function(object){
  
  dir <- object@directory 
  
  if(base::length(dir) == 1){
    
    out <- FALSE
    
  } else {
    
    if(base::is.na(dir)){
      
      out <- FALSE
      
    } else {
      
      out <- stringr::str_detect(dir, pattern = filetypes)
        
    }
    
  }
  
  return(out)
  
})


#' @rdname containsDirectory
#' @export
setMethod(f = "containsDirectory", signature = "WellPlate", definition = function(object){
  
  dir <- object@directory 
  
  if(!base::length(dir) >= 1){
    
    out <- FALSE
    
  } else {
    
    if(base::is.na(dir)){
      
      out <- FALSE
      
    } else {
      
      out <- TRUE
      
    }
    
  }
  
  return(out)
  
})


# E -----------------------------------------------------------------------


#' @title Test for validation errors
#' 
#' @description Tests if the return value of \code{validateInputDf()} contained
#' any error messages.
#'
#' @inherit argument_dummy params
#'
#' @return TRUE or FALSE
#' 
#' @export
#'
setGeneric(name = "containsErrors", def = function(object){
  
  standardGeneric(f = "containsErrors")
  
})


#' @rdname containsErrors
#' @export
setMethod(f = "containsErrors", signature = "DataFile", definition = function(object){
  
  content <- object@content
  
  errors <- purrr::keep(.x = content, .p = ~ isOfClass(x = .x, valid_class = "glue"))
  
  res <- base::length(errors) >= 1
  
  return(res)
  
})




# O -----------------------------------------------------------------------

#' @rdname containsComputableVariables
#' @export
setGeneric(name = "containsOptionalVariables", def = function(object){
  
  standardGeneric(f = "containsOptionalVariables")
  
})

#' @rdname containsComputableVariables
#' @export
setMethod(f = "containsOptionalVariables", signature = "AnalysisModule", definition = function(object){
  
  res <- base::length(object@variables_optional) >= 1
  
  return(res)
  
})



# S -----------------------------------------------------------------------



#' @title Tests for analysis modules
#' 
#' @description A variety of tests that check for certain module specifics.
#' 
#' @inherit argument_dummy params
#' 
#' @return TRUE or FALSE
#' 
#' @export
#' 

setGeneric(name = "containsSummaryVariables", def = function(object){
  
  standardGeneric(f = "containsSummaryVariables")
  
})

#' @rdname containsSummaryVariables
#' @export

setMethod(f = "containsSummaryVariables", signature = "AnalysisModule", definition = function(object){
  
  base::length(object@variables_summary) >= 1
  
})




#' @title Test for stats data.frame
#' 
#' @description Tests if the respective slot is not empty.
#'
#' @inherit argument_dummy params
#'
#' @return TRUE or FALSE
#' @export

setGeneric(name = "containsStatsDf", def = function(object, stop_if_false = FALSE){
  
  standardGeneric(f = "containsStatsDf")
  
})

#' @rdname containsStatsDf
#' @export
setMethod(f = "containsStatsDf", signature = "CdataTimeLapse", definition = function(object, stop_if_false = FALSE){
  
  is_empty <- base::nrow(object@features_stats) == 0
  
  fdb_empty_slot(
    eval = is_empty,
    ref = "feature_stats", 
    stop_if_empty = stop_if_false
  )
  
  out <- !is_empty
  
  return(out)
  
})

#' @rdname containsStatsDf
#' @export
setMethod(f = "containsStatsDf", signature = "CdataTimeLapseMP", definition = function(object, stop_if_false = FALSE){
  
  is_empty <- 
    purrr::map_int(.x = object@features_stats, .f = base::nrow) %>% 
    base::all(. == 0)
  
  fdb_empty_slot(
    eval = is_empty,
    ref = "feature_stats", 
    stop_if_empty = stop_if_false
  )
  
  out <- !is_empty
  
  return(out)
  
})

#' @rdname containsStatsDf
#' @export
setMethod(f = "containsStatsDf", signature = "Cypro", definition = function(object, stop_if_false = FALSE){
  
  getCdata(object) %>% 
    containsStatsDf(stop_if_false = stop_if_false)
  
})



#' @title Tests for tracks data.frame
#' 
#' @inherit containsStatsDf description params return
#'
#' @export
setGeneric(name = "containsTracksDf", def = function(object, stop_if_false = FALSE){
  
  standardGeneric(f = "containsTracksDf")
  
})

#' @rdname containsTracksDf
#' @export
setMethod(f = "containsTracksDf", signature = "CdataTimeLapse", definition = function(object, stop_if_false = FALSE){
  
  is_empty <- base::nrow(object@features_tracks) == 0
  
  fdb_empty_slot(
    eval = is_empty,
    ref = "feature_stats", 
    stop_if_empty = stop_if_false
  )
  
  out <- !is_empty
  
  return(out)
  
})

#' @rdname containsTracksDf
#' @export
setMethod(f = "containsTracksDf", signature = "CdataTimeLapseMP", definition = function(object, stop_if_false = FALSE){
  
  is_empty <- 
    purrr::map_int(.x = object@features_tracks, .f = base::nrow) %>% 
    base::all(. == 0)
  
  fdb_empty_slot(
    eval = is_empty,
    ref = "feature_stats", 
    stop_if_empty = stop_if_false
  )
  
  out <- !is_empty
  
  return(out)
  
})

#' @rdname containsTracksDf
#' @export
setMethod(f = "containsTracksDf", signature = "Cypro", definition = function(object, stop_if_false = FALSE){
  
  getCdata(object) %>% 
    containsTracksDf(stop_if_false = stop_if_false)
  
})

