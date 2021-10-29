


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



#' @title Tests for analysis modules
#' 
#' @description A variety of tests that check for certain module specifics.
#' 
#' @inherit argument_dummy params
#' 
#' @return TRUE or FALSE
#' 

setGeneric(name = "containsComputableVariables", def = function(object){
  
  standardGeneric(f = "containsComputableVariables")
  
})

#' @rdname containsComputableVariables
#' @export

setMethod(f = "containsComputableVariables", signature = "AnalysisModule", definition = function(object){
  
  res <- base::length(object@variables_required) >= 1
  
  return(res)
  
})

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
