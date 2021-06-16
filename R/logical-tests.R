#' @include cypro-modules.R
NULL

#' @title Logical Tests
#' 
#' @description Logical tests that return TRUE if the specified functions/algorithms
#' have been used on the object and FALSE if not. 
#'
#' @inherit argument_dummy params
#'
#' @return TRUE or FALSE
#' @export

existOutlierResults <- function(object){
  
  outlier_results <- object@analysis$outlier_detection
  
  res <- !base::identical(outlier_results, y = base::list()) & !base::is.null(outlier_results)
  
  base::return(res)
  
}

#' @rdname existOutlierResults
#' @export
isTimeLapseExp <- function(object){
  
  return(object@set_up$experiment_type == "time_lapse")
  
}

#' @rdname existOutlierResults
#' @export
isUsable <- function(object, module, verbose = FALSE){
  
  if(base::is.list(object@modules[[module]])){
    
    res <- TRUE
    
  } else {
    
    res <- FALSE
    
    if(base::isTRUE(verbose)){
      
      stop(glue::glue("Module '{module}' is not usable with this cypro object."))
      
    }
    
  }
  
  base::return(res)
  
}

#' @rdname existOutlierResults
#' @export
multiplePhases <- function(object){
  
  if(base::length(getPhases(object)) == 1){
    
    base::return(FALSE)
    
  } else {
    
    base::return(TRUE)
    
  }
  
}





# not exported ------------------------------------------------------------





