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

existOutlierResults <- function(object, phase = NA, method_outlier = NA, verbose = FALSE){
  
  outlier_results <- object@qcheck$outlier_detection
  
  
  # check if method has been used
  if(!base::is.na(method_outlier)){
  
    confuns::is_value(method_outlier, mode = "character")
      
    outlier_results <- outlier_results[[method_outlier]]
    
    if(base::is.null(outlier_results)){
      
      if(base::isTRUE(verbose)){
        
        msg <- glue::glue("Did not find outlier results for method '{method_outlier}'.")
        
        stop(msg)
        
      } else {
        
        return(FALSE)
        
      }
      
    } else {
      
      if(!base::is.na(phase) & multiplePhases(object)){
        
        phase <- check_phase(object, phase = phase, max_phases = 1)
        
        outlier_results <- outlier_results[[phase]]
        
        if(base::is.null(outlier_results)){
          
          if(base::isTRUE(verbose)){
            
            msg <- glue::glue(
              "Did not find outlier results for method '{method_outlier}' and ", 
              "{phase} phase."
            )
            
            stop(phase)
            
          } else {
            
            return(FALSE)
            
          }
          
        }
        
      }
      
    }
    
  } else if(!base::is.na(phase) & multiplePhases(object)){
    
    res_list <- 
      purrr::map(.x = outlier_results, .f = ~ .x[[phase]]) %>%
      purrr::discard(.p = base::is.null)
    
    if(base::length(res_list) == 0){
      
      if(base::isTRUE(verbose)){
        
        msg <- glue::glue(
          "Did not find any outlier_results for {phase} phase."
        )
        
        stop(msg)
        
      } else {
        
        return(FALSE)
        
      }
      
    }
    
  }
  
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

#' @rdname existOutlierResults
#' @export
isNormalizedWithZscore <- function(object){
  
  res <- base::is.list(object@information$normalization$zscore)
  
  return(res)
  
}



# not exported ------------------------------------------------------------





