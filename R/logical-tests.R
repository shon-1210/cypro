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

  res <- !methods::is(object, class2 = "CyproScreening")
    
  return(res)
  
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

# candidate check (for picker outputs) return TRUE or FALSE
is_grouping_candidate <- function(var){
  
  var <- var[!base::is.na(var)]
  
  if(base::is.character(var) | base::is.factor(var)){
    
    res <- TRUE
    
    # if not character of factor -> numeric
  } else if(!base::any(stringr::str_detect(var, pattern = "\\."))){ # . in var -> double
    
    res <- TRUE
    
  } else {
    
    res <- FALSE
    
  }
  
  return(res)
  
}

is_numeric_candidate <- function(var){
  
  if(base::is.numeric(var)){
    
    res <- TRUE
    
  } else if(base::is.character(var)){
    
    n_na <- base::is.na(var) %>% base::sum()
    
    var_numeric <- base::suppressWarnings({ base::as.numeric(var) })
    
    n_na_new <- base::is.na(var_numeric) %>% base::sum()
    
    if(n_na_new > n_na){
      
      res <- FALSE
      
    } else {
      
      res <- TRUE
      
    }
    
  } else {
    
    res <- FALSE
    
  }
  
  return(res)
  
}


isOfClass <- function(x, valid_class, stop_if_false = FALSE, ref_x = NULL){
  
  res <- methods::is(x, class2 = valid_class)
  
  if(base::isTRUE(stop_if_false) && base::isFALSE(res)){
    
    if(!base::is.character(ref_x)){
      
      ref_x <- base::substitute(expr = x)
      
    }
    
    stop(
      glue::glue(
        "Input {ref_x} must be of class '{valid_class}' but is of class '{base::class(x)}'."
      )
    )
    
  }
  
  return(res)
  
}


