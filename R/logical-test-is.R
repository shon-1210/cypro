



# L -----------------------------------------------------------------------

#' @title Test layout data.frame
#' 
#' @description Tests if the input data.frame is of class \code{layout_df}.
#'
#' @param df A data.frame.
#'
#' @return TRUE or FALSE.
#' @export
#'
isLayoutDf <- function(df){
  
  if("layout_df" %in% base::class(df)){
    
    res <- TRUE
    
  } else {
    
    res <- FALSE
    
  }
  
  return(res)
  
}

# M -----------------------------------------------------------------------

#' @title Test layout data.frame modification
#' 
#' @description Checks if the input data.frame has been modified by \code{setWellInfo()}.
#' 
#' @param df  A data.frame.
#' 
#' @return TRUE or FALSE.
#' 
isModifiedLayoutDf <- function(df){
  
  base::stopifnot(isLayoutDf(df))
  
  cell_line <- base::any(!base::is.na(df$cell_line))
  condition <- base::any(!base::is.na(df$condition))
  
  modified <- base::any(c(cell_line, condition))
  
  return(modified)  
  
}

#' @title Test multiple phases
#' 
#' @description Tests if the provided object derives from a multiple phase experiment.
#'
#' @inherit argument_dummy params
#'
#' @return TRUE or FALSE
#' @export
#'
setGeneric(name = "isMultiplePhase", def = function(object, stop_if_false = FALSE){
  
  standardGeneric(f = "isMultiplePhase")
  
})


#' @rdname isMultiplePhase
#' @export
setMethod(f = "isMultiplePhase", signature = "WellPlate", function(object, stop_if_false = FALSE){
  
  res <- base::is.numeric(base::attr(object@layout, "n_phases"))
  
  if(base::isFALSE(res) && base::isTRUE(stop_if_false)){
    
    stop("Well plate '{object@name}' of '{object@experiment}' does not derive from multiple phase experiment.")
    
  }
  
  return(res)
  
})

#' @rdname isMultiplePhase
#' @export
setMethod(f = "isMultiplePhase", signature = "ExperimentDesign", function(object, stop_if_false = FALSE){
  
  res <- object@type == "CyproTimeLapseMP"
  
  if(base::isFalse(res) && base::isTRUE(stop_if_false)){
    
    stop("Experiment design of '{object@experiment}' does not include multiple phases.")
    
  }
  
  return(res)
  
  
})

#' @rdname isMultiplePhase
#' @export
setMethod(f = "isMultiplePhase", signature = "Cypro", function(object, stop_if_false = FALSE){
  
  res <- methods::is(object, class2 = "CyproTimeLapseMP")
  
  if(base::isFALSE(res) && base::isTRUE(stop_if_false)){
    
    stop("Experiment design of '{object@name}' does not include multiple phases.")
    
  }
  
  return(res)
  
})


# S -----------------------------------------------------------------------

#' @title Test single phase
#' 
#' @description Tests if the provided object derives from a single phase experiment.
#'
#' @inherit argument_dummy params
#'
#' @return TRUE or FALSE
#' @export
#'
setGeneric(name = "isSinglePhase", def = function(object, ...){
  
  standardGeneric(f = "isSinglePhase")
  
})

#' @rdname isSinglePhase
#' @export
setMethod(f = "isSinglePhase", signature = "WellPlate", function(object, stop_if_false = FALSE){
  
  res <- bas::is.null(base::attr(object@layout, "n_phases"))
  
  if(base::isFALSE(res) && base::isTRUE(stop_if_false)){
    
    stop("Well plate '{object@name}' of '{object@experiment}' derives from multiple phase experiment.")
    
  }
  
  return(res)
  
})

#' @rdname isSinglePhase
#' @export
setMethod(f = "isSinglePhase", signature = "ExperimentDesign", function(object, stop_if_false = FALSE){
  
  res <- object@type != "CyproTimeLapseMP"
  
  if(base::isFalse(res) && base::isTRUE(stop_if_false)){
    
    stop("Experiment design of '{object@experiment}' includes multiple phases.")
    
  }
  
  return(res)
  
  
})

#' @rdname isSinglePhase
#' @export
setMethod(f = "isSinglePhase", signature = "Cypro", function(object, stop_if_false = FALSE){
  
  res <- !methods::is(object, class2 = "CyproTimeLapseMP")
  
  if(base::isFALSE(res) && base::isTRUE(stop_if_false)){
    
    stop("Experiment design of '{object@experiment}' includes multiple phases.")
    
  }
  
  return(res)
  
})

