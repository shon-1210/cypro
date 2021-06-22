

#' @title Valid default arguments
#' 
#' @description Obtain a character vector of argument names whose 
#' default input you can adjust via \code{adjustDefaultInstructions()}.
#'
#' @return Character vector. 
#' @export
#'
adjustableDefaultInstructions <- function(){
  
  base::return(base::names(default_list))
  
}


#' @title Protected variables
#' 
#' @description Obtain a character vector of names from variables
#' that must not be renamed and whose content must not be changed. 
#' 
#' @return Character vector. 
#' 
#' @export
#' 
protectedVariables <- function(){
  
  protected_vars_all  
  
}
  

#' @title Valid input options
#' 
#' @description Helper functions that to not take any arguments but simply return 
#' a character vector of valid input options for certain arguments. 
#'
#' @return Character vector.
#' @export
#'
validAgglomerationMethods <- function(){
  
  confuns::valid_methods_aggl
  
}


#' @rdname validAgglomerationMethods
#' @export
validColorPalettes <- function(){
  
  confuns::all_color_palettes()
  
}

#' @rdname validAgglomerationMethods
#' @export
validColorSpectra <- function(){
  
  confuns::all_color_spectra()
  
}

#' @rdname validAgglomerationMethods
#' @export
validDistanceMethods <- function(){
  
  confuns::valid_methods_dist
  
}

#' @rdname validAgglomerationMethods
#' @export
validKmeansMethods <- function(){
  
  confuns::valid_methods_kmeans
  
}

