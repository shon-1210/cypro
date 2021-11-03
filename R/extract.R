






# W -----------------------------------------------------------------------

#' @title Extract well plate information
#' 
#' @description Extracts information from the input vector regarding
#' the well plate layout by using regular expressions.
#' 
#' @inherit argument_dummy params
#' 
#' @return A character vector of the same length as the input vector.
#' @export
extractWellInfo <- function(vec, cypro_nc = FALSE){
  
  pattern <- 
    base::ifelse(
      test = base::isTRUE(cypro_nc), 
      yes = rgx_well_cypro, 
      no = rgx_well
    )
  
  stringr::str_extract(string = vec, pattern = pattern)
  
}


#' @rdname extractWellInfo
#' @export
extractRoiInfo <- function(vec, cypro_nc = FALSE){
  
  vec <- base::as.character(vec)
  
  pattern <- 
    base::ifelse(
      test = base::isTRUE(cypro_nc), 
      yes = rgx_roi_cypro, 
      no = rgx_roi
    ) %>% 
    stringr::str_c(., "$")
  
  stringr::str_extract(string = vec, pattern = pattern)
  
}

#' @rdname extractWellInfo
#' @export
extractWellRoiInfo <- function(vec, cypro_nc = FALSE){
  
  pattern <- 
    base::ifelse(
      test = base::isTRUE(cypro_nc), 
      yes = rgx_well_roi_cypro, 
      no = rgx_well_roi
    )
  
  stringr::str_extract(string = vec, pattern = pattern)
  
}

#' @rdname extractWellInfo
#' @export
extractWellRoiFileInfo <- function(vec, cypro_nc = FALSE){
  
  pattern <- 
    base::ifelse(
      test = base::isTRUE(cypro_nc), 
      yes = rgx_well_roi_file_cypro, 
      no = rgx_well_roi_file
    )
  
  stringr::str_extract(string = vec, pattern = pattern)
  
}

