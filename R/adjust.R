

# A -----------------------------------------------------------------------

#' @title Adjust well naming conventions
#' 
#' @description Converts all values of the provided vectors to 
#' the proper naming convention. 
#' 
#' @param wells Character vector of wells.
#' @param well_rois Character vector of well-rois.
#' 
#' @details The naming convention that the \code{cypro}-package 
#' follows is \emph{A1, A2, A3, ... A10, A11, ..., B1, B2} \bold{not}
#' \emph{A01, A02, A03, ..., A10, A11}. The inherent order of 
#' the wells is ensured by storing well and well ROIs as factors 
#' in the \code{Cypro} object with corresponding levels. 
#' 
#' @seealso \code{getWellLevels()}
#' 
#' @export
#' 
#' @examples 
#' 
#' well_vec <- c('B01', 'B02', 'B3')
#' 
#' adjustWellInfo(vec = well_vec)
#' 
#' well_rois_vec <- c('B1_1', 'B2_1', 'B3_01')
#' 
#' adjustWellRoiInfo(vec = well_rois_vec)
#' 

adjustWellInfo <- function(vec){
  
  well_vec <- extractWellInfo(vec = vec)
  
  row_letters <- stringr::str_extract(string = well_vec, pattern = "[A-Z]")
  
  col_numbers <-
    stringr::str_extract(string = well_vec, pattern = "\\d{1,2}$") %>% 
    stringr::str_remove(pattern = "^0")
  
  clean_well_vec <- stringr::str_c(row_letters, col_numbers, sep = "")
  
  return(clean_well_vec)
  
}


#' @rdname adjustWellInfo
#' @export
adjustWellRoiInfo <- function(vec){
  
  well_roi_vec <- vec
  
  well_vec <- 
    extractWellInfo(well_roi_vec) %>% 
    adjustWellInfo()
  
  roi_vec <- 
    stringr::str_extract(string = well_roi_vec, pattern = "\\d{1,2}$") %>% 
    stringr::str_remove(pattern = "^0")
  
  clean_well_roi_vec <- 
    stringr::str_c(well_vec, roi_vec, sep = "_")
  
  return(clean_well_roi_vec)
  
}

#' @rdname adjustWellInfo
#' @export
adjustRoiInfo <- function(vec){
  
  base::as.character(vec) %>% 
    stringr::str_remove(pattern = "^0")
  
}
