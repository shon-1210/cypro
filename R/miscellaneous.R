

#' @title Filter track data.frame
#' 
#' @description Filters a track data.frame according to the parameters 
#' set.
#'
#' @inherit check_track_df params
#' @param n_cells Numeric value. The number of cells the resulting data.frame
#' contains. 
#' @param condition  Character vector. The conditions to be kept. 
#' @param cell_line Character vector. The cell lines to be kept. 
#'
#' @return A filtered track data.frame 

filterTracks <- function(track_df,
                         n_cells = 100,
                         condition = NULL,
                         cell_line = NULL){
  
  if(!base::is.null(condition)){
    
    track_df <-
      dplyr::filter(track_df, condition %in% {{condition}})
    
  }
  
  if(!base::is.null(cell_line)){
    
    track_df <- 
      dplyr::filter(track_df, cell_line %in% {{cell_line}})
    
  }
  
  cell_ids <- base::unique(track_df$cell_id)
  
  sample_ids <- base::sample(x = cell_ids, size = n_cells)
  
  result_df <- 
    dplyr::filter(track_df, cell_id %in% sample_ids)
  
  base::return(result_df)
  
}


#' @title Function to make well vectors compatible with cypros terminology
#' @export
adjust_well_vec <- function(well_vec){
  
  row_letters <- stringr::str_extract(string = well_vec, pattern = "[A-Z]")
  
  col_numbers <-
    stringr::str_extract(string = well_vec, pattern = "\\d{1,2}$") %>% 
    stringr::str_remove(pattern = "^0")
  
  clean_well_vec <- stringr::str_c(row_letters, col_numbers, sep = "")

  return(clean_well_vec)
    
}

#' @title Function to make well roi vectors compatible with cypros terminology
#' @export
adjust_well_roi_vec <- function(well_roi_vec){
  
  well_vec <-
    stringr::str_extract(string = well_roi_vec, pattern = "[A-Z]\\d{1,2}") %>% 
    adjust_well_vec()
  
  roi_vec <- 
    stringr::str_extract(string = well_roi_vec, pattern = "\\d{1,2}$") %>% 
    stringr::str_remove(pattern = "^0")
  
  clean_well_roi_vec <- 
    stringr::str_c(well_vec, roi_vec, sep = "_")
  
  return(clean_well_roi_vec)
  
}
