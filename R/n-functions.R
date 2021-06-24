

#' @title Number of miscellaneous content
#' 
#' @description These functions return a numeric value describing the number
#' of \code{n*()} the object contains. (E.g. \code{nCells()} returns the total 
#' number of cells about which the object contains data, \code{nConditions()} 
#' returns the total number of conditions, etc. )
#'
#' @inherit argument_dummy params
#'
#' @return A numeric value. 
#' @export
#'

nCells <- function(object, drop_na = FALSE){
  
  check_object(object)
  
  getStatsDf(object, drop_na = drop_na) %>% 
    base::nrow()
  
}

#' @rdname nCells
#' @export
nCellLines <- function(object){
  
  check_object(object)
  
  getCellLines(object) %>% 
    base::length()
  
}

#' @rdname nCells
#' @export
nConditions <- function(object, phase = NULL){
  
  check_object(object)
  
  getConditions(object) %>% 
    base::length()
  
}



#' @title Number of files read in 
#' 
#' @inherit nCells description
#'
#' @inherit argument_dummy params 
#'
#' @return A numeric vector named by well plate. Values indicate the number of valid directories
#' that have been read in via \code{loadData()}
#' 
#' @export
#'
nFiles <- function(object){
  
  check_object(object, set_up_req = "load_data")
  
  purrr::map_int(
    .x = object@well_plates, 
    .f = ~ base::length(.x$valid_directories)
  ) %>% 
  purrr::set_names(
    x = ., 
    nm = getWellPlateNames(object)
  )
  
}



#' @title Number of NAs by cell id  (in tracks)
#' 
#' @description Returns the number of missing values of every track
#' variable denoted in \code{variable_names} by cell id.
#'
#' @inherit argument_dummy params
#' 
#' @return A summarized data.frame. Each row contains the number of missing 
#' values a cell has in the respective variable. 
#' 
#' @export
#'

nMissingValuesTracks <- function(object,
                                 variable_names,
                                 phase = NULL,
                                 verbose = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase)
  
  if(multiplePhases(object)){
    
    df <- 
      purrr::map_df(.x = object@qcheck$na_count[phase], .f = ~ .x) %>% 
      dplyr::select(cell_id, -dplyr::all_of(non_data_track_variables))
    
    if(base::length(phase) > 1){
      
      msg <- 
        glue::glue(
          "Computing sum of NAs of {ref_phase} phase.",
          ref_phase = confuns::scollapse(phase)
          )
      
      confuns::give_feedback(msg = msg, verbose = verbose)
      
      df <- 
        dplyr::group_by(df, cell_id) %>% 
        dplyr::summarise_all(.funs = base::sum)
      
    }
    
  } else {
    
    df <- object@qcheck$na_count
    
  }
  
  return(df)
  
}


#' @title Number of NAs by cell id  (in stats)
#' 
#' @description Sums up the number of missing values every cell id has 
#' in the stats data.frame.
#'
#' @inherit argument_dummy params
#' @param variable_names Character vector or NULL. If character, denotes 
#' the track variables of interest. If NULL all of them are chosen. 
#' 
#' @return A summarized data.frame. Variable \emph{na_count} refers to 
#' the number of NAs across all stat variables. E.g a value of 8 in \emph{na_count}
#' means that the respective cell features missing values in 8 stat variables. 
#' 
#' @export
#'
nMissingValuesStats <- function(object, phase = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  mtr <-
    getStatsDf(
      object = object, 
      phase = phase, 
      with_grouping = FALSE, 
      verbose = FALSE
    ) %>% 
    tibble::column_to_rownames(var = "cell_id") %>% 
    base::as.matrix()
  
  df <- 
    base::is.na(mtr) %>% 
    base::rowSums() %>% 
    base::as.data.frame() %>% 
    magrittr::set_colnames(value = "stat_na_count") %>% 
    tibble::rownames_to_column(var = "cell_id") %>% 
    tibble::as_tibble()
  
  return(df)
  
}


