

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



# a -----------------------------------------------------------------------


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



# c -----------------------------------------------------------------------

count_valid_dirs <- function(vec){
  
  not_na <- !base::is.na(vec)
  
  n_not_na <- base::sum(not_na)
  
  return(n_not_na)
  
}



# d -----------------------------------------------------------------------

data_status_by_well <- function(vec){
  
  if(base::all(vec == 1)){
    
    out <- "Complete"
    
  } else if(base::any(vec > 1)){
    
    out <- "Ambiguous"
    
  } else if(base::all(vec == 0)){
    
    out <- "Missing"
    
  } else if(base::any(vec == 0)){
    
    out <- "Incomplete"
    
  }
  
  return(out)
  
  
}

# e -----------------------------------------------------------------------


# f -----------------------------------------------------------------------

file_dir_validity <- function(dir){
  
  if(base::length(dir) == 0){
    
    out <- "Missing"
    
  } else if(base::length(dir) > 1){
    
    out <- "Ambiguous"
    
  } else if(base::is.na(dir)){
    
    out <- "Missing"
    
  } else if(base::length(1) & base::is.character(dir)){
    
    out <- "Valid"
    
  }
  
  return(out)
  
}

flatten_folder_path <- function(input_dir){
  
  root <-
    stringr::str_remove(input_dir$root, pattern = "\\(") %>% 
    stringr::str_remove(pattern = "\\)")
  
  path <-
    purrr::map_chr(.x = input_dir$path, ~ base::return(.x)) %>% 
    purrr::discard(.p = ~ .x == "") %>% 
    stringr::str_c(collapse = "/") %>% 
    stringr::str_c(root, ., sep = "/")
  
  return(path)
  
}



# n -----------------------------------------------------------------------





#' @title Shift scope of well plate layout data.frame
#' 
#' @description Shift between layout data.frame scopes. 
#' 
#' @param df A data.frame of class \code{layout_df}.
#' 
#' @details In nested layout data.frames every row represents a well.
#' They contain a list column named \emph{roi_info} that contains additional
#' information regarding the regions of interest of eachwell. 
#' 
#' In unnested layout data.frames every row represents a region of interest of
#' a well. The list column \emph{roi_info} has been unnested into it's parts.
#' 
#' Which variables belong to \emph{roi_info} is stored in the layout data.frames 
#' attributes and can change based on the experiment design and data loading modality. 

nestLayoutDf <- function(df){
  
  UseMethod(generic = "nestLayoutDf", object = df)
  
}

#' @rdname nestLayoutDf
#' @export
nestLayoutDf.layout_df <- function(df){
  
  if(!isNested(df)){
  
    attr <- getLayoutAttributes(df)
    
    add_cols <- character(0)
    
    if("dir" %in% base::names(df)){
      
      add_cols <- c(add_cols, "dir")
      
    }
    
    df <- tidyr::nest(df, roi_info = c("well_roi", "roi", add_cols))
    
    df <- setLayoutAttributes(df, attr = attr)
  
  }
  
  return(df)
  
}


# s -----------------------------------------------------------------------

#' @title Suggest the proper loading function
#' 
#' @description Suggests the loading function to use depending on the 
#' example data and the variable assignment during \code{assignVariables()}.
#' 
#' @inherit argument_dummy params
#' 
#' @return Character value.
#' 
#' @export

setGeneric(name = "suggestLoadingModality", def = function(object){
  
  standardGeneric(f = "suggestLoadingModality")
  
})


#' @rdname suggestLoadingModality
#' @export
setMethod(f = "suggestLoadingModality", signature = "Cypro", definition = function(object){
  
  base::stopifnot(base::isTRUE(object@progress@assignVariables))
  
  example_df <- getExampleDf(object)
  
  id_assignment <- getVariableAssignmentID(object, drop_na = F)
  
  optional_vars <- id_assignment[c("well_plate", "well", "roi")]
  
  if(base::all(base::is.na(optional_vars))){
    
    res <- "by_roi"
    
  } else if(base::is.na(optional_vars["roi"])) {
    
    res <- "by_roi"
    
  } else if(base::is.na(optional_vars["well"])){
    
    res <- "by_well"
    
  } else if(base::is.na(optional_vars["well_plate"])){
    
    res <- "by_well_plate"
    
  } else {
    
    res <- "by_file"
    
  }
  
  return(res)
  
})





# u -----------------------------------------------------------------------

#' @rdname nestLayoutDf
#' @export
unnestLayoutDf <- function(df){
  
  UseMethod(generic = "unnestLayoutDf", object = df)
  
}

#' @rdname nestLayoutDf
#' @export
unnestLayoutDf.layout_df <- function(df){
  
  if(isNested(df)){
    
    attr <- getLayoutAttributes(df)
    
    df <- 
      tidyr::unnest(df, cols = "roi_info") %>% 
      dplyr::select(
        well_plate_name, row_num, col_num, row_letter, well, well_roi, roi,
        dplyr::everything()
      )
    
    df <- setLayoutAttributes(df, attr = attr)
    
  }
  
  return(df)
  
}
