


# A -----------------------------------------------------------------------


#' @title Number of ambiguous file directories
#' 
#' @description Returns the number of ambiguous file directories in 
#' the folder assigned to slot @@directory. 
#'
#' @inherit argument_dummy params
#' 
#' @details If the number of files in slot @@file of \code{WellPlate} object
#' is zero the function returns NA.
#'
#' @return A numeric value.
#' @export
#'
setGeneric(name = "nAmbiguousFiles", def = function(object){
  
  standardGeneric(f = "nAmbiguousFiles")
  
})


#' @rdname nAmbiguousFiles
#' @export
setMethod(f = "nAmbiguousFiles", signature = "WellPlate", definition = function(object){
  
  layout_df <- getLayoutDf(object) %>% unnestLayoutDf()
  
  out <- base::sum(layout_df$file_status == "Ambiguous")
  
  return(out)
  
})

# C -----------------------------------------------------------------------

#' @title Number of miscellaneous content
#' 
#' @description These functions return a numeric value describing the number
#' of \code{n*()} the object contains. (E.g. \code{nCells()} returns the total 
#' number of cells about which the object contains data, \code{nConditions()} 
#' returns the total number of conditions, etc. )
#'
#' @inherit argument_dummy params
#'
#' @return A numeric value. If \code{across} is specified a data.frame.
#' @export
#'

nCells <- function(object, across = NULL, drop_na = FALSE, phase = NULL, ...){
  
  check_object(object)
  
  assign_default(object)
  
  confuns::is_value(x = across, mode = "character", skip.all = TRUE, skip.val = NULL)
  
  phase <- check_phase(object, phase = phase, max_phase = 1)
  
  stat_df <- getStatsDf(object, drop_na = drop_na, phase = phase, ...)
  
  if(base::is.null(across)){
    
    res <- base::nrow(stat_df)
    
  } else if(base::is.character(across)){
 
    confuns::check_one_of(
      input = across, 
      against = getGroupingVariableNames(object, phase = phase)
    )
    
    res <- 
      dplyr::group_by(stat_df, dplyr::across(.cols = dplyr::all_of(across))) %>% 
      dplyr::tally()
    
  }
  
  return(res)
  
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



# E -----------------------------------------------------------------------

#' @title Number of expected files
#'
#' @description Returns the number of files that is expected according to 
#' the number of regions of interest, the number of wells with a complete information 
#' status and the loading modality chosen - according to the input example. 
#'
#' @inherit argument_dummy params
#'
#' @return A numeric value. 
#' @export
#'
setGeneric(name = "nExpectedFiles", def = function(object){
  
  standardGeneric(f = "nExpectedFiles")
  
})


#' @rdname nExpectedFiles
#' @export

setMethod(f = "nExpectedFiles", signature = "WellPlate", definition = function(object){
  
  base::stopifnot(isOfLength(object@loading_modality, l = 1))
  
  ldm <- object@loading_modality
  
  if(ldm == "by_roi"){
    
    n_files <- nRois(object) * nWells(object, info_status = "Complete")
    
  } else if(ldm == "by_well"){
    
    n_files <- nWells(object, info_status = "Complete")
    
  } else {
    
    n_files <- 1
    
  }
  
  return(n_files)
  
})




# F -----------------------------------------------------------------------

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


#' @title Number of frames
#' 
#' @description Returns the total number of frames as was denoted during \code{designExperiment()}.
#' 
#' @inherit argument_dummy params
#' 
#' @return A numeric value. 
#' @export

setGeneric(name = "nFrames", def = function(object){
  
  standardGeneric(f = "nFrames")
  
})

#' @rdname nFiles
#' @export
setMethod(f = "nFrames", signature = "ExperimentDesign", definition = function(object){
  
  object@n_frames
  
})

#' @rdname nFiles
#' @export
setMethod(f = "nFrames", signature = "Cypro", definition = function(object){
  
  getExperimentDesign(object) %>% 
    nFrames()
  
})


# M -----------------------------------------------------------------------


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


# R -----------------------------------------------------------------------

#' @title Number of regions of interest
#' 
#' @description Returns the number of regions of interest set during 
#' \code{designExperiment()}.
#'
#' @param object 
#'
#' @return Numeric value.
#' @export
#'
setGeneric(name = "nRois", def = function(object){
  
  standardGeneric(f = "nRois")
  
})

#' @rdname nRois
#' @export
setMethod(f = "nRois", signature = "WellPlate", function(object){
  
  out <- 
    getLayoutDf(object = object) %>% 
    dplyr::pull(var = "n_rois") %>% 
    base::unique()
  
  return(out)
  
})

#' @rdname nRois
#' @export
setMethod(f = "nRois", signature = "layout_df", function(object){
  
  base::unique(object$n_rois)
  
})



# W -----------------------------------------------------------------------



#' @title Number of well plates
#' 
#' @description Returns the number of well plates the experiment 
#' design includes.
#'
#' @inherit argument_dummy params
#' 
#' @return Numeric value.
#' 
#' @export
#'

setGeneric(name = "nWellPlates", def = function(object){
  
  standardGeneric(f = "nWellPlates")
  
})

#' @rdname nWellPlates
#' @export
setMethod(f = "nWellPlates", signature = "ExperimentDesign", definition = function(object){
  
  base::length(object@well_plates)
  
})

#' @rdname nWellPlates
#' @export
setMethod(f = "nWellPlates", signature = "Cypro", definition = function(object){
  
  res <- 
    getExperimentDesign(object) %>% 
    nWellPlates()
  
  return(res)
  
})




#' @title Number of wells and well rois
#' 
#' @description Returns the absolute number of wells or well rois of a well plate.
#'
#' @inherit argument_dummy params 
#'
#' @return Numeric value.
#' @export

setGeneric(name = "nWells", def = function(object, info_status = info_status_levels){
  
  standardGeneric(f = "nWells")
  
})


#' @rdname nWells
#' @export
setMethod(f = "nWells", signature = "layout_df", definition = function(object, info_status = info_status_levels){
  
  df <- nestLayoutDf(object)
    
  base::nrow(df[df$info_status %in% info_status, ])
  
})

#' @rdname nWells
#' @export
setMethod(f = "nWells", signature = "WellPlate", definition = function(object, info_status = info_status_levels){
  
  df <- getLayoutDf(object)
  
  nWells(object = df, info_status = info_status)
  
})

#' @rdname nWells
#' @export
setGeneric(name = "nWellRois", def = function(object, info_status = info_status_levels){
  
  standardGeneric(f = "nWellRois")
  
})

#' @rdname nWells
#' @export
setMethod(f = "nWellRois", signature = "layout_df", definition = function(object, info_status = info_status_levels){
  
  df <- unnestLayoutDf(object)
  
  base::nrow(df[df$info_status %in% info_status, ])
  
})