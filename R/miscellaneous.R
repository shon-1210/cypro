

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

#' @title Complete tracks data.frame
#' 
#' @description Completes the track data.frame by adding missing \emph{cell_id} +
#' \emph{frame} observations.
#'
#' @inherit argument_dummy params
#'
#' @return The input object. 
#' @export
setGeneric(name = "completeTracksDf", def = function(object, verbose = TRUE){
  
  standardGeneric(f = "completeTracksDf")
  
})

#' @rdname completeTracksDf
#' @export
setMethod(
  f = "completeTracksDf",
  signature = "CyproTimeLapse",
  definition = function(object, verbose = TRUE){
    
    df <- getTracksDf(object, with_well_plate = TRUE)
    
    give_feedback(msg = "Completing tracks data.frame by observation.", verbose = verbose)
    
    all_cell_ids <- base::unique(df$cell_id)
    
    all_frames <- getFrames(object) 
    
    id_df <- 
      dplyr::select(df, cell_id, well_plate_name, well_plate_index, well, roi) %>% 
      dplyr::distinct()
    
    incomplet_df <- dplyr::select(df, -well_plate_name, -well_plate_index, -well, -roi)
    
    tracks_df <- 
      tidyr::expand_grid(cell_id = {{all_cell_ids}}, frame_num = {{all_frames}}) %>% 
      dplyr::left_join(x = ., y = incomplet_df, by = c("cell_id", "frame_num")) %>% 
      dplyr::left_join(x = id_df, y = ., by = "cell_id") %>% 
      dplyr::mutate(frame_added = tidyr::replace_na(frame_added, replace = TRUE)) %>% 
      dplyr::select(-dplyr::any_of(var_names_dfs$well_plate)) %>% 
      dplyr::select(cell_id, dplyr::all_of(var_names_dfs$tracks), dplyr::everything())
    
    object <- setTracksDf(object, df = tracks_df)
    
    return(object)
    
  })


count_valid_dirs <- function(vec){
  
  not_na <- !base::is.na(vec)
  
  n_not_na <- base::sum(not_na)
  
  return(n_not_na)
  
}


cypro_object_ready <- function(object, verbose = TRUE){
  
  give_feedback(
    msg = glue::glue("{base::class(object)} object '{object@experiment}' is all set for downstream analysis."), 
    verbose = verbose
  )
  
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


#' @title Empty data files 
#' 
#' @description Empties slot @@content of objects of class \code{DataFile}.
#'
#' @inherit argument_dummy params
#' @param transferred Logical value. Indicates that the emptying takes place due to 
#' the content beeing transferred to an object of class \code{Cdata}.
#' 
#' @details Empties slot @@content of objects of class \code{DataFile} that were
#' created for data loading and sets slot @@transferred to TRUE indicating 
#' that the content has been merged via \code{transferData()} and transferred
#' to the \code{Cypro} objects @@cdata slot in form of proper \code{cypro_df} data.frames.
#'
#' @return The adjusted input object. 
#' @export
#'
setGeneric(name = "emptyDataFiles", def = function(object, ...){
  
  standardGeneric(f = "emptyDataFiles")
  
})

#' @rdname emptyDataFiles
#' @export
setMethod(f = "emptyDataFiles", signature = "WellPlate", definition = function(object, transferred = TRUE){
  
  object@files <- 
    purrr::map(object@files, .f = function(object){
      
      if(!containsErrors(object)){
        
        object@content <- list()
        object@transferred <- transferred
        
      }
      
      return(object)
      
    })
  
  return(object)
  
})

#' @rdname emptyDataFiles
#' @export
setMethod(f = "emptyDataFiles", signature = "ExperimentDesign", definition = function(object, transferred = TRUE){
  
  object@well_plates <- 
    purrr::map(
      .x = object@well_plates, 
      .f = ~ emptyDataFiles(.x)
    )
  
  return(object)
  
})

#' @rdname emptyDataFiles
#' @export
setMethod(f = "emptyDataFiles", signature = "Cypro", definition = function(object, transferred = TRUE){
  
  exp_design <- 
    getExperimentDesign(object) %>% 
    emptyDataFiles()
  
  object <- 
    setExperimentDesign(object, exp_design = exp_design)
  
  return(object)
  
})


# f -----------------------------------------------------------------------

fdb_empty_slot <- function(eval, ref, stop_if_empty = FALSE){
  
  if(base::isTRUE(eval) && stop_if_empty){
    
    give_feedback(
      msg = glue::glue("Slot @{ref} is empty."), 
      fdb.fn = "stop", 
      with.time = FALSE
    )
    
  }
  
  return(eval)
  
}

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



# j -----------------------------------------------------------------------


#' @title Join cell data frames
#' 
#' @description Joins different variable types of the cell data to one
#' data.frame by the key variable \emph{cell_id}.
#'
#' @inherit argument_dummy params
#' @param df A data.frame of class \code{cypro_df}.
#'
#' @return A data.frame.
#' @export
#'
setGeneric(name = "joinWith", def = function(object, 
                                             df, 
                                             with_cluster = FALSE, 
                                             with_meta = FALSE, 
                                             with_well_plate = FALSE){
  
  standardGeneric(f = "joinWith")
  
})

#' @rdname joinWith
#' @export
setMethod(f = "joinWith", signature = "Cdata", definition = function(object,
                                                                     df,
                                                                     with_cluster = FALSE, 
                                                                     with_meta = FALSE, 
                                                                     with_well_plate = FALSE){
  
  base::stopifnot(isOfClass(df, "cypro_df"))
  
  if(base::isTRUE(with_cluster)){
    
    df <- dplyr::left_join(x = df, y = object@cluster, by = "cell_id")
    
  }
  
  if(base::isTRUE(with_meta)){
    
    df <- dplyr::left_join(x = df, y = object@meta, by = "cell_id")
    
  }
  
  if(base::isTRUE(with_well_plate)){
    
    df <- dplyr::left_join(x = df, y = object@well_plate, by = "cell_id")
    
  }
  
  df <- tibble::as_tibble(df)
  
  return(df)
  
})


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
    
    df <- tidyr::nest(df, roi_info = c(attr$roi_info_vars))
    
    df <- setLayoutAttributes(df, attr = attr)
  
  }
  
  return(df)
  
}




# p -----------------------------------------------------------------------

#' @title Data loading - Process data
#' 
#' @description Third of three data loading steps A pipeline that does all
#' the processing after the data has been loaded via \code{loadDataFiles()}.
#'  See details fore more.
#'
#' @inherit argument_dummy params 
#' 
#' @details Depending on the method the pipeline consists of the following steps:
#' 
#' \code{transferData()}: All files that have been loaded with \code{loadDataFiles()} without 
#' any error are merged to a data.frame. Depending on the loading modality - \emph{by_roi},
#' \emph{by_well}, \emph{by_well_plate} - the missing ID variables are added to the 
#' data.frame. It is then split into the functional parts the \code{Cdata} object
#' that corresponds to the experiment design determines. The \code{Cdata} object 
#' is then set in slot @@cdata of the \code{Cypro} object.
#' 
#' \code{completeTracksDf()}: The method for time lapse experiments completes the data.frame by adding 
#' observations were \emph{cell_id}+\code{frame_num} combinations are missing (due
#' to failed recognition of a cell in one ore more frames). Values of numeric variables
#' are filled with NAs for these observations. Which observation was added is indicated
#' by the variable \emph{frame_added}. 
#' 
#' \code{computeModuleVariabels()}: Computable, module related variables that were
#' not assigned during \code{assignVariables()} are computed and added to the feature
#'  data.frame \code{CyproScreening} / tracks data.frame
#' \code{CyproTimeLapse()}. 
#' 
#' \code{summarizeDataByCellId()}: The method for time lapse experiments 
#' summarizes the time lapse data by cell ID to create the stats data.frame.
#' 
#' \code{summarizeModuleVariables()}: The method for time lapse experiments
#' additionally summarizes all \code{SummaryVariables} of the used analysis modules
#'  (e.g. \emph{mgr_efficiency}) and adds them to the stats data.frame.
#' 
#' The output is a \code{Cypro} object that is ready for analysis.
#'
#' @return The input object. 
#' @export

setGeneric(name = "processData", def = function(object, verbose = TRUE){
  
  standardGeneric(f = "processData")
  
})


#' @rdname processData
#' @export
setMethod(f = "processData", signature = "CyproScreening", definition = function(object, verbose = TRUE){
  
  object <- 
    transferData(object, verbose = verbose) %>% 
    computeModuleVariables(verbose = verbose)
  
  object <- setProgress(object, processData = TRUE)
  
  cypro_object_ready(object = object, verbose = verbose)
  
  return(object)
  
})

#' @rdname processData
#' @export
setMethod(f = "processData", signature = "CyproTimeLapse", definition = function(object, verbose = TRUE){
  
  object <- 
    transferData(object, verbose = verbose) %>% 
    completeTracksDf(verbose = verbose) %>% 
    computeModuleVariables(verbose = verbose) %>% 
    summarizeDataByCell(verbose = verbose) %>% 
    summarizeModuleVariables(verbose = verbose)
  
  object <- setProgress(object, processData = TRUE)
  
  give_feedback(msg = cypro_object_ready, verbose = verbose)
  
  cypro_object_ready(object = object, verbose = verbose)
  
  return(object)
  
})


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



# t -----------------------------------------------------------------------

#' @title Transfer loaded data 
#' 
#' @description Extracts the content of all loaded files across
#' all well plates, merges them into one data.frame. Then sets up the appropriate
#' \code{Cdata} object of slot @@cdata.
#'
#' @inherit argument_dummy params
#' 
#' @details All files that have been loaded with \code{loadDataFiles()} without 
#' any error are merged to a data.frame. Depending on the loading modality - \emph{by_roi},
#' \emph{by_well}, \emph{by_well_plate} - the missing ID variables are added to the 
#' data.frame. It is then split into the functional parts the \code{Cdata} object
#' that corresponds to the experiment design determines. The \code{Cdata} object 
#' is then set in slot @@cdata of the \code{Cypro} object.
#' 
#' @return The input \code{Cypro} object with the data files merged to an appropriate 
#' \code{cypro_df} in an appropriate \code{Cdata} object set in slot @@cdata.
#' 
#' @export
#'

setGeneric(name = "transferData", def = function(object, ...){
  
  standardGeneric(f = "transferData")
  
})

#' @rdname transferData
#' @export
setMethod(
  f = "transferData",
  signature = "CyproScreening",
  definition = function(object, verbose = TRUE){
  
  df <- 
    getMergedDf(object, verbose = verbose) %>% 
    as_cypro_df()
  
  object <- emptyDataFiles(object)
  
  cluster_df <- 
    dplyr::select(df, cell_id) %>% 
    dplyr::distinct() 
  
  feature_df <- 
    dplyr::select(df, cell_id, where(base::is.numeric))
  
  well_plate_df <-
    dplyr::select(df, cell_id, dplyr::all_of(var_names_dfs$well_plate)) %>% 
    dplyr::distinct()
  
  layouts_df <- 
    getWellPlates(object) %>% 
    purrr::map_df(.f = ~ .x@layout) %>% 
    dplyr::select(well_plate_name, well, cell_line, condition)
  
  grouping_vars <- getAdditionalVariableNames(object)$grouping
  
  meta_df <- 
    dplyr::select(df, cell_id, dplyr::any_of(x = grouping_vars), well_plate_name, well) %>% 
    dplyr::distinct() %>% 
    dplyr::left_join(x = ., y = layouts_df, by = c("well_plate_name", "well")) %>% 
    dplyr::select(-well_plate_name, -well) %>% 
    dplyr::mutate(cell_line = base::as.factor(cell_line), condition = base::as.factor(condition))
  
  give_feedback(msg = "Setting cell data.", verbose = verbose)
  
  cdata_object <- 
    methods::new(
      Class = "CdataScreening", 
      cluster = base::as.data.frame(cluster_df),
      features = base::as.data.frame(feature_df),
      meta = base::as.data.frame(meta_df),
      well_plate = base::as.data.frame(well_plate_df)
    )
  
  object@cdata <- cdata_object
  
  return(object)
  
})


#' @rdname transferData
#' @export
setMethod(f = "transferData", signature = "CyproTimeLapse", definition = function(object){
  
  n_frames <- nFrames(object)
  
  df <- 
    getMergedDf(object) %>% 
    dplyr::filter(frame_num <= n_frames) %>% 
    as_cypro_df()
  
  object <- emptyDataFiles(object)
  
  cluster_df <- 
    dplyr::select(df, cell_id) %>% 
    dplyr::distinct() 
  
  stats_df <- 
    dplyr::select(df, cell_id) %>% 
    dplyr::distinct() %>% 
    as_stats_df()
  
  tracks_df <- 
    dplyr::select(df, -dplyr::all_of(var_names_dfs$well_plate)) %>% 
    as_tracks_df()
  
  well_plate_df <-
    dplyr::select(df, cell_id, dplyr::all_of(var_names_dfs$well_plate)) %>% 
    dplyr::distinct() 
  
  layouts_df <- 
    getWellPlates(object) %>% 
    purrr::map_df(.f = ~ .x@layout) %>% 
    dplyr::select(well_plate_name, well, cell_line, condition)
  
  grouping_vars <- getAdditionalVariableNames(object)$grouping
  
  meta_df <- 
    dplyr::select(df, cell_id, dplyr::any_of(x = grouping_vars), well_plate_name, well) %>% 
    dplyr::distinct() %>% 
    dplyr::left_join(x = ., y = layouts_df, by = c("well_plate_name", "well")) %>% 
    dplyr::select(-well_plate_name, -well) %>% 
    dplyr::mutate(cell_line = base::as.factor(cell_line), condition = base::as.factor(condition))
  
  give_feedback(msg = "Setting cell data.", verbose = verbose)
  
  cdata_object <- 
    methods::new(
      Class = "CdataTimeLapse", 
      cluster = base::as.data.frame(cluster_df),
      features_stats = base::as.data.frame(stats_df),
      features_tracks = base::as.data.frame(tracks_df),
      meta = base::as.data.frame(meta_df),
      well_plate = base::as.data.frame(well_plate_df)
    )
  
  object@cdata <- cdata_object
  
  return(object)
  
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

# v -----------------------------------------------------------------------

valid_content_but_inappropriate_class <- function(x){
  
  glue::glue(
    "Valid content but class attribute is not set properly. Should include '{x}'."
  )
  
}
