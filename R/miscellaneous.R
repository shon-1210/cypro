

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



# i -----------------------------------------------------------------------


#' @title Initiate empty cypro object 
#' 
#' @description Initiates an empty cypro object and 
#' adds the version under which is is created.
#'
#' @param ... 
#'
initiateEmptyCyproObject <- function(...){
  
  object <- methods::new(Class = "cypro", ...)
  
  object@version <- current_version
  
  base::return(object)
  
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



#' @title Data loading - Prepare data loading 
#' 
#' @description First of three data loading steps. Assigns a folder/file to every 
#' well plate. See details for more. 
#' 
#' @inherit argument_dummy params
#' 
#' @seealso \code{readataFiles()}, \code{processData()}
#' 
#' @details
#' 
#' Has to be called iteratively for every well plate. 
#' 
#' Loading modality \emph{by_roi} or \emph{by_well}:
#' 
#' Sets \code{directory} in the slot @@directory of well plate 
#' denoted in \code{well_plate}. Screens the folder (and subfolders if \code{recursive} = TRUE)
#' for files that end with the valid file endings denoted in \code{valid_filetypes}. 
#' Then it uses regular expressions to assign the remaining files to the wells / well rois. 
#' The \emph{file_status} is then declared: Every well / well roi that afterwards contain one assigned
#' file is considered \emph{Valid}. Wells / well rois with more matching files are considered \emph{Ambiguous}
#' and those with no matching files are considered \emph{Missing}. For every valid file an object
#' of class \code{DataFile} is created in the list of slot @@files of the \code{WellPlate} object
#' containing the assigned file directory.
#' 
#' Afterwards \code{loadDataFiles()} uses the prepared \code{DataFile} objects to read every file.
#'    

setGeneric(name = "prepareDataLoading", def = function(object,
                                                       well_plate,
                                                       directory,
                                                       valid_filetypes,
                                                       recursive = FALSE,
                                                       in_shiny = FALSE){
  
  standardGeneric(f = "prepareDataLoading")
  
})

#' @rdname prepareDataLoading
#' @export
setMethod(
  f = "prepareDataLoading",
  signature = "Cypro",
  definition = function(object,
                        well_plate,
                        directory,
                        valid_filetypes,
                        recursive = FALSE,
                        in_shiny = FALSE){
    
    ldm <- suggestLoadingModality(object)
    
    if(ldm == "by_roi"){
      
      object <- 
        prepare_data_loading_by_roi(
          object = object, 
          well_plate = well_plate,
          directory = directory, 
          recursive = recursive,
          valid_filetypes = valid_filetypes, 
          in_shiny = in_shiny
        )
      
    } else if(ldm == "by_well"){
      
      object <- 
        prepare_data_loading_by_well(
          object = object, 
          well_plate = well_plate, 
          directory = directory,
          recursive = recursive,
          valid_filetypes = valid_filetypes, 
          in_shiny = in_shiny
        )
      
    } else if(ldm == "by_well_plate"){
      
      object <- 
        prepare_data_loading_by_well_plate(
          object = object, 
          well_plate = well_plate, 
          directory = directory, 
          in_shiny = in_shiny
        )
      
    }
    
    return(object)
    
  })

prepare_data_loading_by_roi <- function(object,
                                        well_plate,
                                        recursive = TRUE,
                                        directory = "data-examples/Maier et al. 2021",
                                        valid_filetypes = filetypes, 
                                        in_shiny = FALSE){
  
  give_feedback(
    msg = glue::glue("Evaluating content of folder '{directory}'."), 
    fdb.fn = "message", 
    in.shiny = in_shiny, 
    with.time = FALSE, 
    duration = 15
  )
  
  valid_filetypes_pattern <- 
    stringr::str_c(valid_filetypes, collapse = "|") %>% 
    stringr::str_c("(", ., ")", sep = "")
  
  well_plate_obj <- getWellPlate(object, well_plate = well_plate)
  
  layout_df <- 
    getLayoutDf(well_plate_obj) %>% 
    unnestLayoutDf()
  
  layout_df$file_dir <- NULL
  layout_df$file_status <- NULL
  
  well_rois_all <- getWellRois(layout_df)
  well_rois_complete <- getWellRois(layout_df, info_status = "Complete")
  
  directories <-
    base::list.files(path = directory, recursive = recursive, full.names = TRUE) %>% 
    stringr::str_subset(pattern = rgx_well_roi_file) %>% 
    stringr::str_subset(pattern = valid_filetypes_pattern)
  
  extracted_well_rois <- 
    extractWellRoiFileInfo(vec = directories) %>% 
    stringr::str_remove_all(pattern = rgx_file) %>% 
    adjustWellRoiInfo()
  
  well_roi_dirs <-
    tibble::tibble(file_dir = directories, well_roi = extracted_well_rois) %>% 
    dplyr::left_join(x = tibble::tibble(well_roi = getWellRois(layout_df)), y = ., by = "well_roi")
  
  file_dir_list <- 
    base::vector(mode = "list", length = nWellRois(layout_df)) %>% 
    purrr::set_names(nm = getWellRois(layout_df))
  
  for(well_roi in well_rois_all){
    
    file_dir_list[[well_roi]] <- 
      dplyr::filter(well_roi_dirs, well_roi == {{well_roi}}) %>% 
      dplyr::pull(file_dir)
    
  }
  
  layout_df$file_dir <- base::unname(file_dir_list)
  
  layout_df$file_status <-
    purrr::map_chr(.x = file_dir_list, .f = file_dir_validity) %>% 
    base::factor(levels = file_status_levels)
  
  well_rois_with_files <- 
    dplyr::filter(layout_df, file_status == "Valid") %>% 
    dplyr::pull(var = "well_roi")
  
  n_files <- base::length(well_rois_with_files)
  
  file_list <- 
    base::vector(mode = "list", length = n_files) %>% 
    purrr::set_names(nm = well_rois_with_files)
  
  for(well_roi in well_rois_with_files){
    
    well_roi_df <- dplyr::filter(layout_df, well_roi == {{well_roi}})
    
    file_list[[well_roi]] <- 
      methods::new(
        Class = "DataFile", 
        directory = purrr::flatten_chr(well_roi_df$file_dir),
        file_status = base::as.character(well_roi_df$file_status),
        loading_modality = "by_roi",
        name = well_roi,
        transferred = FALSE, 
        valid = NA,
        well_plate = well_plate
      )
    
  }
  
  layout_df <- addRoiInfoVarNames(layout_df, vars = c("file_dir", "file_status"))
  
  well_plate_obj@directory <- directory
  well_plate_obj@files <- file_list
  well_plate_obj@filetypes <- valid_filetypes
  well_plate_obj@layout <- setDataStatus(layout_df) %>% nestLayoutDf()
  well_plate_obj@loading_modality <- "by_roi"
  well_plate_obj@recursive <- recursive
  
  object <- setWellPlate(object, well_plate_object = well_plate_obj)
  
  give_feedback(
    msg = glue::glue("Folder directory '{directory}' set for well plate '{well_plate}' with a total of {n_files} valid file names."), 
    fdb.fn = "message", 
    in.shiny = in_shiny, 
    with.time = FALSE
  )
  
  return(object)
  
}


prepare_data_loading_by_well <- function(object, 
                                         well_plate, 
                                         recursive = TRUE, 
                                         directory = "data-development/load-data/by-well/example1", 
                                         valid_filetypes = filetypes,
                                         in_shiny = FALSE){
  
  give_feedback(
    msg = glue::glue("Evaluating content of folder '{directory}'."), 
    fdb.fn = "message", 
    in.shiny = in_shiny, 
    with.time = FALSE, 
    duration = 15
  )
  
  valid_filetypes_pattern <- 
    stringr::str_c(valid_filetypes, collapse = "|") %>% 
    stringr::str_c("(", ., ")", sep = "")
  
  well_plate_obj <- getWellPlate(object, well_plate = well_plate)
  
  layout_df <- getLayoutDf(well_plate_obj)
  
  layout_df$file_dir <- NULL
  layout_df$file_status <- NULL
  
  wells_all <- getWells(layout_df)
  wells_complete <- getWells(layout_df, info_status = "Complete")
  
  directories <-
    base::list.files(path = directory, recursive = recursive, full.names = TRUE) %>% 
    stringr::str_subset(pattern = rgx_well_file) %>% 
    stringr::str_subset(pattern = valid_filetypes_pattern)
  
  extracted_wells <- 
    stringr::str_remove_all(directories, pattern = rgx_file) %>% 
    extractWellInfo() %>% 
    adjustWellInfo()
  
  well_dirs <- tibble::tibble(file_dir = directories, well = extracted_wells)
  
  file_dir_list <- 
    base::vector(mode = "list", length = nWells(layout_df)) %>% 
    purrr::set_names(nm = getWells(layout_df))
  
  for(well in wells_all){
    
    file_dir_list[[well]] <- 
      dplyr::filter(well_dirs, well == {{well}}) %>% 
      dplyr::pull(file_dir)
    
  }
  
  layout_df$file_dir <- base::unname(file_dir_list)
  
  layout_df$file_status <-
    purrr::map_chr(.x = file_dir_list, .f = file_dir_validity) %>% 
    base::factor(levels = file_status_levels)
  
  wells_with_files <- 
    dplyr::filter(layout_df, file_status == "Valid") %>% 
    dplyr::pull(var = "well")
  
  n_files <- base::length(wells_with_files)
  
  file_list <- 
    base::vector(mode = "list", length = n_files) %>% 
    purrr::set_names(nm = wells_with_files)
  
  for(well in wells_with_files){
    
    well_df <- dplyr::filter(layout_df, well == {{well}})
    
    file_list[[well]] <- 
      methods::new(
        Class = "DataFile", 
        directory = purrr::flatten_chr(well_roi_df$file_dir),
        file_status = base::as.character(well_roi_df$file_status),
        loading_modality = "by_roi",
        name = well,
        transferred = FALSE, 
        valid = NA,
        well_plate = well_plate
      )
    
  }
  
  well_plate_obj@directory <- directory
  well_plate_obj@files <- file_list
  well_plate_obj@filetypes <- valid_filetypes
  well_plate_obj@layout <- setDataStatus(layout_df) %>% nestLayoutDf()
  well_plate_obj@loading_modality <- "by_well"
  well_plate_obj@recursive <- recursive
  
  object <- setWellPlate(object, well_plate_object = well_plate_obj)
  
  give_feedback(
    msg = glue::glue("Folder directory '{directory}' set for well plate '{well_plate}' with a total of {n_files} valid file names."), 
    fdb.fn = "message", 
    in.shiny = in_shiny, 
    with.time = FALSE, 
    duration = 15
  )
  
  return(object)
  
}


prepare_data_loading_by_well_plate <- function(object, 
                                               well_plate, 
                                               directory, 
                                               in_shiny = FALSE){
  
  if(!stringr::str_detect(directory, pattern = filetypes)){
    
    msg <- 
      glue::glue(
        "Invalid directory assignment for well plate '{well_plate}'. ", 
        "Assigned directory must lead to a filetype .csv, .txt, .xls or .xlsx."
      )
    
    give_feedback(msg = msg, fdb.fn = "stop", msg = msg, with.time = FALSE, in.shiny = in_shiny)
    
  }
  
  well_plate_obj <- getWellPlate(object, well_plate = well_plate)
  
  layout_df <- getLayoutDf(well_plate_obj)
  
  layout_df$file_dir <- directory
  layout_df$file_status <- "Valid"
  
  well_plate_obj@directory <- directory
  
  well_plate_obj@files <- 
    methods::new(
      Class = "DataFile", 
      directory = directory, 
      loading_modality = "by_well_plate"
    )
  
  well_plate_obj@layout <- layout_df
  
  object <- setWellPlate(object, well_plate_object = well_plate_obj)
  
  give_feedback(
    msg = glue::glue("File directory '{directory}' set for well plate '{well_plate}'."), 
    fdb.fn = "message", 
    in.shiny = in_shiny, 
    with.time = FALSE
  )
  
  return(object)
  
}

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

#' @title Save cypro object
#'
#' @inherit check_object params
#'
#' @export

saveCyproObject <- function(object, verbose = TRUE){
  
  dir <- object@information$storage_directory
  
  if(!base::is.character(dir)){
    
    base::stop("No storage directory set. Use `setStorageDirectory()` first.")
    
  }
  
  confuns::give_feedback(
    msg = glue::glue("Saving cypro object under '{dir}'."), 
    verbose = verbose
  )
  
  base::saveRDS(object, file = dir)
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
}

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
