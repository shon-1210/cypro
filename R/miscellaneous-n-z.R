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
#' Sets \code{dir_input} in the slot @@directory of well plate 
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
                                                       dir_input,
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
                        dir_input,
                        valid_filetypes,
                        recursive = FALSE,
                        in_shiny = FALSE){
    
    ldm <- suggestLoadingModality(object)
    
    if(ldm == "by_roi"){
      
      object <- 
        prepare_data_loading_by_roi(
          object = object, 
          well_plate = well_plate,
          directory = dir_input, 
          recursive = recursive,
          valid_filetypes = valid_filetypes, 
          in_shiny = in_shiny
        )
      
    } else if(ldm == "by_well"){
      
      object <- 
        prepare_data_loading_by_well(
          object = object, 
          well_plate = well_plate, 
          directory = dir_input,
          recursive = recursive,
          valid_filetypes = valid_filetypes, 
          in_shiny = in_shiny
        )
      
    } else if(ldm == "by_well_plate"){
      
      object <- 
        prepare_data_loading_by_well_plate(
          object = object, 
          well_plate = well_plate, 
          directories = dir_input, 
          valid_filetypes = valid_filetypes,
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
  
  layout_df <- setDataStatus(layout_df) %>% nestLayoutDf()
  
  well_plate_obj <- setLayoutDf(object = well_plate_obj, layout_df = layout_df)
  
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
        directory = purrr::flatten_chr(well_df$file_dir),
        file_status = base::as.character(well_df$file_status),
        loading_modality = "by_well",
        name = well,
        transferred = FALSE, 
        valid = NA,
        well_plate = well_plate
      )
    
  }
  
  well_plate_obj@directory <- directory
  well_plate_obj@files <- file_list
  well_plate_obj@filetypes <- valid_filetypes
  
  layout_df <- setDataStatus(layout_df) %>% nestLayoutDf()
  
  well_plate_obj <- setLayoutDf(object = well_plate_obj, layout_df = layout_df)
  
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
                                               directories, 
                                               valid_filetypes,
                                               in_shiny = FALSE){
  
  
  directories <- base::unique(directories)
  
  n_dirs <- base::length(directories)
  
  valid_filetypes_pattern <- 
    stringr::str_c(valid_filetypes, collapse = "|") %>% 
    stringr::str_c("(", ., ")", sep = "")
  
  valid_dirs <- stringr::str_detect(directories, pattern = valid_filetypes_pattern)
  
  if(!base::all(valid_dirs)){
    
    msg <- 
      glue::glue(
        "Invalid directory assignment for well plate '{well_plate}'. ", 
        "All assigned directories must lead to a filetype .csv, .txt, .xls or .xlsx."
      )
    
    give_feedback(
      msg = msg,
      fdb.fn = "stop",
      with.time = FALSE,
      in.shiny = in_shiny
    )
    
  }
  
  well_plate_obj <- getWellPlate(object, well_plate = well_plate)
  
  layout_df <- getLayoutDf(well_plate_obj)
  
  if(n_dirs > 1){
    
    layout_df$file_dir <- "Multiple"
    layout_df$file_status <- "Valid"
    
  } else {
    
    layout_df$file_dir <- directories
    layout_df$file_status <- "Valid"
    
  }
  
  well_plate_obj@directory <- directories
  
  for(d in base::seq_along(directories)){
    
    dir <- directories[d]
    
    fname <- stringr::str_c("file", d, sep = "")
    
    well_plate_obj@files[[fname]] <- 
      methods::new(
        Class = "DataFile", 
        directory = dir, 
        loading_modality = "by_well_plate", 
        transferred = FALSE,
        valid = NA,
        well_plate = well_plate
      )
    
  }
  
  well_plate_obj <- 
    setLayoutDf(
      object = well_plate_obj,
      layout_df = ayout_df
    )
  
  object <- setWellPlate(object, well_plate_object = well_plate_obj)
  
  ref <- adapt_reference(directories, sg = "directory", pl = "directories")
  dir_ref <- scollapse(directories)
  
  give_feedback(
    msg = glue::glue("File {ref} '{dir_ref}' set for well plate '{well_plate}'."), 
    fdb.fn = "message", 
    in.shiny = in_shiny, 
    with.time = FALSE
  )
  
  return(object)
  
}

#' @title Print subset information 
#' 
#' @description If the object is the results of one or more 
#' subsetting processes you can print the respective information 
#' into the console to keep track of the objects origin. 
#'
#' @inherit argument_dummy params
#'
#' @return A printed message via \code{writeLines()}.
#' @export
#'

setGeneric(name = "printSubsetHistory", def = function(object){
  
  standardGeneric(f = "printSubsetHistory")
  
})

#' @rdname printSubsetHistory
#' @export
setMethod(f = "printSubsetHistory", signature = "Cypro", definition = function(object){
  
  subsets <- object@subsets
  
  if(isOfLength(subsets, 0)){
    
    base::writeLines("Cypro object has not been subsetted yet.")
    
  } else {
    
    purrr::walk(
      .x = subsets, 
      .f = function(subset_object){
        
        writeLines(text = stringr::str_c(base::rep("-", 50), collapse = ""))
        
        methods::show(subset_object)
        
      }
    )
    
  }
  
})

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
  
  object <- transferData(object, verbose = verbose)
  
  object <- computeModuleVariables(object, verbose = verbose)
  
  object <- scaleData(object)
  
  # outlier detection
  df <- getFeatureDf(object, with_everything = TRUE)
  
  outlier_object <- initiateOutlierDetection(data = df, key_name = "cell_id")
  
  object <- setOutlierDetection(object, outlier_object = outlier_object)
  
  # set analysis aspect list 
  fnames <- getFeatureNames(object) %>% vselect(-any_of(non_data_variables))
  
  object <- addFeatureSet(object, name = "all_features", features = fnames)
  
  object <- setAnalysisList(object)
  
  object <- setProgress(object, processData = TRUE)
  
  cypro_object_ready(object = object, verbose = verbose)
  
  return(object)
  
})

#' @rdname processData
#' @export
setMethod(f = "processData", signature = "CyproTimeLapse", definition = function(object, verbose = TRUE){
  
  object <- transferData(object, verbose = verbose)
  
  object <- completeTracksDf(object, verbose = verbose)
  
  object <- computeModuleVariables(object, verbose = verbose)
  
  object <- summarizeDataByCell(object, verbose = verbose) 
  
  object <- summarizeModuleVariables(object, verbose = verbose)
  
  object <- setProgress(object, processData = TRUE)
  
  # set outlier detection 
  
  df <- getStatsDf(object, with_everything = TRUE)
  
  outlier_object <- initiateOutlierDetection(data = df, key_name = "cell_id")
  
  object <- setOutlierDetection(object, outlier_object = outlier_object)
  
  # set analysis list
  
  fnames <- getStatFeatureNames(object) %>% vselect(-any_of(non_data_variables))
  
  object <- addFeatureSet(object, name = "all_features", stat_features = fnames)
  
  object <- setAnalysisList(object)
  
  cypro_object_ready(object = object, verbose = verbose)
  
  return(object)
  
})

#' @rdname processData
#' @export
setMethod(f = "processData", signature = "CyproTimeLapseMP", definition = function(object, verbose = TRUE){
  
  object <- transferData(object, verbose = verbose) 
  
  object <- completeTracksDf(object, verbose = verbose) 
  
  object <- computeModuleVariables(object, verbose = verbose) 
  
  object <- summarizeDataByCell(object, verbose = verbose) 
  
  object <- summarizeModuleVariables(object, verbose = verbose)
  
  object <- setProgress(object, processData = TRUE)
  
  # set outlier detection 
  
  df <- getStatsDf(object, with_everything = TRUE)
  
  outlier_object <- initiateOutlierDetection(data = df, key_name = "cell_id")
  
  object <- setOutlierDetection(object, outlier_object = outlier_object)
  
  # set analysis list
  
  fnames <- getStatFeatureNames(object) %>% vselect(-any_of(non_data_variables))
  
  object <- addFeatureSet(object, name = "all_features", stat_features = fnames)
  
  object <- setAnalysisList(object)
  
  cypro_object_ready(object = object, verbose = verbose)
  
  return(object)
  
})


# s -----------------------------------------------------------------------

#' @title Save \code{Cypro} object
#'
#' @description Savesa the object under its storage directory. 
#'
#' @inherit argument_dummy params
#' 
#' @return An invisible TRUE if saving occured without any error.
#'
#' @export

setGeneric(name = "saveCyproObject", def = function(object, ...){
  
  standardGeneric(f = "saveCyproObject")
  
})

#' @rdname saveCyproObject
#' @export

setMethod(f = "saveCyproObject", signature = "Cypro", function(object, verbose = TRUE){
  
  dir <- object@information$storage_directory
  
  if(!base::is.character(dir)){
    
    stop("No storage directory set. Use `setStorageDirectory()` first.")
    
  }
  
  confuns::give_feedback(
    msg = glue::glue("Saving cypro object under '{dir}'."), 
    verbose = verbose
  )
  
  base::saveRDS(object, file = dir)
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::invisible(TRUE)
  
})

#' @rdname scaleData
#' @export
setMethod(
  f = "scaleData", 
  signature = "CyproScreening", 
  definition = function(object, verbose = TRUE, ...){
    
    give_feedback(msg = "Scaling data.", verbose = verbose)
    
    scaled_df <- 
      getFeatureDf(object) %>% 
      dplyr::select(-dplyr::any_of(non_data_variables)) %>% 
      tibble::column_to_rownames(var = "cell_id") %>% 
      base::scale() %>% 
      base::as.data.frame() %>% 
      tibble::rownames_to_column(var = "cell_id") 
    
    object@cdata@scaled <- scaled_df
    
    give_feedback(msg = "Done.", verbose = verbose)
    
    return(object)
    
  }
)

#' @rdname scaleData
#' @export
setMethod(
  f = "scaleData", 
  signature = "CyproTimeLapse", 
  definition = function(object, verbose = TRUE, ...){
    
    give_feedback(msg = "Scaling data.", verbose = verbose)
    
    scaled_df <- 
      getStatsDf(object) %>% 
      dplyr::select(-dplyr::any_of(non_data_variables)) %>% 
      tibble::column_to_rownames(var = "cell_id") %>% 
      base::scale() %>% 
      base::as.data.frame() %>% 
      tibble::rownames_to_column(var = "cell_id")
    
    object@cdata@scaled <- scaled_df
    
    give_feedback(msg = "Done.", verbose = verbose)
    
    return(object)
    
  }
)

#' @rdname scaleData
#' @export
setMethod(
  f = "scaleData", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, verbose = TRUE, ...){
    
    give_feedback(msg = "Scaling data.", verbose = verbose)
    
    scaled_dfs <- 
      purrr::map(.x = getPhaseNames(object), .f = function(p){
        
        getStatsDf(object, phase = p) %>% 
          dplyr::select(-dplyr::any_of(non_data_variables)) %>% 
          dplyr::select(-dplyr::any_of(non_data_variables)) %>% 
          tibble::column_to_rownames(var = "cell_id") %>% 
          base::scale() %>% 
          base::as.data.frame() %>% 
          tibble::rownames_to_column(var = "cell_id")
        
      })
    
    object@cdata@scaled <- scaled_dfs
    
    give_feedback(msg = "Done.", verbose = verbose)
    
    return(object)
    
  }
)


#' @title Select and deselect wells 
#' 
#' @description 
#' 
#' \itemize{
#'  \item{\code{selectWells()}}{ Selects wells by changing the value 
#'  of variable \emph{selected} of all wells found in input for argument 
#'  \code{wells} to TRUE.}
#'  \item{\code{deselectWells()}}{ De selects wells by changing the value 
#'  of variable \emph{selected} of all wells found in input for argument 
#'  \code{wells} to FALSE.}
#'  }
#'  
#'  The selection status of wells that are not denoted in the input for argument \code{wells} stays
#'  as is.
#' 
#' @param df Data.frame of class \code{layout_df}.
#' @param wells Character vector or NULL. If character, denotes wells which 
#' are supposed to be (de-)selected. If NULL, all wells are considered. 
#' 
#' @return A layout data.frame of the same class as the input.
#' 
#' @export

selectWells <- function(df, wells = NULL){
  
  UseMethod(generic = "selectWells", object = df)
  
}

#' @rdname selectWells
#' @export
selectWells.layout_df <- function(df, wells = NULL){
  
  if(base::is.null(wells)){
    
    wells <- base::unique(df$well)
    
  }
  
  check_one_of(
    input = wells, 
    against = base::unique(df$well), 
    fdb.fn = "warning", 
    with.time = FALSE
  )
  
  out <- 
    dplyr::mutate(
      .data = df, 
      selected = dplyr::if_else(
        condition = well %in% {{wells}},
        true = TRUE,
        false = selected
      )
    )
  
  return(out)
  
}



#' @title Set seed
#'
#' @param set_seed Numeric or NULL.
#'
set_seed_hlpr <- function(set_seed){
  
  if(base::is.numeric(set_seed)){
    
    confuns::give_feedback(
      msg = glue::glue("Setting seed: {set_seed}"), 
      verbose = verbose
    )
    
    base::set.seed(seed = set_seed)
    
  }
  
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
  
  optional_vars <- id_assignment[c("well_plate", "well", "roi", "well_roi")]
  
  if(base::all(base::is.na(optional_vars[c("roi", "well_roi")]))) { # no roi information
    
    res <- "by_roi"
    
  } else if(base::all(base::is.na(optional_vars[c("well", "well_roi")]))){ # no well information
    
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
    
    df <- getMergedDf(object, verbose = verbose) 
    
    object <- emptyDataFiles(object)
    
    object <- setCdata(object = object, df = df, verbose = verbose)
    
    return(object)
    
  })


#' @rdname transferData
#' @export
setMethod(f = "transferData", signature = "CyproTimeLapse", definition = function(object, verbose = TRUE){
  
  df <- getMergedDf(object, verbose = verbose) 
  
  object <- emptyDataFiles(object)
  
  object <- setCdata(object = object, df = df, verbose = verbose)
  
  return(object)
  
})

#' @rdname transferData
#' @export
setMethod(f = "transferData", signature = "CyproTimeLapseMP", definition = function(object, verbose = TRUE){
  
  df <- getMergedDf(object, verbose = verbose)
  
  object <- emptyDataFiles(object)
  
  object <- setCdata(object = object, df = df, verbose = verbose)
  
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
