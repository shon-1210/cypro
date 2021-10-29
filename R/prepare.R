




# D -----------------------------------------------------------------------


prepareDataLoading <- function(object,
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
  
  
}


prepare_data_loading_by_roi <- function(object,
                                        well_plate,
                                        recursive = TRUE,
                                        directory = "data-examples/Maier et al. 2021",
                                        valid_filetypes = filetypes, 
                                        in_shiny = TRUE){
  
  valid_filetypes_pattern <- 
    stringr::str_c(valid_filetypes, collapse = "|") %>% 
    stringr::str_c("(", ., ")", sep = "")
  
  well_plate_obj <- getWellPlate(object, well_plate = well_plate)
  
  layout_df <- 
    getLayoutDf(well_plate_obj) %>% 
    unnestLayoutDf()
  
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
  
  file_list <- list()
  
  for(well_roi in well_rois_complete){
    
    well_roi_df <- dplyr::filter(layout_df, well_roi == {{well_roi}})
    
    file_list[[well_roi]] <- 
      methods::new(
        Class = "DataFile", 
        file_status = base::as.character(well_roi_df$file_status),
        directory = purrr::flatten_chr(well_roi_df$file_dir),
        loading_modality = "by_roi"
      )
    
  }
  
  layout_df <- add_roi_info_vars(layout_df, vars = c("file_dir", "file_status"))
  
  well_plate_obj@directory <- directory
  well_plate_obj@files <- file_list
  well_plate_obj@filetypes <- valid_filetypes
  well_plate_obj@layout <- nestLayoutDf(layout_df)
  well_plate_obj@loading_modality <- "by_roi"
  well_plate_obj@recursive <- recursive
  
  object <- setWellPlate(object, well_plate_object = well_plate_obj)
  
  give_feedback(
    msg = glue::glue("Folder directory '{directory}' set for well plate '{well_plate}'."), 
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
                                         in_shiny = TRUE){
  
  valid_filetypes_pattern <- 
    stringr::str_c(valid_filetypes, collapse = "|") %>% 
    stringr::str_c("(", ., ")", sep = "")
  
  well_plate_obj <- getWellPlate(object, well_plate = well_plate)
  
  layout_df <- getLayoutDf(well_plate_obj)
  
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
  
  file_list <- 
    base::vector(mode = "list", length = nWells(layout_df, info_status = "Complete")) %>% 
    purrr::set_names(nm = getWells(layout_df, info_status = "Complete"))
  
  for(well in wells_complete){
    
    well_df <- dplyr::filter(layout_df, well == {{well}})
    
    file_list[[well]] <- 
      methods::new(
        Class = "DataFile", 
        file_status = base::as.character(well_df$file_status),
        directory = purrr::flatten_chr(well_df$file_dir),
        loading_modality = "by_well"
      )
    
  }
  
  well_plate_obj@directory <- directory
  well_plate_obj@files <- file_list
  well_plate_obj@filetypes <- valid_filetypes
  well_plate_obj@layout <- nestLayoutDf(layout_df)
  well_plate_obj@loading_modality <- "by_well"
  well_plate_obj@recursive <- recursive
  
  object <- setWellPlate(object, well_plate_object = well_plate_obj)
  
  give_feedback(
    msg = glue::glue("Folder directory '{directory}' set for well plate '{well_plate}'."), 
    fdb.fn = "message", 
    in.shiny = in_shiny, 
    with.time = FALSE
  )
  
  return(object)
  
}


prepare_data_loading_by_well_plate <- function(object, 
                                               well_plate, 
                                               directory, 
                                               in_shiny){
  
  if(!stringr::str_detect(directory, pattern = filetypes)){
    
    msg <- 
      glue::glue(
        "Invalid directory assignment for well plate '{well_plate}'. ", 
        "Assigned directory must lead to a filetype .csv, .txt, .xls or .xlsx."
      )
    
    give_feedback(msg = msg, fdb.fn = "stop", msg = msg, with.time = FALSE)
    
  }
  
  well_plate_obj <- getWellPlate(object, well_plate = well_plate)
  
  layout_df <- getLayoutDf(well_plate_obj)
  layout_df$file_dir <- directory
  
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