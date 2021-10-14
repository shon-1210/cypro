

#' Adds variable denotation from pdl to object 
#'
add_vardenotation_to_cypro_object_shiny <- function(object, amils){
  
  # identifier 
  object@information$variable_denotation$identifier <- 
    purrr::map(.x = amils$identifier$variables, .f = ~ .x$name_in_example)
  
  # additional 
  object@information$variable_denotation$additional <- 
    purrr::map(.x = amils$additional$variables, .f = ~ .x$name_in_example) %>% 
    purrr::set_names(nm = c("grouping_variables", "numeric_variables"))
  
  # analysis modules 
  object@modules <- 
    purrr::map(.x = amils$analysis_modules, .f = function(analysis_module){
      
      module_list <- list()
      
      module_list$variable_denotation <- 
        purrr::map(.x = analysis_module$variables, .f = ~ .x$name_in_example)
      
      return(module_list)
      
    })
  
  return(object)
  
}


#' Title
#'
#' @param input_list shiny server input list.
#' @param object the cypro object of the session
#' @param example_df the example data file uploaded
#'
#' @return the list describing the experiment-type depending identification module containing 
#' information about the names the identification variables carry in 
#' the example data.frame
#' @export
#'
assemble_id_module_info_shiny <- function(input_list, object, example_df = NULL, ed_vars = NULL){
  
  if(isTimeLapseExp(object)){
    
    identification_module <- module_identification_time_lapse
    
  } else {
    
    identification_module <- module_identification_one_time_imaging
    
  }
  
  identification_module$variables <- 
    purrr::map(.x = identification_module$variables, .f = function(var_info){
      
      pattern <- stringr::str_c(var_info$name_in_cypro, "$", sep = "")
      
      var_name_in_example <- 
        confuns::lselect(
          lst = input_list,
          contains("vardenotation-") &
          matches(pattern)
        ) %>% 
        purrr::flatten_chr()
      
      var_info$name_in_example <- var_name_in_example
      
      return(var_info)
      
    })
  
  n_obs <- base::nrow(example_df)
  
  cell_id_var <- identification_module$variables$cell_id$name_in_example
  
  if(isTimeLapseExp(object)){
    
    frame_num_var <- identification_module$variables$frame_num$name_in_example
    
    if(base::is.character(ed_vars)){
    
      ed_vars <- ed_vars %>% confuns::vselect(-none)  
      
    }
    
    n_unique_obs <- 
      example_df[,c(ed_vars, cell_id_var, frame_num_var)] %>% 
      dplyr::distinct() %>% 
      base::nrow()
    
    if(n_unique_obs < n_obs){
      
      msg  <- 
        glue::glue(
          "Frame-variable '{frame_num_var}' and Cell ID-variable '{cell_id_var}' do not ", 
          "uniquely identify each observation. Invalid variable assignment."
        )
      
      shiny_fdb(ui = msg, type = "error", duration = 20)
      shiny::req(FALSE)
      
    }
    
  } else {
    
    n_unique_obs <- dplyr::n_distinct(example_df[[cell_id_var]])
    
    if(n_unique_obs < n_obs){
      
      msg  <- 
        glue::glue(
          "Cell ID-variable {cell_id_var} does not ", 
          "uniquely identify each observation. Invalid variable assignment."
        )
      
      shiny_fdb(ui = msg, type = "error", duration = 20)
      shiny::req(FALSE)
      
    }
    
  }
  
  return(identification_module)
  
}


#' Title
#'
#' @param input_list shiny server input list.
#' @param object the cypro object of the session
#' @param example_df the example data file uploaded
#'
#' @return the list describing the 'additional data variables' module. it contains 
#' information about the names the additional variables carry in 
#' the example data.frame as well as whether they are grouping or numeric
#' variables
#' @export
#'
assemble_additional_data_variables_module_info_shiny <- function(input_list, object){
  
  additional_data_module <- module_additional_data_variables
  
  additional_data_module$variables <- 
    purrr::map(.x = additional_data_module$variables, .f = function(var_info){
      
      pattern <- stringr::str_c(var_info$name_in_cypro, "$", sep = "")
      
      var_name_in_example <- 
        confuns::lselect(
          lst = input_list,
          contains("vardenotation-") &
            matches(pattern)
        ) %>% 
        purrr::flatten_chr()
      
      var_info$name_in_example <- var_name_in_example
      
      return(var_info)
      
    })
  
  return(additional_data_module)
  
}


#' Title
#'
#' @param input_list shiny server input list.
#' @param object the cypro object of the session
#' @param example_df the example data file uploaded
#'
#' @return the list \code{cypro_modules} subsetted by modules to be used. It 
#' contains information about how the variables needed are named in the 
#' example data.frame as well as which of the computable variables 
#' have to be computed or are already part of the data to be loaded.
#' 
#' @export
#'
assemble_analysis_module_info_shiny <- function(input_list, object){
  
  if(isTimeLapseExp(object)){
    
    module_names <- time_lapse_module_names
    
  } else {
    
    module_names <- one_time_imaging_module_names
    
  }
  
  module_selection <- 
    purrr::map(.x = module_names, .f = function(module_name){
      
      pattern <- stringr::str_c("module_usage-", module_name)
      
      input <- confuns::lselect(input_list, contains(pattern))
      
      return(input[[1]])
      
    }) %>% 
    purrr::set_names(module_names) %>% 
    purrr::keep(.p = base::isTRUE) %>% 
    base::names()
  
  
  used_modules <- cypro_modules[module_selection]
  
  used_modules_with_input <- 
    purrr::map(.x = used_modules, .f = function(used_module){
      
      module_name_pattern <- stringr::str_c("-", used_module$name, "$")
      
      used_module$variables <- 
        purrr::map(.x = used_module$variables, .f = function(var_info){
          
          module_variable_name_pattern <- 
            stringr::str_c(
              var_info$name_in_cypro, 
              module_name_pattern, 
              sep = ""
            )
          
          var_name_in_example <- 
            confuns::lselect(
              lst = input_list,
              contains("vardenotation-") &
                matches(module_variable_name_pattern)
            ) %>% 
            purrr::flatten_chr()
          
          
          var_info$name_in_example <- var_name_in_example
          
          return(var_info)
          
        })
      
      return(used_module)
      
    })
  
}

#' @title Integrate cell stat tables
#' 
#' @description Subsets the successfully loaded files 
#'
#' @param track_data_list The output of \code{load_track_files_shiny()}.
#'
#' @return A list of one slot 'only' with the stat data.frame
#
assemble_tracks_one_time_imaging_shiny <- function(stat_data_list, well_plate_list, object){
  
  df_list <- 
    purrr::map(.x = stat_data_list, ~ .x$successful) %>% 
    purrr::map(.x = ., .f = ~ purrr::map_df(.x = .x, .f = ~ .x))
  
  all_data_list <- 
    list(df_list,
         well_plate_list,
         base::seq_along(df_list), 
         base::names(df_list)
    )
  
  # join well plate information and create final cell id
  df <- purrr::pmap_dfr(.l = all_data_list, .f = hlpr_assemble_df)
  
  cell_lines <- df$cell_line %>% base::unique() %>% base::sort()
  conditions <- df$condition %>% base::unique() %>% base::sort()
  cl_conditions <- df$cl_condition %>% base::unique() %>% base::sort()
  
  final_df <-
    dplyr::mutate(
      .data = df, 
      cell_line = base::factor(x = cell_line, levels = cell_lines), 
      condition = base::factor(x = condition, levels = conditions), 
      cl_condition = base::factor(x = cl_condition, levels = cl_conditions)
    )
  
  stat_list <- base::list("only" = final_df)
  
  base::return(stat_list)
  
}

#' @title Integrate cell track tables
#' 
#' @description Subsets the successfully loaded files. In case of timelapse 
#' experiments with multiple phases it sorts the data into multiple 
#' data.frames and returns a list of multiple data.frames named according 
#' to the ordinal number of the phase ("first", "second" etc.). In case of
#' non timelapse experimernts the list contains only one data.frame in a 
#' slot named "first".
#'
#' @param track_data_list The output of \code{load_track_files_shiny()}.
#'
#' @return A list of data.frames named by 

assemble_tracks_time_lapse_shiny <- function(track_data_list, well_plate_list, object){
  
  df_list <- 
    purrr::map(.x = track_data_list, ~ .x$successful) %>% 
    purrr::map(.x = ., .f = ~ purrr::map_df(.x = .x, .f = ~ .x))
  
  all_data_list <- 
    list(df_list, well_plate_list, base::seq_along(df_list), base::names(df_list))
  
  # join well plate information and create final cell id
  df <- purrr::pmap_dfr(.l = all_data_list, .f = hlpr_assemble_df)
  
  cell_lines <- df$cell_line %>% base::unique() %>% base::sort()
  conditions <- df$condition %>% base::unique() %>% base::sort()
  cl_conditions <- df$cl_condition %>% base::unique() %>% base::sort()
  
  final_df <-
    dplyr::mutate(
      .data = df, 
      cell_line = base::factor(x = cell_line, levels = cell_lines),
      condition = base::factor(x = condition, levels = conditions),
      cl_condition = base::factor(x = cl_condition, levels = cl_conditions)
    )
  
  phases <- object@set_up$phases
  
  measurements <- object@set_up$measurement_string
  
  conditions <- 
    stringi::stri_split(str = base::unique(final_df$condition), regex = "->")
  
  phase_starts <-
    purrr::map_dbl(.x = phases, .f = ~ base::which(x = measurements == .x))
  
  # if more than one phase exists return list of data.frames named by phase
  if(base::length(phase_starts) > 1){
    
    phase_endings <- 
      phase_starts[2:base::length(phase_starts)] %>% 
      base::append(x = ., values = base::max(final_df$frame_num)) %>% 
      purrr::set_names(nm = base::names(phases))
    
    phase_info <-
      purrr::pmap(
        .l = list(x = phase_starts, y = phase_endings, z = base::seq_along(phases)),
        .f = function(x, y, z){ list("start" = x, "end" = y, "index" = z)}
      ) %>% 
      purrr::set_names(nm = base::names(phases))
    
    track_list <- 
      purrr::map(.x = phase_info, .f = function(phase){
                   
                   phase_index <- phase$index
                   phase_start <- phase$start
                   phase_end <- phase$end
                   
                   phase_pattern <- stringr::str_c("^", phase_index, "\\.",sep = "")
                   
                   df <-
                     dplyr::filter(final_df, frame_num >= {{phase_start}} & frame_num < {{phase_end}})
                   
                   condition_split <- 
                     stringr::str_split_fixed(
                       string = df$condition,
                       pattern = " -> ", 
                       n = base::length(phase_endings)
                       ) 
                   
                   condition_vec <- 
                     stringr::str_remove_all(
                       string = base::as.character(condition_split[,phase_index]),
                       pattern = phase_pattern
                       )
                   
                   df$condition <- condition_vec
                   df$cl_condition <- stringr::str_c(df$cell_line, df$condition, sep = " & ")
                   
                   base::return(df)
                   
                 }) %>% 
      magrittr::set_names(value = base::names(phase_endings))
    
  } else { # if only one phase return list of one data.frame named "first"
    
    final_df$phase = "first"
    
    track_list <- base::list("first" = final_df)
    
  }
  
  base::return(track_list)
  
}


#' @title Return css status
#' 
css_status <- function(evaluate, case_true = "success", case_false = "warning"){
  
  if(base::isTRUE(evaluate)){
    
    return(case_true)
    
  } else {
    
    return(case_false)
    
  }
  
}


#' @title Evaluate the availability of files 
#' 
#' @description This functions takes a list as input that contains a well-plate data.frame (slot 
#' must be named \emph{wp_df}) and a directory. The directory leads to the folder in which to look for
#' .csv and .xls files named according to the well-image they belong. The availability of files is compared 
#' to what is expected based on the well-plate data.frame (wells, number of images per well etc.) 
#'
#' @param wp_list A list that contains a well-plate data.frame and a directory leading to the folder in which 
#' the respective files are stored. 
#' @param recursive Logical value. Given to argument \code{recursive} of \code{base::list.files()}.
#' @param keep_filetype Character value. Determines the filetype to be ignored if for one well-image 
#' a .csv and a .xls file is found. Either \emph{'xls$'} or \emph{'csv$'}.
#'
#' @return The same list with additional slots containing a the well-plate data.frame joined 
#' with the evaluation variables as well as a vector containing the well-image files that are missing,
#' a vector with file directories that are ambiguous and a vector with the valid directories from which
#' to load the data. 
#' 

evaluate_file_availability_shiny <- function(wp_list, recursive = TRUE, keep_filetype = "csv$"){
  
  directory <- wp_list$directory
  wp_df <- wp_list$wp_df
  relevant_wp_df <- dplyr::filter(.data = wp_df, information_status == "Complete")
  
  ignore_filetypes <- filetypes[filetypes != keep_filetype]
  
  rois_per_well <- wp_df$rois_per_well %>% base::unique()
  
  # filter all subsequent directories for the 'file_regex' pattern
  wp_directories <-
    base::list.files(path = directory, recursive = recursive, full.names = TRUE) %>% 
    stringr::str_subset(pattern = file_regex)
  
  assign("wp_list", value = wp_list, envir = .GlobalEnv)
  
  if(base::isTRUE(debug_ct)){print(wp_directories)}
  
  # get directories that are to be kept
  well_roi_vec <-
    stringr::str_extract(string = wp_directories, pattern = file_regex) %>% 
    stringr::str_extract(pattern = well_roi_regex)
  
  well_roi_vec_unique <- base::unique(well_roi_vec)
  
  well_roi_df <- 
    base::data.frame("well_roi" = well_roi_vec, stringsAsFactors = FALSE)
  
  # count well images, extract ambiguous ones and create the respective 
  ambiguous_files <- 
    dplyr::group_by(.data = well_roi_df, well_roi) %>% 
    dplyr::tally() %>% 
    dplyr::filter(n > 1) %>% 
    dplyr::pull(var = "well_roi")
  
  if(base::length(ambiguous_files) != 0){
    
    ambiguous_files <-
      purrr::map(
        .x = ignore_filetypes, 
        .f = ~ stringr::str_c(ambiguous_files, .x, sep = ".")
        ) %>% 
      purrr::flatten_chr()
    
  }
  
  #  extract irrelevant, dismissed wells and create the respective regex
  dismissed_wells <- 
    dplyr::filter(.data = wp_df, information_status != "Complete") %>% 
    dplyr::pull(var = "well")
  
  if(base::length(dismissed_wells) != 0){
    
    dismissed_wells <- stringr::str_c(dismissed_wells, "\\d{1}\\.(csv|xls|xlsx)$", sep = "_")
    
  }
  
  # create a regex that discards invalid, dismissed or irrelevant directories 
  discard_pattern <- stringr::str_c(c(ambiguous_files, dismissed_wells), collapse = "|")
  
  # apply discard pattern if there are files to be discarded
  if(discard_pattern != ""){
    
    invalid_directories <- stringr::str_detect(string = wp_directories, pattern = discard_pattern)
    
    filtered_directories <- wp_directories[!invalid_directories]
    
  } else if(discard_pattern == "") {
    
    filtered_directories <- wp_directories
    
  }
  
  # filter valid directories for directories that fall in the range of well images
  # with complete information status
  wells <- 
    base::unique(relevant_wp_df$well) %>% 
    stringr::str_c(collapse = "|") %>% 
    stringr::str_c("(", ., ")")
  
  relevant_pattern <- 
    stringr::str_c(1:rois_per_well, collapse = "|") %>% 
    stringr::str_c("(", ., ")") %>% 
    stringr::str_c(wells, "_", ., "\\.(csv|xls|xlsx)$")
  
  valid_directories <- stringr::str_subset(string = filtered_directories, pattern = relevant_pattern)
  
  # obtain file availability in a tidy data.frame
  file_availability <- 
    base::data.frame(
      files = stringr::str_extract(valid_directories, file_regex), 
      stringsAsFactors = FALSE
    ) %>%
    tibble::as_tibble() %>% 
    tidyr::separate(col = files, into = c("well", "image", "file_type"), sep = "_|\\.") %>% 
    tidyr::unite(col = "well_roi", well, image, sep = "_", remove = FALSE) %>% 
    dplyr::group_by(well) %>% 
    dplyr::mutate(well_files = dplyr::n()) %>% 
    dplyr::group_by(well_roi) %>% 
    dplyr::mutate(well_roi_files = dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(well, well_files, well_roi_files)
  
  missing_files <- 
    dplyr::pull(.data = relevant_wp_df, var = "well") %>% 
    tidyr::expand_grid(well = ., image = 1:rois_per_well) %>% 
    dplyr::mutate(
      well_roi = stringr::str_c(well, image, sep = "_"), 
      missing = !well_roi %in% {{well_roi_vec_unique}}
    ) %>% 
    dplyr::filter(missing) %>% 
    dplyr::pull(var = "well_roi")
  
  # evaluate file availability in a joined data.frame  
  eval_df <- 
    dplyr::left_join(x = wp_df, y = file_availability, by = "well") %>% 
    tidyr::replace_na(replace = list(well_files = 0, well_roi_files = 0)) %>% 
    dplyr::group_by(well) %>% 
    dplyr::mutate(
      ambiguous = dplyr::if_else(base::any(well_roi_files > 1), true = TRUE, false = FALSE),
      availability_status = dplyr::case_when(
        information_status != "Complete" ~ "Dismissed", 
        ambiguous ~ "Ambiguous",
        well_files == 0 ~ "Missing", 
        well_files < rois_per_well ~ "Incomplete", 
        well_files >= rois_per_well ~ "Complete"
      ), 
      availability_status = base::factor(x = availability_status, levels = c("Complete", "Incomplete", "Missing", "Ambiguous", "Dismissed"))
    ) %>% 
    dplyr::distinct()
  
  if(base::any(eval_df$availability_status == "Ambiguous")){
    
    ambiguous_patterns <- 
      stringr::str_extract(string = valid_directories, pattern = file_regex) %>% 
      base::data.frame("files" = ., stringsAsFactors = FALSE) %>% 
      dplyr::group_by(files) %>% 
      dplyr::summarise(count = dplyr::n()) %>% 
      dplyr::filter(count > 1) %>% 
      dplyr::pull(var = "files") %>% 
      stringr::str_c(., "$", sep = "") %>% 
      stringr::str_c(collapse = "|")
    
    ambiguous_directories <- 
      stringr::str_subset(string = valid_directories, pattern = ambiguous_patterns) %>% 
      stringr::str_remove(pattern = directory) %>% 
      stringr::str_c("~", ., sep = "")
    
    ambiguous_well_rois <- 
      stringr::str_extract(string = ambiguous_directories, pattern = well_roi_regex)
    
    ambiguous_list <-
      base::vector(mode = "list", length = dplyr::n_distinct(ambiguous_well_rois)) %>% 
      magrittr::set_names(value = base::unique(ambiguous_well_rois))
    
    for(i in base::seq_along(ambiguous_directories)){
      
      aw <- ambiguous_well_rois[i]
      
      if(stringr::str_detect(ambiguous_directories[i], pattern = aw)){
        
        ambiguous_list[[aw]] <-
          base::append(
            x = ambiguous_list[[aw]],
            value = ambiguous_directories[i]
            )
        
      } 
      
    }
    
    rows <- base::names(ambiguous_list)
    cols <- 
      purrr::map_int(.x = ambiguous_list, .f = base::length) %>%
      base::max()
    
    mtr <- base::matrix(data = NA, nrow = base::length(rows), ncol = cols)
    
    base::rownames(mtr) <- rows
    base::colnames(mtr) <- stringr::str_c("Amb. Directory", 1:cols, sep = " ")
    
    for(i in base::seq_along(ambiguous_list)){
      
      well_roi <- ambiguous_list[[i]]
      
      for(d in base::seq_along(well_roi)){
        
        mtr[i,d] <- well_roi[d]
        
      }
      
    }
    
    ambiguous_df <- 
      base::as.data.frame(mtr) %>% 
      tibble::rownames_to_column(var = "Well-Image")
    
  } else {
    
    ambiguous_df <- base::data.frame()
    
  }
  
  wp_list$wp_df_eval <- eval_df
  wp_list$valid_directories <- valid_directories
  wp_list$missing_files <- missing_files
  wp_list$ambiguous_directories <- ambiguous_df
  
  base::return(wp_list)
  
}


#' @rdname evaluate_file_availability_shiny
evaluate_file_content_shiny <- function(var_name_well_plate = "none",
                                        var_name_well = "Metadata_Well",
                                        var_name_roi = "Metadata_ROI",
                                        wp_list, 
                                        df,
                                        dir,
                                        ...){
  
  # make sure that well variables exist in the right format
  
  df_clean <- 
    dplyr::rename(.data = df,
      well := {{var_name_well}},
      roi := {{var_name_roi}}
    ) %>% 
    dplyr::mutate(
      well_letter = stringr::str_extract(string = well, pattern = "^[A-Z]"), 
      well_number = stringr::str_extract(string = well, pattern = "\\d{1,2}$") %>% stringr::str_remove(pattern = "^0"),
      well = stringr::str_c(well_letter, well_number, sep = ""), 
      well_roi = stringr::str_c(well, roi, sep = "_"),
      well_letter = NULL, 
      well_number = NULL
    ) %>% 
    tibble::as_tibble()
  
  # check if all well plates exist
  
  if(var_name_well_plate != "none"){
    
    # make sure that well plate name variable exists
    df_clean <- dplyr::rename(df_clean, well_plate_name := {{var_name_well_plate}})
    
  } 
  
  well_plate_names <- base::names(wp_list)
  n_well_plates <- base::length(well_plate_names)
  
  if(var_name_well_plate == "none"){
    
    if(n_well_plates > 1){
      
      shiny_fdb(
        ui = "The experiment design contains more than one well plate. Please denote a well plate variable.", 
        type = "error",
        duration = 15
      )
      
      shiny::req(FALSE)
      
    } else {
      
      # make sure that well plate name variable exists
      df_clean$well_plate_name <- well_plate_names
      
    }
    
    missing_well_plates <- NULL
    
  } else {
    
    wp_var <- df_clean[[var_name_well_plate]]
    
    well_plates_found <- base::unique(wp_var)
    
    missing_well_plates <- well_plate_names[!well_plate_names %in% well_plates_found]
    
    if(base::length(missing_well_plates) == base::length(well_plate_names)){
      
      msg <- 
        glue::glue(
          "The file '{dir}' does not contain data of any of the well plates designed '{well_plate_names}'.", 
          well_plate_names = confuns::scollapse(missing_well_plates)
        )
      
      shiny_fdb(ui = msg, type = "error", duration = 15)
      
      shiny::req(FALSE)
      
    }
    
  }
  
  wp_list_new <-
    purrr::imap(.x = wp_list, .f = function(wp_slot, wp_name){
      
      wp_df <- wp_slot[["wp_df"]]
      
      wp_df$well_plate_name <- wp_name
      
      wp_df <- 
        dplyr::select(.data = wp_df,
                      dplyr::any_of("well_plate_name"),
                      well,
                      dplyr::everything()
        )
      
      # get total number of rois per well
      n_rois <- wp_df$rois_per_well %>% base::unique()
      
      # create vector of valid rois 
      all_rois <- 1:n_rois 
      
      df_count <- 
        dplyr::filter(df_clean, well_plate_name == {{wp_name}}) %>% 
        dplyr::select(well, roi) %>% 
        dplyr::filter(roi %in% {{all_rois}}) %>% 
        dplyr::distinct() %>% 
        dplyr::group_by(well) %>% 
        dplyr::tally(name = "well_files") 
      
      wp_df_eval <- 
        dplyr::left_join(x = wp_df, y = df_count, by = c("well")) %>%
        dplyr::mutate(
          well_files = tidyr::replace_na(data = well_files, replace = 0), 
          ambiguous = FALSE, 
          availability_status = dplyr::case_when(
            information_status != "Complete" ~ "Dismissed", 
            well_files == 0 ~ "Missing", 
            well_files < rois_per_well ~ "Incomplete", 
            well_files >= rois_per_well ~ "Complete"
          )
        ) %>% 
        dplyr::distinct()
      
      wp_slot$wp_df_eval <- wp_df_eval 
      wp_slot$directory <- dir
      wp_slot$valid_directories <- NULL
      wp_slot$missing_files <- NULL
      wp_slot$ambiguous_directories <- NULL
      
      return(wp_slot)
      
    })
  
  
  if(base::length(missing_well_plates) >= 1){
    
    msg <- 
      glue::glue(
        "The file '{dir}' does not contain data for {ref_wp} '{ref_missing_wp}'.", 
        ref_wp = confuns::adapt_reference(missing_well_plates, "well_plate"), 
        ref_missing_wp = confuns::scollapse(missing_well_plates)
      )
    
    shiny_fdb(ui = msg, type = "warning")
    
  }
  
  n_data <- 
    purrr::map_df(.x = wp_list_new, .f = ~ .x$wp_df_eval) %>% 
    dplyr::filter(information_status == "Complete") %>% 
    dplyr::pull(well_files) %>% 
    base::unique()
  
  if(base::all(n_data == 0)){
    
    msg <- 
      glue::glue(
        "Variable assignemnt resulted in no data found for any well. Please make sure to assign ",
        "the correct experiment design variables."
        )
    
    shiny_fdb(ui = msg, type = "error")
    
    shiny::req(FALSE)
    
  }
  
  return(list(wp_list_new = wp_list_new, df_clean = df_clean))
  
}


hovered_well_info_shiny <- function(df){
  
  UseMethod(generic = "hovered_well_info_shiny", object = df)
  
}


#' @title Create printable well info df
#'
hovered_well_info_shiny <- function(df){
  
  dplyr::select(
    .data = df,
    `Well:` = well,
    `Status:` = info_status,
    `Cell Line:` = cell_line,
    `Condition:` = condition
  ) 
  
}


#' @rdname hovered_well_info_shiny
#' @export
hovered_well_info_shiny_mp <- function(df){
  
  n_phases <- base::attr(df, "n_phases")
  
  c_df <- df$condition[[1]]
  
  c_vec <- 
    base::as.character(c_df[1,]) %>%
    stringr::str_c(1:n_phases, .,sep = ".") %>% 
    confuns::scollapse(sep = ", ", last = " and ")
  
  df_res <- 
    dplyr::select(
      .data = df,
      `Well:` = well,
      `Status:` = info_status,
      `Cell Line:` = cell_line
    ) %>% 
    dplyr::mutate(
      `Conditions:` = {{c_vec}}
    )
  
  return(df_res)
  
}


#' @title Read in cell track data
#' 
#' @description Reads in excel- and csv-files of all valid 
#' directories while displaying the progress via a progress bar.
#'
#' @param wp_list A well plate list.
#' @param wp_name The name of the respective well plate.
#'
#' @return A nested list obtained by \code{purrr::safely()} with 
#' successfully loaded files and failed ones including the error message.
#' 
#' @export

load_data_files_shiny <- function(wp_list,
                                  wp_name,
                                  session,
                                  object,
                                  assembled_module_info_lists,
                                  used_variable_names,
                                  mitosis_module_used){
  
  directories <- wp_list$valid_directories
  
  well_roi_files <-
    stringr::str_extract(string = directories, pattern = file_regex)
  
  n_files <- base::length(directories)
  
  # set up progress bar
  progress <- shiny::Progress$new(session, min = 1, max = n_files)
  base::on.exit(progress$close())
  
  progress$set(message = glue::glue("Reading data for well plate '{wp_name}' :"), 
               detail = glue::glue("Total of {n_files} files."))
  
  data_list <- 
    purrr::map2(.x = directories, 
                .y = base::seq_along(directories), # input for progress_n
                .f = read_data_files_shiny,
                object = object, 
                progress = progress,
                assembled_module_info_lists = assembled_module_info_lists,
                used_variable_names = used_variable_names,
                mitosis_module_used = mitosis_module_used) %>% 
    purrr::set_names(nm = well_roi_files)
  
  # sort successfull and failed loading 
  output_data_list <- 
    list(
      "successful" = purrr::keep(.x = data_list, .p = base::is.data.frame), # data.frame has been returned
      "failed" = purrr::keep(.x = data_list, .p = base::is.character) # vector of messages has been returned
    )
  
  base::return(output_data_list)
  
}




#' @title Display loading status
#' 
#' @description Creates a data.frame that displays information 
#' about the file availability evaluation undergone for every well
#' plate.
#'
#' @param well_plate_list A list of well plate lists. 
#' 

loading_status_table_shiny <- function(well_plate_list){
  
  well_plates <- base::names(well_plate_list)
  
  directories <- purrr::map_chr(.x = well_plate_list, .f = hlpr_wp_directories)
  
  n_files <- purrr::map_int(.x = well_plate_list, hlpr_wp_file_number)
  
  num_ambiguous <- purrr::map_int(.x = well_plate_list, .f = hlpr_wp_ambiguous_number)
  
  n_files_expected <- purrr::map_int(.x = well_plate_list, hlpr_wp_exp_file_number)
  
  status_df <- 
    base::data.frame(
      "wp" = well_plates, 
      "asd" = directories, 
      "nof" = n_files, 
      "enof" = n_files_expected,
      "noaf" = num_ambiguous
    ) %>% 
    dplyr::mutate(
      "ready" = dplyr::case_when(
        asd == "No directory assigned" ~ "No", 
        nof == 0 ~ "No", 
        noaf != 0 ~ "No", 
        TRUE ~ "Yes"
      )
    ) %>% 
    dplyr::rename(
      "Well Plate" = wp,
      "Assigned Directory" = asd,
      "Number of Files" = nof,
      "Expected Number of Files" = enof,
      "Number of Ambiguous Directories" = noaf, 
      "Ready to load" = ready
    )
  
}


#' @title Visualize the well plate
#'
#' @param wp_df A well-plate data.frame.
#' @param selected_wells_df A subsetted well-plate data.frame or NULL.
#' @param aes_fill Character value. Variable of \code{wp_df} to map on the 
#' fill-aesthetic.
#' @param aes_color Character value. Variable of \code{wp_df} to map on the 
#' color-aesthetic.
#' @param pt_size Numeric value. Size of points that represent the wells. 
#' @param pt_stroke Numeric value. Size of border that indicates what has been 
#' mapped on the color-aesthetic.
#' @param border Numeric value. Distance between the outer wells and the plate's
#' margin.
#'
#' @inherit ggplot_return return
#'

plot_well_plate_shiny <- function(wp_df,
                                  selected_wells_df = NULL,
                                  aes_fill,
                                  fill_values = NULL, 
                                  fill_guide = FALSE,
                                  aes_color,
                                  color_values,
                                  color_guide = TRUE,
                                  pt_size = 13.5,
                                  pt_stroke = 2,
                                  border = 0.75){
  
  limit_x <- base::max(wp_df$col_num) + border
  limit_y <- base::max(wp_df$row_num) + border
  
  if(base::is.data.frame(selected_wells_df)){
    
    geom_point_add_on <-  
      ggplot2::geom_point( data = selected_wells_df, fill = "red",
                           size = pt_size, shape = 21, alpha = 1,
                           stroke = pt_stroke ) 
    
  } else {
    
    geom_point_add_on <- NULL
    
  }
  
  if(base::is.null(fill_values)){
    
    fill_add_on <-
      confuns::scale_color_add_on(
      aes = "fill",
      variable = wp_df[[aes_fill]],
      clrp.adjust = c("unknown" = "lightgrey", "unknown & unknown" = "lightgrey"),
      clrp = "milo",
      guide = FALSE
      ) 
    
  } else {
    
    fill_add_on <- 
      ggplot2::scale_fill_manual(values = fill_values, drop = FALSE, guide = FALSE) 
    
  }
  
  if(base::isTRUE(color_guide)){
    
    color_guide <- ggplot2::guide_legend(override.aes = list(size = 15, shape = 21))
    
  } 
  
  if(base::isTRUE(fill_guide)){
    
    fill_guide <- ggplot2::guide_legend(override.aes = list(size = 15, shape = 21))
    
  } 
  
  # plot output
  ggplot2::ggplot(data = wp_df, mapping = ggplot2::aes(x = col_num, y = row_num)) + 
    ggplot2::geom_point(
      data = wp_df, 
      mapping = ggplot2::aes(fill = .data[[aes_fill]], color = .data[[aes_color]]),
      size = pt_size, shape = 21, alpha = 0.9, stroke = pt_stroke, 
    ) + 
    geom_point_add_on +
    ggplot2::geom_text(mapping = ggplot2::aes(label = well)) +
    ggforce::geom_mark_rect(
      mapping = ggplot2::aes(x = col_num, y = row_num, color = group), 
      color = "black", size = 1, expand = ggplot2::unit(15, "mm")
    ) +
    ggplot2::scale_x_continuous(limits = c(-border, limit_x)) +
    ggplot2::scale_y_reverse(limits = c(border + limit_y, -border)) +
    ggplot2::theme_void() +
    #ggplot2::scale_color_manual(values = color_values, drop = FALSE) +
    confuns::scale_color_add_on(aes = "color", variable = wp_df[[aes_color]], clrp.adjust = color_values, clrp = "milo") + 
    fill_add_on +
    ggplot2::guides(
      color = color_guide, 
      fill = fill_guide
    )
  
  
}

#' @title Quality check histogram
#'
#' @param track_summary_df Output of \code{quality_check_summary()}
#' @param aes_x Character value. 
#' @param aes_fill Character value.
#' @param lab_x Character value.
#' @param lab_fill Character value.

plot_qc_histogram_shiny <- function(track_summary_df, 
                                    aes_x = "skipped_meas", 
                                    lab_x = "Measurements",
                                    legend_position = "none"){
  
  labels_breaks <-
    dplyr::pull(track_summary_df, var = {{aes_x}}) %>% 
    base::unique()
  
  ggplot2::ggplot(data = track_summary_df, mapping = ggplot2::aes(x = .data[[aes_x]])) + 
    ggplot2::geom_histogram(position = "stack", color = "black", binwidth = 1) + 
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(color = "grey"), 
      axis.text.x = ggplot2::element_text(vjust = 5), 
      plot.title = ggplot2::element_text(face = "bold", size = 15), 
      legend.position = "none"
    ) + 
    ggplot2::scale_x_continuous(labels = labels_breaks, breaks = labels_breaks) +
    ggplot2::labs(x = lab_x, y = NULL)
  
}


#' @title Quality check barplot
#'
#' @param df A data.frame 

plot_qc_barplot_shiny <- function(df, aes_x, aes_fill, bar_position){
  
  ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = forcats::fct_infreq(.data[[aes_x]]))) + 
    ggplot2::geom_bar(mapping = ggplot2::aes(fill = .data[[aes_fill]]),
                      color = "black", position = bar_position) + 
    ggplot2::theme_minimal() + 
    ggplot2::labs(x = NULL, y = NULL, fill = NULL,
                  subtitle = stringr::str_c("n = ", base::nrow(df)))
  
}

#' @title Summarize tracking quality
#'
#' @param track_df A track data.frame

quality_check_summary_shiny <- function(track_df){
  
  dplyr::group_by(.data = track_df, cell_id) %>% 
    dplyr::filter(!frame_added) %>% 
    dplyr::summarise(
      last_meas = base::max(frame_num), 
      first_meas = base::min(frame_num), 
      total_meas = dplyr::n(), 
      skipped_meas = base::length(first_meas:last_meas) - total_meas
    )
  
}


#' @title Read example file 
#' 
#' @description Reads in the example table. 
#' 
read_example_file_shiny <- function(directory){
  
  if(stringr::str_detect(string = directory, pattern = ".csv$")){
    
    df <- 
      base::suppressMessages({
        
        base::suppressWarnings({
          
          readr::read_csv(file = directory)
          
        })
        
      })
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".xlsx$")){
    
    df <- readxl::read_xlsx(path = directory, sheet = 1)
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".xls")){
    
    df <- readxl::read_xls(path = directory, sheet = 1)
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".txt")){
    
    df <- utils::read.delim(file = directory, header = TRUE)
    
  }
  
  print(head(df))
  
  if(tibble::has_rownames(df)){
    
    df <- tibble::rownames_to_column(var = "rownames")
    
  }
  
  df <- dplyr::mutate_if(df, .predicate = is.numeric, .funs = base::round, digits = 2)  
  
  return(df)
  
}

#' @title Reads cell track data
#' 
#' @description A function to be used for argument \code{.f} of \code{purrr::map2()}.
#' 1. Calls the respective reading function depending on the directory's ending. 
#' 2. Makes sure that all denoted variables exist
#' 3. Makes sure that all denoted variables have valid content
#'
#' @param directory Character value. The directory from which to read the data.frame.
#' of \code{.x} contains.
#' @param progress A shiny - R6 progress object. 
#' @param progress_n Numeric value. Indicates the iterating progress.
#' @param assembled_module_info_lists  A list of three slots: 
#' 
#' 'identifier' = pdl_step_2_info_assembled()
#' 'analysis_modules' = pdl_step_3_info_assembled(),
#' 'additional' = pdl_step_4_info_assembled()
#'
#' @return The read in data.frame with three additional informative columns: \emph{well_roi, condition}
#' and \emph{cell_line}.

read_data_files_shiny <- function(directory,
                                  progress_n,
                                  progress,
                                  object,
                                  used_variable_names,
                                  assembled_module_info_lists,
                                  mitosis_module_used = FALSE){

  amils <- assembled_module_info_lists

  #assign("amils", value = amils, envir = .GlobalEnv)
  
  # 1. Checkpoint: Reading  -------------------------------------------------
      
  progress$set(value = progress_n)
  
  df <- 
    base::tryCatch({
      
      if(stringr::str_detect(string = directory, pattern = "csv$")){
        
        base::suppressMessages({
          
          base::suppressWarnings({
            
            readr::read_csv(file = directory)
            
          })
          
        })
        
        
      } else if(stringr::str_detect(string = directory, pattern = "xls$")){
        
        readxl::read_xls(path = directory)
        
      } else if(stringr::str_detect(string = directory, pattern = "xlsx$")){
        
        readxl::read_xlsx(path = directory)
        
      }
      
    }, error = function(error){
      
      return(stringr::str_c("Reading failed with error message:", error$message))
      
    })
  
  # if is.character -> reading resulted in error -> return error message
  if(base::is.character(df)){
    
    return(df)
    
  } else if(base::is.data.frame(df)){

    # 2. Checkpoint: Does data.frame contain data? ----------------------------
    
    # if not -> return info
    if(base::nrow(df) == 0){
      
      return("Read in data.frame contains 0 rows.")
      
    } else {

      # 3. Checkpoint: Is data of data.frame valid? -----------------------------
      
      feedback_messages <- NULL
      
      well_roi <-
        stringr::str_extract(string = directory, pattern = file_regex) %>% 
        stringr::str_extract(pattern = well_roi_regex)
      
      variable_names <- used_variable_names
      
      # 1. Are there any variables missing?
      missing_variables <- 
        purrr::map(.x = variable_names, .f = find_missing_variables_shiny, df = df) %>%
        purrr::discard(.p = base::is.null) %>% 
        purrr::flatten_chr()
      
      n_missing_variables <- base::length(missing_variables)
      
      # if so, add to feedback 
      if(n_missing_variables > 0){
        
        ref <- base::ifelse(n_missing_variables > 1, "variables", "variable")
        ref_cols <- stringr::str_c(missing_variables, collapse = "', '")
        
        msg <- glue::glue("Missing {ref}: '{ref_cols}'")
        
        feedback_messages <- c(feedback_messages, msg)
        
      } else {
        
        df <- dplyr::select(df, dplyr::all_of(used_variable_names))
        
      }
      
      # 2. Are there any variables present that are of invalid type among...
      # ... identifier variables? 
      identifier_results <- 
        base::tryCatch({
          
          validate_identifier_variables_shiny(
            df = df, 
            assembled_module_info = amils$identifier, 
            mitosis_module_used = mitosis_module_used,
            object = object,
            in_shiny = FALSE
          )
          
        }, error = function(error){
          
          return(stringr::str_c(" - ", error$message))
          
        }) %>% 
        # discard error message that indicate missing variables as 
        # missing variables are reported above
        purrr::discard(.p = hlpr_discard_messages_missing) 
      
      # if so, add to feedback 
      messages <- 
        purrr::keep(identifier_results, .p = hlpr_keep_messages)
      
      if(base::length(messages) >= 1){
        
        feedback_messages <- c(feedback_messages, messages)  
        
      }
      
      # ... analysis module variables? 
      analysis_module_results <- 
        validate_analysis_module_variables_shiny(
          assembled_module_info = amils$analysis_modules,
          df = df, 
          mitosis_module_used = mitosis_module_used,
          object = object
        )
      
      # if so, add to feedback 
      messages <- 
        purrr::keep(analysis_module_results, .p = hlpr_keep_messages)
      
      if(base::length(messages) >= 1){
        
        feedback_messages <- c(feedback_messages, messages)  
        
      }
      
      # ... additional variables? 
      additional_variables_results <- 
        validate_additional_variables_shiny(
          assembled_module_info = amils$additional,
          df = df 
        )
      
      # if so, add to feedback 
      messages <- 
        purrr::keep(additional_variables_results, .p = hlpr_keep_messages)
      
      if(base::length(messages) >= 1){
        
        feedback_messages <- c(feedback_messages, messages)  
        
      }
      
      # if feedback_messages contains any messages something went wrong
      if(base::is.character(feedback_messages) &&
         base::length(feedback_messages) >= 1){
        
        return(feedback_messages)
        
      } else { # if not, all slots in the three list contain valid data variables
        
        id_name_pairs <- 
          hlpr_module_name_pairs(amils$identifier) 
        
        analysis_module_name_pairs <- 
          purrr::map(.x = amils$analysis_modules, .f = hlpr_module_name_pairs) %>% 
          purrr::flatten_chr() 
        
        analysis_module_name_pairs <- 
          analysis_module_name_pairs[analysis_module_name_pairs != "not selected"]
        
        # assemble to list
        data_list <- 
          c(identifier_results, 
            analysis_module_results,
            additional_variables_results)
        
        # assemble to data.frame
        final_df <-
          purrr::map_df(data_list, .f = ~ .x) %>% 
          dplyr::mutate(well_roi = {{well_roi}}) %>% 
          dplyr::rename(!!!id_name_pairs) %>% 
          dplyr::rename(!!!analysis_module_name_pairs)
        
        return(final_df)
        
      }
    }
  }
}

#' wrapper around several table reading functions
read_table_shiny <- function(directory, ...){
  
  if(stringr::str_detect(string = directory, pattern = ".csv$")){
    
    df <- 
      base::suppressMessages({
        
        base::suppressWarnings({
          
          readr::read_csv(file = directory, ...)
          
        })
        
      })
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".xlsx$")){
    
    df <- readxl::read_xlsx(path = directory, sheet = 1, ...)
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".xls$")){
    
    df <- readxl::read_xls(path = directory, sheet = 1, ...)
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".txt$")){
    
    df <- utils::read.delim(file = dir, header = TRUE, ...)
    
  }
  
  return(df)
  
}

#' @title Show shiny - notifications
#'
#' @param in_shiny Logical value. 
#' @param ui Given to \code{shiny::showNotification()}.
#' @param type Given to \code{shiny::showNotification()}.
#' @param ... More arguments given \code{shiny::showNotification()}.
#'
#' @return A shiny notification.

shiny_fdb <- function(in_shiny = TRUE, ui, type = "message", ...){
  
  if(base::isTRUE(in_shiny)){
    
    shiny::showNotification(ui = ui, type = type, ...)
    
  }
  
}

#' Title
#'
#' @param assembled_module_info output of assemble_module_info()-functions (module information list)
#' @param test_overlap if set to TRUE stops execution if var names are not uniquely
#' assigned
#'
#' @return Extracts information about the name of variables carried by the example file from
#' the module information list in form of a character vector. Checks for duplicated 
#' variable name assignemnt 
#'
used_variable_names_shiny <- function(assembled_module_info, test_overlap = FALSE){
  
  if("variables" %in% base::names(assembled_module_info)){
    
    used_names_list <- 
      purrr::map(
        .x = assembled_module_info$variables,
        .f = ~ .x$name_in_example # 'nameS_in_example' in case of general variable information
      ) 
    
    used_names_vec <- 
      purrr::flatten(used_names_list) %>% 
      purrr::flatten_chr()
    
  } else {
    
    used_names_list <- 
      purrr::map(assembled_module_info, .f = function(analysis_module){
        
        purrr::map(.x = analysis_module$variables, .f = ~ .x$name_in_example)
        
      }) %>% 
      purrr::flatten()
    
    used_names_vec <- 
      purrr::flatten_chr(used_names_list)
    
  }
  
  used_names_vec <- used_names_vec[!used_names_vec %in% c("", "not selected")]
  
  if(base::isTRUE(test_overlap)){
    
    if(base::is.null(assembled_module_info$name)){
      
      step <- "Step 3: Denote module specific variables"
      
    } else if(stringr::str_detect(assembled_module_info$name, "^identification_")){
      
      step <- "Step 2: Denote identifier variables"
      
    } else if(assembled_module_info$name == "additional_data_variables"){
      
      step <- "Step 4: Denote additional variables of interest"
      
    } 
    
    if(base::length(used_names_vec) == 0){
      
      shiny_fdb(
        ui = glue::glue("Proceeding without saving any denoted variables in '{step}'."),
        duration = 10
      )
      
    } else {
      
      used_names_count <- 
        base::table(used_names_vec) %>% 
        base::as.data.frame() 
      
      overlapping_names <- 
        magrittr::set_colnames(used_names_count, value = c("name", "count")) %>% 
        dplyr::mutate(name = base::as.character(name)) %>% 
        dplyr::mutate(name = glue::glue("{name} ({count}x)")) %>% 
        dplyr::filter(count > 1) %>% 
        dplyr::pull(name)
      
      if(base::length(overlapping_names) >= 1){
        
        ovlp <- confuns::scollapse(overlapping_names)
        
        msg <-
          glue::glue(
            "Duplicated variable assignment is not allowed. ",
            "In '{step}' the following {ref} {ref2} been denoted several times: ",
            "'{ovlp}'",
            ref = confuns::adapt_reference(overlapping_names, "variable", "variables"),
            ref2 = confuns::adapt_reference(overlapping_names, "has", "have")
          )
        
        shiny_fdb(in_shiny = TRUE, ui = msg, type = "error", duration = 20)
        
        shiny::req(FALSE)
        
      }
      
    }
    
  }
  
  unique_used_names <- base::unique(used_names_vec)
  
  return(unique_used_names)
  
}



#' Title
#' @description makes sure that the denoted identifier variables are valid. 
#' to be used in observeEvent() of pdl_step4 action button 'Save & Proceed'
#' 
#' @param example_df the example file loaded in prepare data loading
#' @param assembled_module_info the respective output of assemble_module_info*()
#'  functions (module information list)
#' @param mitosis_module_used logical, skip if lineages have to be reconstructed
#' @param object 
#'
#' @return Variables (list of variables if time lapse), converted if necessary. If 
#' conversion not possible or variable missing the error message of the thrown
#' error.
#'
validate_identifier_variables_shiny <- function(df,
                                                assembled_module_info,
                                                mitosis_module_used,
                                                object,
                                                in_shiny){
  
  am_info <- assembled_module_info
  cell_id_name <- am_info$variables$cell_id$name_in_example
  
  if(isTimeLapseExp(object)){
    
    frame_num_name <- am_info$variables$frame_num$name_in_example
    
    if(base::isTRUE(mitosis_module_used)){
      
      # postpone check to after lineages and ids have been reconstructed
      res <- 
        dplyr::select(df, dplyr::any_of(c(cell_id_name, frame_num_name))) %>% 
        base::as.list()
      
    } else {
      
      res <- 
        check_and_convert_vars_frame_num_and_cell_id(
          var_frame_num = df[[frame_num_name]],
          ref_frame_num = frame_num_name,
          var_cell_id = df[[cell_id_name]],
          ref_cell_id = cell_id_name,
          in_shiny = in_shiny
        )
      
    }
    
  } else {
    
    check_content_fun <- am_info$variables$cell_id$check_content
    
    res <- 
      rlang::invoke(
        .fn = check_content_fun,
        .args = list(
          var = df[[cell_id_name]],
          ref = cell_id_name,
          in_shiny = in_shiny
        )
      )
    
  }
  
  return(res)
  
}



#' Title
#' @description makes sure that the denoted identifier variables are valid. 
#' to be used in observeEvent() of pdl_step4 action button 'Save & Proceed'
#' 
#' @param example_df the example file loaded in prepare data loading
#' @param assembled_module_info the respective output of assemble_module_info*()
#'  functions (module information list)
#' @param mitosis_module_used logical, skip if lineages have to be reconstructed
#' @param object 
#'
#' @return List of variables, converted if necessary, named according to the variable. If 
#' conversion was not possible or the variable was missing the respective slot 
#' contains the error message of the thrown error instead.
#'
validate_analysis_module_variables_shiny <- function(assembled_module_info,
                                                     df,
                                                     mitosis_module_used,
                                                     object){
  
  all_check_results <- 
    purrr::map(.x = assembled_module_info, .f = function(analysis_module){
      
      names_in_example <- 
        purrr::map_chr(.x = analysis_module$variables, .f = ~ .x$name_in_example)
      
      module_check_results <- 
        purrr::map(.x = analysis_module$variables, .f = function(variable_info){
          
          name_in_example <- variable_info$name_in_example
          
          # not selected (computable) variables do not have to be in the 
          # data frame and are computed later on 
          if(name_in_example == "not selected"){
            
            check_res <- NULL
            
          } else {
            
            check_res <- 
              base::tryCatch({
                
                base::suppressWarnings({
                  
                  rlang::invoke(
                    .fn = variable_info$check_content,
                    .args = list(
                      var = df[[name_in_example]],
                      ref = name_in_example,
                      in_shiny = FALSE
                    )
                  )
                  
                })
                
              }, error = function(error){
                
                return(stringr::str_c(" - ", error$message))
                
              })
            
          }
          
          return(check_res)
          
        }) %>% 
        purrr::set_names(nm = names_in_example) %>% 
        # discard not selected not needed vars
        purrr::discard(.p = base::is.null) %>%  
        # discard variable missing indicating messages (reported prev. to this function)
        purrr::discard(.p = hlpr_discard_messages_missing) 
      
      return(module_check_results)
      
    }) %>% 
    # flatten from list of analysis modules (list of variables) to list of all variables 
    purrr::flatten() 
  
  return(all_check_results)
  
}

#' Title
#' @description makes sure that the denoted identifier variables are valid. 
#' to be used in observeEvent() of pdl_step4 action button 'Save & Proceed'
#' 
#' @param example_df the example file loaded in prepare data loading
#' @param assembled_module_info the respective output of assemble_module_info*()
#'  functions (module information list)
#' @param mitosis_module_used logical, skip if lineages have to be reconstructed
#' @param object 
#'
#' @return List of variables, converted if necessary, named according to the variable. If 
#' conversion was not possible or the variable was missing the respective slot 
#' contains the error message of the thrown error instead.
#'
validate_additional_variables_shiny <- function(assembled_module_info, 
                                                df, 
                                                object){
  
  # check for grouping variables
  grouping_var_info <-
    assembled_module_info$variables$grouping_variable_info
  
  grouping_var_names <- grouping_var_info$name_in_example
  grouping_var_check_fun <- grouping_var_info$check_content
  
  grouping_check_results <- 
    purrr::map(.x = grouping_var_names, .f = function(name_in_example){
      
      check_res <- 
        base::tryCatch({
          
          base::suppressWarnings({
            
            rlang::invoke(
              .fn = grouping_var_check_fun,
              .args = list(
                var = df[[name_in_example]],
                ref = name_in_example,
                in_shiny = FALSE
              )
            )
            
          })
          
        }, error = function(error){
          
          return(stringr::str_c(" - ", error$message))
          
        })
      
      return(check_res)
      
      
      
    }) %>% 
    purrr::set_names(nm = grouping_var_names) %>% 
    # discard variable missing indicating messages (reported prev. to this function)
    purrr::discard(.p = hlpr_discard_messages_missing) 
  
  # check for numeric variables
  numeric_var_info <- 
    assembled_module_info$variables$numeric_variable_info
  
  numeric_var_names <- numeric_var_info$name_in_example
  numeric_var_check_fun <- numeric_var_info$check_content
  
  numeric_check_results <- 
    purrr::map(.x = numeric_var_names, .f = function(name_in_example){
      
      check_res <- 
        base::tryCatch({
          
          base::suppressWarnings({
            
            rlang::invoke(
              .fn = numeric_var_check_fun,
              .args = list(
                var = df[[name_in_example]],
                ref = name_in_example,
                in_shiny = FALSE
              )
            )
            
          })
          
        }, error = function(error){
          
          return(stringr::str_c(" - ", error$message))
          
        })
      
      return(check_res)
      
    }) %>% 
    purrr::set_names(nm = numeric_var_names) %>% 
    # discard variable missing indicating messages (reported prev. to this function)
    purrr::discard(.p = hlpr_discard_messages_missing) 
  
  all_results <- c(grouping_check_results, numeric_check_results)
  
  return(all_results)
  
}