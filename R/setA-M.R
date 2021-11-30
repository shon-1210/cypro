

#' @title Set analysis objects
#' 
#' @description Sets a convenient object and empties its data slot. 
#'
#' @inherit argument_dummy params 
#' @inherit updated_object return
#' 
#' @export
#'
setClusterConv <- function(object, cluster_object, method, variable_set, phase){
  
  cluster_object@data <- matrix()
  
  if(multiplePhases(object)){
    
    object@analysis$clustering[[method]][[variable_set]][[phase]] <- 
      cluster_object
    
  } else {
    
    object@analysis$clustering[[method]][[variable_set]] <- 
      cluster_object
    
  }
  
  base::return(object)
  
}

#' @rdname setClusterConv
#' @export
setDimRedConv <- function(object, dim_red_object, method, variable_set, phase){
  
  dim_red_object@data <- matrix()
  
  if(multiplePhases(object)){
    
    object@analysis$dim_red[[method]][[variable_set]][[phase]] <- 
      dim_red_object
    
  } else {
    
    object@analysis$dim_red[[method]][[variable_set]] <- 
      dim_red_object
    
  }
  
  base::return(object)
  
}

#' @rdname setClusterConv
#' @export
setCorrConv <- function(object, corr_object, variable_set, phase){
  
  corr_object@variables_discrete <- base::character()
  
  corr_object@data <- base::matrix()
  corr_object@meta <- dplyr::select(corr_object@meta, key)
  
  if(multiplePhases(object)){
    
    object@analysis$correlation[[variable_set]][[phase]] <- corr_object
    
  } else {
    
    object@analysis$correlation[[variable_set]] <- corr_object
    
  }
  
  base::return(object)
  
}


#' @title Set cell data.frame
#' 
#' @inherit argument_dummy params 
#' @inherit updated_object return
#' 
#' 
setCellDf <- function(object, slot, df, phase){
  
  if(multiplePhases(object)){
    
    object@cdata[[slot]][[phase]] <- df
    
  } else {
    
    object@cdata[[slot]] <- df
    
  }
  
  base::return(object)
  
}


#' @title Set cypro default
#' 
#' @inherit argument_dummy params 
#' @inherit updated_object return
#' 
#' @export
setDefaultInstructions <- function(object){
  
  object@default <- default_list
  
  return(object)
  
}



#' @title Set default storage directory
#' 
#' @inherit argument_dummy params 
#' @inherit updated_object return
#' 
#' @param directory Character value. The directory under which 
#' the object is supposed to be stored via \code{saveCyproObject()}.
#'
setStorageDirectory <- function(object, directory){
  
  confuns::is_value(directory, "character")
  
  if(!stringr::str_detect(directory, pattern = "\\.RDS$")){
    
    base::stop("Input directory has to end with '.RDS'")
    
  }
  
  object@information$storage_directory <- directory
  
  base::return(object)  
  
}


# A -----------------------------------------------------------------------




#' @title Set additional variables
#' 
#' @description Sets the variable names that are supposed to be included 
#' during the loading process. (In addition to what has been assigned to 
#' the module related variables.)
#'
#' @inherit argument_dummy params
#' @param var_names Character vector. 
#'
#' @return The input object. 
#' @export

setGeneric(name = "setAdditionalVariableNames", def = function(object, ...){
  
  standardGeneric(f = "setAdditionalVariableNames")
  
})


#' @rdname setAdditionalVariables
#' @export
setMethod(
  f = "setAdditionalVariableNames",
  signature = "ExperimentDesign",
  definition = function(object, grouping_vars = NULL, numeric_vars = NULL, in_shiny = FALSE){
    
    validate_no_overlap_additional_vars(
      grouping_vars = grouping_vars, 
      numeric_vars = numeric_vars, 
      in_shiny = in_shiny, 
      stop_if_false = TRUE
    )
    
    if(base::is.character(grouping_vars)){
      
      object@variables_grouping <- grouping_vars
      
    }
    
    if(base::is.character(numeric_vars)){
      
      object@variables_numeric <- numeric_vars
      
    }
    
    return(object)
    
  })


#' @rdname setAdditionalVariables
#' @export
setMethod(
  f = "setAdditionalVariableNames",
  signature = "Cypro",
  definition = function(object, grouping_vars = NULL, numeric_vars = NULL, in_shiny = FALSE){
    
    exp_design <- getExperimentDesign(object)
    
    exp_design <- 
      setAdditionalVariableNames(
        object = exp_design, 
        grouping_vars = grouping_vars, 
        numeric_vars = numeric_vars, 
        in_shiny = in_shiny
      )
    
    object <- setExperimentDesign(object, exp_design = exp_design)
    
    return(object)
    
  })


#' @title Set analysis aspect
#' 
#' @description Sets analysis aspect and removes data. 
#' 
#' @inherit argument_dummy params
#' 
#' @return The input object.
#' 
#' @export

setGeneric(name = "setAnalysisAspect", def = function(object, ...){
  
  standardGeneric(f = "setAnalysisAspect")
  
})

#' @rdname setAnalysisAspect
#' @export
setMethod(
  f = "setAnalysisAspect",
  signature = "Cypro", 
  definition = function(object, analysis_aspect, fset_name){
    
    aspect <- 
      base::class(analysis_aspect) %>% 
      base::tolower() %>% 
      base::as.character()
    
    analysis_aspect@data <- base::data.frame()
    analysis_aspect@data_scaled <- base::data.frame()
    
    object@analysis[[aspect]][[fset_name]] <- analysis_aspect
    
    return(object)
    
  }
)

#' @rdname setAnalysisAspect
#' @export
setMethod(
  f = "setAnalysisAspect",
  signature = "CyproTimeLapseMP", 
  definition = function(object, analysis_aspect, fset_name, phase){
    
    aspect <- 
      base::class(analysis_aspect) %>% 
      base::tolower() %>% 
      base::as.character()
    
    analysis_aspect@data <- base::data.frame()
    analysis_aspect@data_scaled <- base::data.frame()
    
    object@analysis[[aspect]][[fset_name]][[phase]] <- analysis_aspect
    
    return(object)
    
  }
)



#' @title Set empty analysis list
#'
#' @description Sets empty analysis list in slot @@analysis.
#'
#' @inherit argument_dummy params
#'
#' @return The input object.
#' @export
#'
setGeneric(name = "setAnalysisList", def = function(object, ...){
  
  standardGeneric(f = "setAnalysisList")
  
})

#' @rdname setAnalysisList
#' @export
setMethod(
  f = "setAnalysisList", 
  signature = "Cypro", 
  definition = function(object, ...){
    
    object@analysis <- cypro_analysis_list
    
    return(object)
    
  }
)


# C -----------------------------------------------------------------------


#' @title Set cell IDs
#' 
#' @description Sets a vector of all cell IDs the \code{Cypro} object
#' contains.
#' 
#' @inherit argument_dummy params
#' @param cell_ids Character vector. All cell IDs. 
#' 
#' @export
setGeneric(name = "setCellIDs", def = function(object, ...){
  
  standardGeneric(f = "setCellIDs")
  
})

#' @rdname setCellIDs
#' @export
setMethod(f = "setCellIDs", signature = "Cypro", definition = function(object, cell_ids){
  
  base::stopifnot(base::is.character(cell_ids))
  
  object@information$cell_ids <- cell_ids
  
  return(object)
  
})


#' @title Set cdata slot 
#' 
#' @description Sets slot @@cdata of the \code{Cypro} object by splitting 
#' the data.frame provided with \code{df} into its functional subcategories. 
#' 
#' @inherit argument_dummy params 
#' @param df A data.frame that contains the whole data set of the experiment
#' named according to the \code{cypro} naming convention. Can (should) be obtained
#' via \code{getMergedDf()} after loading all the data.
#' 
#' @param directory Character value. The directory under which 
#' the object is supposed to be stored via \code{saveCyproObject()}.
#' 
#' @export
#'

setGeneric(name = "setCdata", def = function(object, df, verbose = TRUE, ...){
  
  standardGeneric(f = "setCdata")
  
})

#' @rdname setCdata
#' @export
setMethod(f = "setCdata", signature = "CyproScreening", definition = function(object, df, verbose = TRUE, ...){
  
  cluster_df <- 
    dplyr::select(df, cell_id) %>% 
    dplyr::distinct() 
  
  feature_df <- 
    dplyr::select(df, cell_id, where(base::is.numeric))
  
  well_plate_df <-
    dplyr::select(df, cell_id, dplyr::all_of(var_names_dfs$well_plate)) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(var_names_dfs$well_plate), 
        .fns = base::as.factor
      )
    )
  
  layouts_df <- 
    getWellPlates(object) %>% 
    purrr::map_df(.f = ~ .x@layout) %>% 
    dplyr::select(well_plate_name, well, cell_line, condition)
  
  grouping_vars <- getAdditionalVariableNames(object)$grouping
  
  meta_df <- 
    dplyr::select(df, cell_id, dplyr::any_of(x = grouping_vars), well_plate_name, well) %>% 
    dplyr::distinct() %>% 
    dplyr::left_join(x = ., y = layouts_df, by = c("well_plate_name", "well")) %>% 
    dplyr::select(-dplyr::any_of(var_names_dfs$well_plate)) %>% 
    dplyr::mutate(cell_line = base::as.factor(cell_line), condition = base::as.factor(condition)) %>% 
    dplyr::mutate(dplyr::across(.cols = dplyr::all_of(grouping_vars), .fns = base::as.factor))
  
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

#' @rdname setCdata
#' @export
setMethod(f = "setCdata", signature = "CyproTimeLapse", definition = function(object, df, verbose = TRUE){
  
  n_frames <- nFrames(object)
  
  df <- dplyr::filter(df, frame_num <= n_frames)
  
  cluster_df <- 
    dplyr::select(df, cell_id) %>% 
    dplyr::distinct() 
  
  stats_df <- 
    dplyr::select(df, cell_id) %>% 
    dplyr::distinct() 
  
  tracks_df <- 
    dplyr::select(df, -dplyr::all_of(var_names_dfs$well_plate)) 
  
  well_plate_df <-
    dplyr::select(df, cell_id, dplyr::all_of(var_names_dfs$well_plate)) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(var_names_dfs$well_plate), 
        .fns = base::as.factor
      )
    )
  
  layouts_df <- 
    getWellPlates(object) %>% 
    purrr::map_df(.f = ~ .x@layout) %>% 
    dplyr::select(well_plate_name, well, cell_line, condition)
  
  grouping_vars <- getAdditionalVariableNames(object)$grouping
  
  grouping_vars_split <-
    validate_grouping_variables(df = tracks_df, grouping_variables = grouping_vars)
  
  valid_gvars <- grouping_vars_split$valid
  invalid_gvars <- grouping_vars_split$invalid
  
  meta_df <- 
    dplyr::select(df, cell_id, dplyr::any_of(x = valid_gvars), well_plate_name, well) %>% 
    dplyr::distinct() %>% 
    dplyr::left_join(
      x = ., 
      y = layouts_dflayouts_df[,c("well_plate_name", "well", "condition", "cell_line")],
      by = c("well_plate_name", "well")) %>% 
    dplyr::select(-dplyr::any_of(var_names_dfs$well_plate)) %>%
    dplyr::mutate(cell_line = base::as.factor(cell_line), condition = base::as.factor(condition))
  
  if(base::length(valid_gvars) >= 1){
  
    tracks_df <- 
      dplyr::select(tracks_df, -dplyr::all_of(grouping_vars_split$valid))
    
    ref_vars <- adapt_reference(valid_gvars, "variable")
    ref_valid_gvars <- scollapse(valid_gvars)
    
    msg <- 
      glue::glue(
        "Grouping {ref_vars} '{ref_valid_gvars}' are used in meta data."
      )
    
    give_feedback(msg = msg, verbose = verbose)
    
  }
  
  if(base::length(invalid_gvars) >= 1){
    
    ref_vars <- adapt_reference(ivalid_gvars, "variable")
    ref_invalid_gvars <- scollapse(invalid_gvars)
    
    msg <- 
      glue::glue(
        "Grouping {ref_vars} '{ref_invalid_gvars}' could not be ", 
        "summarized to a unique value by cell ID and remain in ", 
        "tracks data.frame."
      )
    
    give_feedback(msg = msg, verbose = verbose)
    
  }
  
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

#' @rdname setCdata
#' @export
setMethod(f = "setCdata", signature = "CyproTimeLapseMP", definition = function(object, df, verbose = TRUE){
  
  n_frames <- nFrames(object)
  
  n_phases <- nPhases(object)
  
  phase_starts <- getPhaseStarts(object)
  
  phase_names <- base::names(phase_starts)
  
  well_plate_df <- 
    dplyr::select(df, cell_id, dplyr::all_of(var_names_dfs$well_plate)) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(var_names_dfs$well_plate), 
        .fns = base::as.factor
      )
    )
  
  cluster_list <- 
    purrr::map(.x = base::seq_along(phase_starts), .f = function(p){
      
      dplyr::select(df, cell_id) %>% 
        dplyr::distinct() %>% 
        magrittr::set_attr(which = "phase", value = p)
      
    })
  
  stats_list <- 
    purrr::map(.x = base::seq_along(phase_starts), .f = function(p){ 
      
      dplyr::select(df, cell_id) %>% 
        dplyr::distinct() %>% 
        magrittr::set_attr(which = "phase", value = p)
      
    })
  
  meta_list <- 
    purrr::map(.x = phase_starts, .f = function(x){ tibble::tibble()})
  
  tracks_list <- 
    purrr::map(.x = phase_starts, .f = function(x){ tibble::tibble()})
  
  for(p in base::seq_along(phase_starts)){
    
    phase <- phase_names[p]
    
    start <- phase_starts[p]
    
    if(p > 1){
      
      # temporarily keep the first frame of the subsequent phase 
      # in the data.frame of phase a for the computations done in 
      # computeModulVariables
      start <- start - 1
      
    }
    
    if(p < n_phases){
      
      # last frame of phase a is first frame_num of next phase -1
      end <- (phase_starts[p + 1] - 1)
      
    } else {
      
      # last frame of last phase is the last frame
      end <- n_frames
      
    }
    
    layouts_df <- 
      getWellPlates(object) %>% 
      purrr::map_df(.f = function(wp){
        
        getLayoutDf(object = wp) %>% 
          merge_condition(phases = p)
        
      }) %>% 
      dplyr::select(well_plate_name, well, cell_line, condition)
    
    tracks_df <- 
      dplyr::filter(
        .data = df,
        dplyr::between(x = frame_num, left = start, right = end)
      ) %>% 
      dplyr::select(-dplyr::all_of(var_names_dfs$well_plate)) %>% 
      dplyr::select(cell_id, where(base::is.numeric)) %>% 
      magrittr::set_attr(which = "phase", value = p) %>% 
      tibble::as_tibble()
    
    tracks_list[[phase]] <- tracks_df
    
    meta_df <- 
      dplyr::left_join(
        x = well_plate_df,
        y = layouts_df[,c("well_plate_name", "well", "condition", "cell_line")],
        by = c("well_plate_name", "well")
      ) %>% 
      dplyr::select(-dplyr::any_of(var_names_dfs$well_plate)) %>%
      dplyr::distinct() %>% 
      magrittr::set_attr(which = "phase", value = p) %>% 
      tibble::as_tibble()
    
    meta_list[[phase]] <- meta_df
    
  }
  
  give_feedback(msg = "Setting cell data.", verbose = verbose)
  
  cdata_object <- 
    methods::new(
      Class = "CdataTimeLapseMP", 
      cluster = cluster_list,
      features_stats = stats_list,
      features_tracks = tracks_list,
      meta = meta_list,
      well_plate = well_plate_df
    )
  
  object@cdata <- cdata_object
  
  return(object)
  
})

#' @title Set layout cell line
#'
#' @description Sets the cell line of all wells that are selected 
#' according to variable \emph{selected} of the \code{layout_df}.
#'
#' @inherit argument_dummy params
#' @inherit setWellInfo params return
#'
#' @export
#'
setCellLine <- function(df, ...){
  
  UseMethod(generic = "setCellLine", object = df)
}

#' @rdname setCellLine
#' @export 
setCellLine.layout_df <- function(df, cell_line, in_shiny = FALSE, verbose = TRUE){
  
  if(!base::any(df$selected)){
    
    confuns::give_feedback(
      msg = "No wells are selected.",
      fdb.fn = "stop",
      with.time = FALSE, 
      in_shiny = in_shiny
    )
    
  }
  
  if(base::is.character(cell_line) | base::is.na(cell_line)){
    
    if(!base::is.na(cell_line)){
      
      confuns::is_value(x = cell_line, mode = "character")  
      
    }
    
    cell_line <- remove_empty_space(cell_line)
    
    if(base::is.character(cell_line) && cell_line == ""){
      
      msg <- "Input for 'condition' must not be an empty string. Did not set new condition."
      
      confuns::give_feedback(
        msg = msg, 
        fdb.fn = "stop",
        with.time = FALSE,
        in_shiny = in_shiny
      )
      
    } else {
      
      cell_line <- base::as.character(cell_line)
      
      df <- 
        dplyr::mutate(
          .data = df,
          cell_line = dplyr::case_when(
            selected ~ {{cell_line}},
            TRUE ~ cell_line
          )
        )
      
      n_wells <- base::sum(df$selected)
      
      confuns::give_feedback(
        msg = glue::glue("Set cell line to '{cell_line}' in {n_wells} wells."),
        with.time = FALSE,
        verbose = verbose,
        in_shiny = in_shiny
      )
      
    } 
    
  } else {
    
    confuns::give_feedback(
      msg = "Did not set cell line as none has been defined.",
      with.time = FALSE,
      verbose = verbose, 
      in_shiny = in_shiny
    )
    
  }
  
  return(df)
  
}


#' @title Set layout condition
#' 
#' @description Sets the condition of all wells that are selected 
#' according to variable \emph{selected} of the \code{layout_df}.
#'
#' @inherit argument_dummy params
#' @inherit setWellInfo params return
#'
#' @export
#'
setCondition <- function(df, ...){
  
  UseMethod(generic = "setCondition", object = df)
  
}

#' @rdname setCondition
#' @export 
setCondition.layout_df <- function(df, condition, in_shiny = FALSE, verbose = TRUE){
  
  if(!base::any(df$selected)){
    
    confuns::give_feedback(
      msg = "No wells are selected.",
      fdb.fn = "stop",
      with.time = FALSE, 
      in_shiny = in_shiny
    )
    
  }
  
  if(base::is.character(condition) | base::is.na(condition)){
    
    condition <- remove_empty_space(condition)
    
    if(base::is.character(condition) && condition == ""){
      
      msg <- "Input for 'condition' must not be an empty string. Did not set new condition."
      
      confuns::give_feedback(
        msg = msg, 
        fdb.fn = "stop", 
        with.time = FALSE, 
        in.shiny = in_shiny
      )
      
    } else {
      
      condition <- base::as.character(condition)
      
      df <- 
        dplyr::mutate(
          .data = df, 
          condition = dplyr::case_when(
            selected ~ {{condition}},
            TRUE ~ condition
          )
        )
      
      n_wells <- base::sum(df$selected)
      
      confuns::give_feedback(
        msg = glue::glue("Set condition to '{condition}' in {n_wells} wells."),
        with.time = FALSE,
        verbose = verbose,
        in.shiny = in_shiny
      )
      
    }
    
  } else {
    
    confuns::give_feedback(
      msg = "Did not set condition as none has been defined.",
      with.time = FALSE,
      verbose = verbose, 
      in.shiny = in_shiny, 
    )
    
  }
  
  return(df)
  
}

#' @rdname setCondition
#' @export 
setCondition.layout_df_mp <- function(df, condition, in_shiny = FALSE, verbose = TRUE){
  
  if(!base::any(df$selected)){
    
    confuns::give_feedback(
      msg = "No wells are selected.",
      fdb.fn = "stop",
      with.time = FALSE, 
      in.shiny = in_shiny
    )
    
  }
  
  n_phases <- base::attr(df, "n_phases")
  
  if(base::length(condition) == 1 && base::is.na(condition)){
    
    condition <- base::rep(x = NA, n_phases)
    
  }
  
  all_vals_chr <- base::all(base::is.character(condition)) 
  all_vals_na <- base::all(base::is.na(condition))
  
  if(!all_vals_chr & !all_vals_na){
    
    confuns::give_feedback(
      msg = "Input for argument condition must contain only character values or only NA.", 
      fdb.fn = "stop", 
      with.time = FALSE
    )
    
  }
  
  if(all_vals_chr | all_vals_na){
    
    n_conditions <- base::length(condition)
    
    n_phases <- base::attr(df, "n_phases")
    
    if(n_conditions != n_phases){
      
      msg <- 
        glue::glue(
          "Number of conditions specified must be equal ",
          "to number of phases ({n_phases}). ", 
          "Did not set new conditions."
        )
      
      confuns::give_feedback(
        msg = msg, 
        fdb.fn = "stop", 
        with.time = FALSE,
        in.shiny = in_shiny
      )
      
    }
    
    condition <- purrr::map_chr(.x = condition, .f = ~ remove_empty_space(.x))
    
    if(all_vals_chr && base::any(condition == "")){
      
      msg <- "Input for 'condition' must not contain empty strings. Did not set new conditions."
      
      confuns::give_feedback(
        msg = msg, 
        fdb.fn = "stop",
        with.time = FALSE,
        in.shiny = in_shiny
      )
      
    } else {
      
      condition <- base::as.character(condition)
      
      df$condition <- 
        purrr::map2(
          .x = df$condition, 
          .y = df$selected, 
          .f = function(condition_df, sel){
            
            if(base::isTRUE(sel)){
              
              condition_df[1, 1:n_phases] <- condition
              
            }
            
            return(condition_df)
            
          }
        )
      
      n_wells <- base::sum(df$selected)
      
      if(all_vals_na){
        
        msg <- 
          glue::glue(
            "Set conditions to 'NA' in {n_wells} wells."
          )
        
      } else {
        
        conditions <- 
          stringr::str_c(1:n_conditions, condition, sep = ".") %>% 
          confuns::scollapse()
        
        msg <- 
          glue::glue(
            "Set conditions to '{conditions}' in {n_wells} wells.", 
          )
        
      }
      
      confuns::give_feedback(
        msg = msg, 
        with.time = FALSE, 
        verbose = verbose, 
        in.shiny = in_shiny
      )
      
    }
    
  } else {
    
    confuns::give_feedback(
      msg = "Did not set conditions as none have been defined.", 
      with.time = FALSE,
      verbose = verbose,
      in.shiny = in_shiny
    )
    
  }
  
  return(df)
  
}


# D -----------------------------------------------------------------------

#' @title Set layout data status
#' 
#' @description Sets the variable \emph{data_status} of the layout  
#' data.frame according to the variables \emph{info_status} and 
#' \emph{file_status}.
#'
#' @inherit argument_dummy params
#' @inherit setWellInfo params return
#'
#' @export
#'
setDataStatus <- function(df){
  
  UseMethod(generic = "setDataStatus", object = df)
  
}

#' @rdname setDataStatus
#' @export
setDataStatus.layout_df <- function(df){
  
  nested_input <- isNested(df)
  
  df <- 
    purrr::map_df(
      .x = getWells(df), 
      .f = ~ dplyr::filter(df, well == {{.x}}) %>% set_data_status_hlpr() 
    )
  
  if(nested_input){
    
    df <- nestLayoutDf(df)
    
  } else {
    
    df <- unnestLayoutDf(df)
    
  }
  
  return(df)
  
}

set_data_status_hlpr <- function(df){
  
  df <- nestLayoutDf(df)
  
  if(df$info_status != "Complete"){
    
    df$data_status <- "Dismissed"
    
  } else {
    
    udf <- unnestLayoutDf(df)
    
    if(base::all(udf$file_status == "Missing")){
      
      df$data_status <- "Missing"
      
    } else if(base::any(udf$file_status == "Ambiguous")){
      
      df$data_status <- "Ambiguous"
      
    } else if(base::all(udf$file_status == "Complete")){
      
      df$data_status <- "Complete"
      
    } else {
      
      df$data_status <- "Incomplete"
      
    }
    
    df$data_status <- base::factor(df$data_status, levels = data_status_levels)
    
  }
  
  return(df)
  
}



# E -----------------------------------------------------------------------

#' @title Set experiment design 
#' 
#' @description Sets the experiment design of the \code{Cypro} object.
#'
#' @inherit argument_dummy params
#' @param exp_design An object of class \code{ExperimentDesign}.
#'
#' @return The input object.
#' @export
#'
setGeneric(name = "setExperimentDesign", def = function(object, exp_design){
  
  standardGeneric(f = "setExperimentDesign")
  
})

#' @rdname setExperimentDesign
#' @export
setMethod(f = "setExperimentDesign", signature = "Cypro", definition = function(object, exp_design){
  
  isOfClass(x = exp_design, valid_class = "ExperimentDesign", stop_if_false = TRUE)
  
  object@experiment_design <- exp_design
  
  return(object)
  
})

#' @title Set experiment name
#' 
#' @description Sets the content for slot @@experiment in the \code{Cypro} object and
#' all S4 objects that have an @@experiment slot.
#' 
#' @inherit argument_dummy params
#' @param name Character value. The experiment name. 
#' 
#' @return The input object.

setGeneric(name = "setExperimentName", def = function(object, name){
  
  standardGeneric(f = "setExperimentName")
  
})

#' @rdname setExperimentName
#' @export
setMethod(f = "setExperimentName", signature = "Cypro", definition = function(object, name){
  
  is_value(x = name, mode = "character")
  
  # cypro
  object@experiment <- name
  
  # experiment_design
  object@experiment_design@experiment <- name
  
  # well plates
  for(well_plate in getWellPlateNames(object)){
    
    wp <- getWellPlate(object, well_plate = well_plate)
    
    wp@experiment <- name
    
    object <- setWellPlate(object, well_plate_object = wp)
    
  }
  
  return(object)
  
})


# F -----------------------------------------------------------------------

#' @title Set feature data.frame
#' 
#' @inherit argument_dummy params
#' 
#' @return The input object.
#' @export
setGeneric(name = "setFeatureDf", def = function(object, df){
  
  standardGeneric(f = "setFeatureDf")
  
})

#' @rdname setFeatureDf
#' @export
setMethod(f = "setFeatureDf", signature = "CyproScreening", definition = function(object, df){
  
  object@cdata@features <- base::as.data.frame(df)
  
  return(object)
  
})




# I -----------------------------------------------------------------------


#' @title Sets image directory data.frame
#' 
#' @description Sets an \bold{empty} data.frame in slot @@image_directories
#' of the \code{Cypro} object. Do not confuse with \code{setImageDirectories()}.
#'
#' @inherit argument_dummy params
#' @param df The image directory data.frame.
#' 
#' @return The input object.
#' @export
#'

setGeneric(name = "setImageDirDf", def = function(object,df = NULL, force = FALSE){
  
  standardGeneric(f = "setImageDirDf")
  
})


#' @rdname setImageDirDf
#' @export
setMethod(f = "setImageDirDf", signature = "Cypro", definition = function(object, df = NULL, force = FALSE){
  
  idir_df <- getImageDirDf(object)
  
  if(base::nrow(idir_df) >= 1 && !base::isTRUE(force)){
    
    stop(
      "Image directory data.frame has already been set. Set argument 'force' to ", 
      "TRUE to overwrite it."
    )
    
  }
  
  if(!base::is.data.frame(df)){
    
    object@image_directories <- 
      getWellPlateDf(object) %>% 
      dplyr::select(well_plate_name, well_plate_index, well, roi) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(dir = NA_character_) %>% 
      base::as.data.frame()
    
  } else {
    
    object@image_directories <- df
    
  }
  
  return(object)
  
})

#' @rdname setImageDirDf
#' @export
setMethod(f = "setImageDirDf", signature = "CyproTimeLapse", definition = function(object){
  
  idir_df <- getImageDirDf(object)
  
  if(base::nrow(idir_df) >= 1 && !base::isTRUE(force)){
    
    stop(
      "Image directory data.frame has already been set. Set argument 'force' to ", 
      "TRUE to overwrite it."
    )
    
  }
  
  if(!base::is.data.frame(df)){
    
    object@image_directories <- 
      getTracksDf(object, with_well_plate = TRUE) %>% 
      dplyr::select(well_plate_name, well_plate_index, well, roi, frame_num) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(dir = NA_character_, stack = NA)
    
  } else {
    
    object@image_directories <- df
    
  }
  
  return(object)
  
})

#' @title Sets info status
#' 
#' @description Sets variable \emph{info_status} of the \code{layout_df}
#' to one of \emph{'Missing', 'Incomplete', 'Complete'} depending on the 
#' information found in variables \emph{cell_line} and \emph{condition}.
#'
#' @inherit setWellInfo params return
#'
#' @return
#' @export
#'
setInfoStatus <- function(df){
  
  UseMethod(generic = "setInfoStatus", object = df)
  
}

#' @rdname setInfoStatus
#' @export 
setInfoStatus.layout_df <- function(df){
  
  df <- 
    dplyr::mutate(
      .data = df, 
      info_status = base::as.character(info_status),
      info_status = dplyr::case_when(
        base::is.na(cell_line) & base::is.na(condition) ~ "Missing", 
        base::is.character(cell_line) & base::is.na(condition) ~ "Incomplete", 
        base::is.na(cell_line) & base::is.character(condition) ~ "Incomplete",
        base::is.character(cell_line) & base::is.character(condition) ~ "Complete"
      ), 
      info_status = base::factor(info_status, levels = levels_info_status)
    )
  
  return(df)
  
}

#' @rdname setInfoStats
#' @export
setInfoStatus.layout_df_mp <- function(df){
  
  dfm <- mergeCondition(df)
  
  dfm <- 
    dplyr::mutate(
      .data = dfm, 
      info_status = base::as.character(info_status),
      info_status = dplyr::case_when(
        base::is.na(cell_line) & base::is.na(condition) ~ "Missing", 
        base::is.character(cell_line) & base::is.na(condition) ~ "Incomplete", 
        base::is.na(cell_line) & base::is.character(condition) ~ "Incomplete",
        base::is.character(cell_line) & base::is.character(condition) ~ "Complete"
      ), 
      info_status = base::factor(info_status, levels = levels_info_status)
    )
  
  df$info_status <- dfm$info_status
  
  return(df)
  
}

#' @rdname setDataStatus
#' @export
setInfoStatus.layout_df_mp <- function(df){
  
  n_rows <- base::nrow(df)
  
  condition_complete <- base::vector(length = n_rows)
  
  for(i in 1:n_rows){
    
    condition_added <- !base::is.na(df$condition[[i]][1,])
    
    condition_complete[i] <- base::all(condition_added)
    
  }
  
  df$condition_complete <- condition_complete
  
  df <- 
    dplyr::mutate(
      .data = df, 
      info_status = base::as.character(info_status),
      info_status = dplyr::case_when(
        base::is.na(cell_line) & !condition_complete ~ "Missing", 
        base::is.character(cell_line) & condition_complete ~ "Incomplete", 
        base::is.na(cell_line) & condition_complete ~ "Incomplete",
        base::is.character(cell_line) & condition_complete ~ "Complete"
      ), 
      info_status = base::factor(info_status, levels = levels_info_status)
    ) %>% 
    dplyr::select(-condition_complete)
  
  return(df)
  
}

#' @title Set input example
#' 
#' @description Sets the input example during \code{assignVariables()} in 
#' slot @@example_df of the S4-class \code{ExperimentDesign}.
#' 
#' @inherit argument_dummy params
#' @param df The example data.frame
#' @param dir Character value. The file directory of the loaded data.frame.
#' @return The input object.
#' 
#' @export
#'
setGeneric(name = "setInputExample", def = function(object, df, dir){
  
  standardGeneric(f = "setInputExample")
  
})

#' @rdname setInputExample
#' @export
setMethod(f = "setInputExample", signature = "ExperimentDesign", def = function(object, df, dir){
  
  confuns::is_value(x = dir, mode = "character")
  
  object@example_df <- df
  object@example_dir <- dir
  
  return(object)
  
})

#' @rdname setInputExample
#' @export
setMethod(f = "setInputExample", signature = "Cypro", def = function(object, df, dir){
  
  exp_design <- getExperimentDesign(object)
  
  exp_design <- 
    setInputExample(
      object = exp_design, 
      df = df, 
      dir = dir
    )
  
  object <- setExperimentDesign(object, exp_design = exp_design)
  
  return(object)
  
})



# L -----------------------------------------------------------------------


#' @title Set layout data.frame attributes
#' 
#' @description Safely sets layout data.frame specific attributes. 
#' 
#' @param df A data.frame of class \code{layout_df}.
#' @param attr A named list to be added to the attributes of input data.frame.
#' 
#' @return A data.frame.
#' 
#' @export

setLayoutAttributes <- function(df, attr){
  
  attr_names <- 
    confuns::keep_named(attr) %>% 
    base::names()
  
  layout_attr <- base::attributes(df)
  
  for(name in attr_names){
    
    layout_attr[[name]] <- attr[[name]]
    
  }
  
  base::attributes(df) <- layout_attr
  
  return(df)
  
}

#' @title Set layout data.frame
#' 
#' @description Places the well plate layout data.frame in the correct slot 
#' of the object provided. 
#' 
#' @inherit argument_dummy params
#' @param df Data.frame of class \code{layout_df}.
#' 
#' @export 
#' 
setGeneric(name = "setLayoutDf", def = function(object, layout_df, ...){
  
  standardGeneric(f = "setLayoutDf")
  
})


#' @rdname setLayoutDf
#' @export
setMethod(f = "setLayoutDf", signature = "WellPlate", definition = function(object, layout_df, ...){
  
  base::stopifnot(isOfClass(layout_df,"layout_df"))
  
  object@layout <- base::as.data.frame(layout_df)
  
  return(object)
  
})

#' @rdname setLayoutDf
#' @export
setMethod(f = "setLayoutDf", signature = "ExperimentDesign", definition = function(object, layout_df, well_plate){
  
  confuns::check_one_of(
    input = well_plate, 
    against = base::names(object@well_plates)
  )
  
  object@well_plates[[well_plate]]@layout <- layout_df
  
  return(object)
  
})

#' @rdname setLayoutDf
#' @export
setMethod(f = "setLayoutDf", signature = "Cypro", definition = function(object, layout_df, well_plate){
  
  confuns::check_one_of(
    input = well_plate, 
    against = getWellPlateNames(object)
  )
  
  exp_design <- getExperimentDesign(object)
  
  exp_design <- setLayoutDf(exp_design, layout_df = layout_df, well_plate = well_plate)
  
  object@experiment_design <- exp_design 
  
  return(object)
  
})


#' @title Set loading modality
#' 
#' @description Sets the slot @@loading_modality of each well plate based on the content of the input 
#' example data.frame.
#' 
#' @inherit argument_dummy params
#' 
#' @return The input object.
#' 
setGeneric(name = "setLoadingModality", def = function(object){
  
  standardGeneric(f = "setLoadingModality")
  
})

#' @rdname setLoadingModality
#' @export
setMethod(f = "setLoadingModality", signature = "Cypro", definition = function(object){
  
  ldm <- suggestLoadingModality(object)
  
  well_plate_list <- getWellPlates(object)
  
  for(i in base::seq_along(well_plate_list)){
    
    well_plate_list[[i]]@loading_modality <- ldm
    
    object <- setWellPlate(object, well_plate_object = well_plate_list[[i]])
    
  }
  
  return(object)
  
})





# M -----------------------------------------------------------------------

#' @title Set module activity
#' 
#' @description Sets slot @@used of the analysis modules which activates 
#' or inactivates the respective modules.  
#'
#' @inherit argument_dummy params
#' @param activity Logical vector. Must be named according to the modules for which
#' slot @@activity is supposed to be set.  
#'
#' @return The input object
#' @export
#'

setGeneric(name = "setModuleActivity", def = function(object, ...){
  
  standardGeneric("setModuleActivity")
  
})


#' @rdname setModuleActivity
#' @export
setMethod(f = "setModuleActivity", signature = "Cypro", definition = function(object, activity, in_shiny = FALSE, verbose = TRUE){
  
  activity <- confuns::keep_named(input = activity)
  
  activity[base::names(activity) %in% c("identification", "identification_timelapse")] <- NULL
  
  names_activity <- base::names(activity) 
  
  confuns::check_one_of(
    input = names_activity, 
    against = base::names(object@modules)
  )
  
  if(confuns::is_list(input = activity)){
    
    activity <- 
      purrr::flatten_lgl(.x = activity) %>% 
      purrr::set_names(nm = base::names(activity))
    
  }
  
  activity <- activity[!base::names(activity) == "identification"]
  
  for(module_name in base::names(activity)){
    
    m_activity <- activity[module_name]
    
    if(object@modules[[module_name]]@active != m_activity){
      
      object@modules[[module_name]]@active <- m_activity
      
      ref_activity <- base::ifelse(test = m_activity, yes = "Activated", no = "Deactivated")
      ref_module <- object@modules[[module_name]]@name_in_app
      
      if(base::isTRUE(in_shiny)){
        
        fdb_fn <- base::ifelse(test = m_activity, yes = "message", no = "warning")
        
      } else {
        
        fdb_fn <- "message"
        
      }
      
      confuns::give_feedback(
        msg = glue::glue("{ref_activity} module '{ref_module}'."),
        verbose = verbose, 
        in.shiny = in_shiny, 
        with.time = FALSE, 
        fdb.fn = fdb_fn
      )
      
    }
    
  }
  
  return(object)
  
})
