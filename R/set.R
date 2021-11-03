

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




# C -----------------------------------------------------------------------

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


# F -----------------------------------------------------------------------


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
      selected = NULL, 
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
  
  base::stopifnot(methods::is(layout_df, class2 = "layout_df"))
  
  object@layout <- layout_df
  
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

# P -----------------------------------------------------------------------


#' @title Set progress status
#' 
#' @description Sets logical values in the object's slot @@progress to keep 
#' track of the progress made during the preparation process.
#'
#' @inherit setWellInfo params return
#' @param designExperiment,assignVariables,loadData,processData TRUE or FALSE.
#' 
#'
#' @return The input object.
#' @export
#'

setGeneric(name = "setProgress", def = function(object, ...){
  
  standardGeneric(f = "setProgress")
  
})

#' @rdname setProgress
#' @export
setMethod(
  f = "setProgress",
  signature = "Cypro",
  definition = function(object,
                        designExperiment = NULL,
                        assignVariables = NULL,
                        loadData = NULL, 
                        processData = NULL, 
                        verbose = TRUE){
    
    if(base::isTRUE(designExperiment) | base::isFALSE(designExperiment)){
      
      methods::slot(object@progress, "designExperiment") <- designExperiment
      
      confuns::give_feedback(
        msg = glue::glue("Progress status of function 'designExperiment()' has been set to {designExperiment}."),
        verbose = verbose
      )
      
    }
    
    if(base::isTRUE(assignVariables) | base::isFALSE(assignVariables)){
      
      methods::slot(object@progress, "assignVariables") <- assignVariables
      
      confuns::give_feedback(
        msg = glue::glue("Progress status of function 'assignVariables()' has been set to {assignVariables}."),
        verbose = verbose
      )
      
    }
    
    if(base::isTRUE(loadData) | base::isFALSE(loadData)){
      
      methods::slot(object@progress, "loadData") <- loadData
      
      confuns::give_feedback(
        msg = glue::glue("Progress status of function 'loadData()' has been set to {loadData}."),
        verbose = verbose
      )
      
    }
    
    if(base::isTRUE(processData) | base::isFALSE(processData)){
      
      methods::slot(object@progress, "processData") <- processData
      
      confuns::give_feedback(
        msg = glue::glue("Progress status of function 'processData()' has been set to {processData}."),
        verbose = verbose
      )
      
    }
    
    return(object)
    
  })


# S -----------------------------------------------------------------------

#' @rdname setTracksDf
#' @export
setGeneric(name = "setStatsDf", def = function(object, df){
  
  standardGeneric(f = "setStatsDf")
  
})

#' @rdname setStatsDf
#' @export
setMethod(f = "setStatsDf", signature = "CdataTimeLapse", definition = function(object, df){
  
  object@features_stats <- base::as.data.frame(df)
  
  return(object)
  
})

#' @rdname setStatsDf
#' @export
setMethod(f = "setStatsDf", signature = "CyproTimeLapse", definition = function(object, df){
  
  object@cdata@features_stats <- base::as.data.frame(df)
  
  return(object)
  
})


# T -----------------------------------------------------------------------

#' @title Set cypro data.frames
#' 
#' @description Functions to safely set the data.frames containing 
#' data about the cells. 
#'
#' @inherit argument_dummy params
#' @param df The data.frame to be set. 
#'
#' @return The input object. 
#' @export
#'
setGeneric(name = "setTracksDf", def = function(object, df){
  
  standardGeneric(f = "setTracksDf")
  
})

#' @rdname setTracksDf
#' @export
setMethod(f = "setTracksDf", signature = "CdataTimeLapse", definition = function(object, df){
  
  object@features_tracks <- base::as.data.frame(df)
  
  return(object)
  
})

#' @rdname setTracksDf
#' @export
setMethod(f = "setTracksDf", signature = "CyproTimeLapse", definition = function(object, df){
  
  object@cdata@features_tracks <- base::as.data.frame(df)
  
  return(object)
  
})


# V -----------------------------------------------------------------------


#' @title Set variable assinment 
#' 
#' @description Only to use within \code{assignVariables()} as it is relying 
#' on the input list that exists in active shiny sessions. 
#' 
#' Picks the variable assignment from the input list of the shiny session and 
#' assigns it to the respective variable of the module.  
#'
#' @inherit argument_dummy params
#' @param input_list The list to which one refers as \code{input} in shiny sessions
#' outputted by \code{shiny::reactiveValuesToList()}.
#'
#' @return The input object. 
#' @export
#'
setGeneric(name = "setVariableAssignmentShiny", def = function(object, ...){
  
  standardGeneric(f = "setVariableAssignmentShiny")
  
})

#' @rdname setVariableAssignmentShiny
#' @export
setMethod(f = "setVariableAssignmentShiny", signature = "AnalysisModule", function(object, input_list){
  
  module_name <- object@name_in_cypro
  
  object@variables_optional <- 
    purrr::map(
      .x = object@variables_optional,
      .f = ~ set_variable_assignment_shiny_hlpr(variable = .x, input_list = input_list, module_name = module_name)
    )
  
  object@variables_required <- 
    purrr::map(
      .x = object@variables_required,
      .f = ~ set_variable_assignment_shiny_hlpr(variable = .x, input_list = input_list, module_name = module_name)
    )
  
  object@variables_computable <- 
    purrr::map(
      .x = object@variables_computable,
      .f = ~ set_variable_assignment_shiny_hlpr(variable = .x, input_list = input_list, module_name = module_name)
    )
  
  return(object)
  
})


#' @rdname setVariableAssignmentShiny
#' @export

setMethod(
  f = "setVariableAssignmentShiny",
  signature = "Cypro",
  definition = function(object, input_list, modules = NULL, validate = TRUE){
    
  object@modules <- 
    purrr::map(
      .x = object@modules,
      .f = function(module){
        
        if(base::is.null(modules) | module@name_in_cypro %in% modules & base::isTRUE(module@active)){
          
          module <- setVariableAssignmentShiny(object = module, input_list = input_list)
          
        }
        
        return(module)
        
      })
  
  if(base::isTRUE(validate)){

    validateVariableAssignment(
      object = object, 
      modules = modules, 
      in_shiny = TRUE, 
      stop_if_false = TRUE
    )    
    
  }

  
  return(object)
  
})

set_variable_assignment_shiny_hlpr <- function(variable, input_list, module_name){
  
  mpattern <- stringr::str_c(module_name, "$", sep = "")
  
  vpattern <- variable@name_in_cypro
  
  pattern <- stringr::str_c("vardenotation", vpattern, mpattern, sep = "-")
  
  name_in_example <- 
    confuns::lselect(
      lst = input_list,
      matches(pattern)
      #contains("vardenotation-") &
        #matches(vpattern) &
        #matches(mpattern)
    ) %>% 
    purrr::flatten_chr()
  
  if(name_in_example == "NA"){
    
    variable@name_in_example <- NA_character_
    
  } else {
    
    variable@name_in_example <- name_in_example
    
  }
  
  return(variable)
  
}


# W -----------------------------------------------------------------------

#' @title Set well plate layout information
#' 
#' @description Subsequently fills the well plate \code{layout_df} with information.
#'
#' @inherit argument_dummy params
#' @param df A data.frame of a class for which a method has been defined.
#' @param selected_wells Character vector. Denotes the wells for which the information 
#' is supposed to be set.
#' @param cell_line Character value, NA or NULL. The cell line seeded in the wells denoted in 
#' \code{selected_wells}.
#' @param condition Character value, NA, or NULL. The condition of the well. If multiple phase
#' experiment a character vector of length equal to the number of phases of
#' the experiment design.
#'
#' @return A data.frame.
#' @export

setWellInfo <- function(df, ...){
  
  UseMethod(generic = "setWellInfo", object = df)
  
}

#' @rdname setWellInfo
#' @export 
setWellInfo.layout_df <- function(df,
                                  selected_wells,
                                  cell_line = NULL,
                                  condition = NULL,
                                  verbose = TRUE, 
                                  in_shiny = FALSE){
  
  confuns::is_vec(x = selected_wells, mode = "character", min.length = 1)
  
  confuns::check_one_of(
    input = selected_wells, 
    against = df$well
  )
  
  df$selected <- df$well %in% selected_wells
  
  df <- setCondition(df = df, condition = condition, in_shiny = in_shiny, verbose = verbose)
  
  df <- setCellLine(df = df, cell_line = cell_line, in_shiny = in_shiny, verbose = verbose)
  
  df <- setInfoStatus(df)
  
  return(df)
  
}



#' @title Set well plate S4 object
#' 
#' @description Sets an object of class \code{WellPlate} in the respective
#' slot of input for argument \code{object}.
#'
#' @inherit argument_dummy params
#' @param well_plate_object Object of class \code{WellPlate}.
#' 
#' @details Slots @@name and @@experiment of the \code{WellPlate} object must not be empty.
#' 
#' @return The input object.
#' 
#' @seealso \code{WellPlate-class}
#' 
#' @export
#'
setGeneric(name = "setWellPlate", def = function(object, ...){
  
  standardGeneric(f = "setWellPlate")
  
})


#' @rdname setWellPlate
#' @export
setMethod(f = "setWellPlate", signature = "ExperimentDesign", function(object, well_plate_object){
  
  base::stopifnot(object@experiment == well_plate_object@experiment)
  
  confuns::is_vec(
    x = well_plate_object@name, 
    ref = "well_plate_object@name",
    mode = "character",
    of.length = 1
  )
  
  object@well_plates[[well_plate_object@name]] <- well_plate_object
  
  return(object)
  
})

#' @rdname setWellPlate
#' @export
setMethod(f = "setWellPlate", signature = "Cypro", definition = function(object, well_plate_object){
  
  exp_design <- getExperimentDesign(object)
  
  exp_design <- 
    setWellPlate(
      object = exp_design, 
      well_plate_object = well_plate_object
    )
  
  object <- setExperimentDesign(object, exp_design = exp_design)
  
  return(object)
  
})







