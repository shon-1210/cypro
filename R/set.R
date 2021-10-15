

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
        in_shiny = in_shiny
      )
      
    } else {
      
      condition <- base::as.character(condition)
      
      df <- 
        dplyr::mutate(
          .data = df, 
          condition = dplyr::case_when(
            selected ~ {{condition}},
            TRUE ~ cell_line
          )
        )
      
      n_wells <- base::sum(df$selected)
      
      confuns::give_feedback(
        msg = glue::glue("Set condition to '{condition}' in {n_wells} wells."),
        with.time = FALSE,
        verbose = verbose,
        in_shiny = in_shiny
      )
      
    }
    
  } else {
    
    confuns::give_feedback(
      msg = "Did not set condition as none has been defined.",
      with.time = FALSE,
      verbose = verbose, 
      in_shiny = in_shiny, 
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
      in_shiny = in_shiny
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
        in_shiny = in_shiny
      )
      
    }
    
    condition <- purrr::map_chr(.x = condition, .f = ~ remove_empty_space(.x))
    
    if(all_vals_chr && base::any(condition == "")){
      
      msg <- "Input for 'condition' must not contain empty strings. Did not set new conditions."
      
      confuns::give_feedback(
        msg = msg, 
        fdb.fn = "stop",
        with.time = FALSE,
        in_shiny = in_shiny
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
        in_shiny = in_shiny
      )
      
    }
    
  } else {
    
    confuns::give_feedback(
      msg = "Did not set conditions as none have been defined.", 
      with.time = FALSE,
      verbose = verbose,
      in_shiny = in_shiny
    )
    
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


# L -----------------------------------------------------------------------


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
#' @param well_plate Object of class \code{WellPlate}.
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
setMethod(f = "setWellPlate", signature = "ExperimentDesign", function(object, well_plate){
  
  base::stopifnot(object@experiment == well_plate@experiment)
  
  confuns::is_vec(
    x = well_plate@name, 
    ref = "well_plate@name",
    mode = "character",
    of.length = 1
  )
  
  object@well_plates[[well_plate@name]] <- well_plate
  
  return(object)
  
})







