

# O -----------------------------------------------------------------------

#' @title Set object of class \code{OutlierDetection}
#' 
#' @description Sets object of class \code{OutlierDetection}. Removes
#' content of slot @@data beforehand to prevent accumulation of equal
#' data.frames in the \code{Cypro} object. 
#' 
#' @inherit argument_dummy params
#' @param outlier_object Object of class \code{OutlierDetection}.
#' 
#' @return The input object. 
#' 
setGeneric(name = "setOutlierDetection", def = function(object, ...){
  
  standardGeneric(f = "setOutlierDetection")
  
})

#' @rdname setOutlierDetection
#' @export
setMethod(f = "setOutlierDetection", signature = "Cypro", definition = function(object, outlier_object){
  
  outlier_object@data <- data.frame()
  
  object@quality_checks[["outliers"]] <- outlier_object
  
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
setGeneric(name = "setStatsDf", def = function(object, df, ...){
  
  standardGeneric(f = "setStatsDf")
  
})

#' @rdname setTracksDf
#' @export
setMethod(f = "setStatsDf", signature = "CdataTimeLapse", definition = function(object, df){
  
  object@features_stats <- base::as.data.frame(df)
  
  return(object)
  
})

#' @rdname setTracksDf
#' @export
setMethod(f = "setStatsDf", signature = "CyproTimeLapse", definition = function(object, df){
  
  object@cdata@features_stats <- base::as.data.frame(df)
  
  return(object)
  
})

#' @rdname setTracksDf
#' @export
setMethod(f = "setStatsDf", signature = "CyproTimeLapseMP", definition = function(object, df, phase){
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  object@cdata@features_stats[[phase]] <- base::as.data.frame(df)
  
  return(object)
  
})

#' @title Set storage directory
#' 
#' @description Sets the directory under which the \code{Cypro} object
#' is saved by default with \code{saveCyproObject()}.
#' 
#' @inherit argument_dummy params
#' 
#' @param directory Character value. The file directory. Must end with \emph{'.RDS'}.
#' 
#' @return The input object. 
#' 
setGeneric(name = "setStorageDirectory", def = function(object, ...){
  
  standardGeneric(f = "setStorageDirectory")
  
}) 

#' @rdname setStorageDirectory
#' @export
setMethod(f = "setStorageDirectory", signature = "Cypro", definition = function(object, directory, verbose = TRUE){
  
  base::stopifnot(stringr::str_detect(directory, pattern = "RDS$"))
  
  object@information$storage_directory <- directory
  
  give_feedback(
    msg = glue::glue("Set directory '{directory}'."),
    verbose = verbose, 
    with.time = FALSE
  )
  
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
setGeneric(name = "setTracksDf", def = function(object, df, ...){
  
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

#' @rdname setTracksDf
#' @export
setMethod(f = "setTracksDf", signature = "CyproTimeLapseMP", definition = function(object, df, phase){
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  object@cdata@features_tracks[[phase]] <- base::as.data.frame(df)
  
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
#' @param wells Character vector. Denotes the wells for which the information 
#' is supposed to be set.
#' @param cell_line Character value, NA or NULL. The cell line seeded in the wells denoted in 
#' \code{selected_wells}.
#' @param condition Character value, NA, or NULL. The condition of the well. If multiple phase
#' experiment a character vector of length equal to the number of phases of
#' the experiment design.
#'
#' @details Sets well cell line and condition for all wells that are selected - 
#' meaning for which their value in variable \emph{selected} is TRUE. This 
#' can be manipulated with argument \code{wells} with which wells are selected.
#' Wells selected this way are deselected right before the data.frame is returned. 
#' 
#' @note \code{setWellInfo()} sets the provided information for all selected 
#' wells. If the input data.frame already contains selected wells these are 
#' included in addition to those denoted in argument \code{wells}.
#' 
#' @seealso \code{selectWells()}, \code{deselectWells()}
#'
#'
#' @return A data.frame.
#' @export

setWellInfo <- function(df, ...){
  
  UseMethod(generic = "setWellInfo", object = df)
  
}

#' @rdname setWellInfo
#' @export 
setWellInfo.layout_df <- function(df,
                                  wells = character(0),
                                  cell_line = NULL,
                                  condition = NULL,
                                  verbose = TRUE, 
                                  in_shiny = FALSE){
  
  df <- selectWells(df = df, wells = wells)
  
  df <- setCondition(df = df, condition = condition, in_shiny = in_shiny, verbose = verbose)
  
  df <- setCellLine(df = df, cell_line = cell_line, in_shiny = in_shiny, verbose = verbose)
  
  df <- setInfoStatus(df)
  
  df <- deselectWells(df = df, wells = wells)
  
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







