
#' @include S3-classes.R
NULL
#' @include S4-classes.R
NULL
#' @include S4-method-skeletons.R
NULL
#' @include imported-generics.R
NULL

# T -----------------------------------------------------------------------

#' @title Extract track data.frame
#' 
#' @description Obtain numeric cell features of time lapse experiments in 
#' form of a data.frame.
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame in which each observation refers to a cell at a given frame.
#' 
#' @export
#'

setGeneric(name = "getTracksDf", def = function(object, ...){
  
  standardGeneric(f = "getTracksDf")
  
})

#' @rdname getTracksDf
#' @export
setMethod(f = "getTracksDf", signature = "CdataTimeLapse", definition = function(object,
                                                                                 with_cluster = FALSE,
                                                                                 with_meta = FALSE, 
                                                                                 with_well_plate = FALSE,
                                                                                 with_everything = FALSE,
                                                                                 only_numeric = TRUE,
                                                                                 ...){
  
  df <-  tibble::as_tibble(object@features_tracks)
  
  if(base::isTRUE(only_numeric)){
    
    df <- dplyr::select(df, dplyr::any_of(non_data_track_variables), where(base::is.numeric))
    
  }
  
  df <- 
    joinWith(
      object = object, 
      df = df, 
      with_cluster = with_cluster, 
      with_meta = with_meta, 
      with_well_plate = with_well_plate,
      with_everything = with_everything,
      ...
    )
  
  return(df)
  
})

#' @rdname getTracksDf
#' @export
setMethod(f = "getTracksDf", signature = "CyproTimeLapse", definition = function(object,
                                                                                 with_cluster = FALSE,
                                                                                 with_meta = FALSE, 
                                                                                 with_well_plate = FALSE,
                                                                                 with_everything = FALSE,
                                                                                 only_numeric = TRUE,
                                                                                 ...){
  
  df <- 
    getCdata(object) %>% 
      getTracksDf(
        with_meta = with_meta, 
        with_cluster = with_cluster, 
        with_well_plate = with_well_plate,
        with_everything = with_everything,
        only_numeric = only_numeric
      )
  
  return(df)
  
})

#' @rdname getTracksDf
#' @export
setMethod(f = "getTracksDf", signature = "CyproTimeLapseMP", definition = function(object,
                                                                                   phase = 1,
                                                                                   with_cluster = FALSE,
                                                                                   with_meta = FALSE, 
                                                                                   with_well_plate = FALSE,
                                                                                   with_everything = FALSE,
                                                                                   only_numeric = TRUE,
                                                                                   ...){
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cdata <- getCdata(object)
  
  if(base::length(phase) > 1){
    
    df <- purrr::map_df(.x = phase, .f = ~ cdata@features_tracks[[.x]])
    
  } else {
  
    df <- cdata@features_tracks[[phase]]  
    
  }
  
  if(base::isTRUE(only_numeric)){
    
    df <- dplyr::select(df, dplyr::any_of(non_data_track_variables), where(base::is.numeric))
    
  }
  
  df <- 
    joinWith(
      object = cdata, 
      df = df, 
      with_cluster = with_cluster, 
      with_meta = with_meta, 
      with_well_plate = with_well_plate, 
      with_everything = with_everything
    )
  
  return(df)
  
})

#' @title Obtain numeric variables of cell track data 
#' 
#' @description Extracts the names of cell features of time lapse experiments.
#'
#' @inherit getStatVariableNames params
#' 
#' @return A character vector. 
#' 
#' @seealso starts_with(), ends_with(), contains(), matches()
#' 
#' @export
#' 

setGeneric(name = "getTrackVariableNames", def = function(object, ...){
  
  standardGeneric(f = "getTrackVariableNames")
  
})

#' @rdname getTrackVariableNames
#' @export
setMethod(
  f = "getTrackVariableNames",
  signature = "CyproTimeLapse",
  definition = function(object, only_numeric = TRUE, ...){
    
    check_object(object)
    
    res <- 
      getTracksDf(object, only_numeric = only_numeric)%>% 
      dplyr::select(-cell_id, -dplyr::any_of(vselect(non_data_track_variables, starts_with("frame")))) %>% 
      base::colnames() %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  })

#' @rdname getTrackVariableNames
#' @export
setMethod(
  f = "getTrackVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, phase = NULL, ...){
    
    check_object(object)
    
    assign_default(object)
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    res <- 
      getTracksDf(object, phase = phase, only_numeric = only_numeric)%>% 
      dplyr::select(-cell_id, -dplyr::any_of(vselect(non_data_track_variables, starts_with("frame")))) %>% 
      base::colnames() %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  }
)



# U -----------------------------------------------------------------------

# W -----------------------------------------------------------------------

#' @title Obtain well levels
#' 
#' @description Extracts a vector of well names in their natural order. 
#'
#' @inherit argument_dummy params 
#'
#' @return A character vector.
#' @export
#'
setGeneric(name = "getWellLevels", def = function(object){
  
  standardGeneric(f = "getWellLevels")
  
})

#' @rdname getWellLevels
#' @export
setMethod(f = "getWellLevels", signature = "WellPlate", definition = function(object){
  
  wp_df <- object@layout 
  
  return(wp_df$well)
  
})

#' @rdname getWellLevels
#' @export
setMethod(f = "getWellLevels", signature = "ExperimentDesign", function(object){
  
  base::stopifnot(nWellPlates(object) >= 1)
  
  well_plate_list <- object@well_plates
  
  biggest_wp <- 
    purrr::map_df(.x = well_plate_list, .f = function(well_plate){
      
      df <- 
        data.frame(
          well_plate = well_plate@name,
          n_wells = base::nrow(well_plate@layout)
        )
      
    }) %>% 
    dplyr::filter(n_wells == base::max(n_wells)) %>% 
    dplyr::slice_head(n = 1) %>% 
    dplyr::pull(well_plate) %>% 
    base::as.character()
  
  well_plate <- well_plate_list[[biggest_wp]]
  
  well_levels <- getWellLevels(well_plate)
  
  return(well_levels)
  
})

#' @rdname getWellLevels
#' @export
setMethod(f = "getWellLevels", signature = "Cypro", function(object){
  
  base::stopifnot(nWellPlates(object) >= 1)
  
  well_levels <- 
    getExperimentDesign(object) %>% 
    getWellLevels()
  
  return(well_levels)
  
})

#' @title Extract well plate
#' 
#' @description Obtain object of class \code{WellPlate}.
#'
#' @inherit argument_dummy params
#' 
#' @return A data.frame.
#' @export

setGeneric(name = "getWellPlate", def = function(object, well_plate = NULL){
  
  standardGeneric(f = "getWellPlate")
  
})


#' @rdname getWellPlate
#' @export
setMethod(f = "getWellPlate", signature = "ExperimentDesign", definition = function(object, well_plate = NULL){
  
  well_plate <- check_wp_name(object, well_plate)
  
  wp <- object@well_plates[[well_plate]]
  
  return(wp)
  
})

#' @rdname getWellPlate
#' @export
setMethod(f = "getWellPlate", signature = "Cypro", definition = function(object, well_plate = NULL){
  
  well_plate <- check_wp_name(object, well_plate)
  
  wp <- 
    getExperimentDesign(object) %>% 
    getWellPlate(well_plate = well_plate)
  
  return(wp)
  
})


#' @title Obtain list of WellPlate objects
#' 
#' @description Extracts a list of \code{WellPlate} objects.
#' 
#' @inherit argument_dummy params
#' 
#' @return A named list of \code{WellPlate} objects. 
#' 
#' @export

setGeneric(name = "getWellPlates", def = function(object, well_plates = NULL){
  
  standardGeneric(f = "getWellPlates")
  
})

#' @rdname getWellPlates
#' @export
setMethod(f = "getWellPlates", signature = "ExperimentDesign", definition = function(object, well_plates = NULL){
  
  well_plate_list <- object@well_plates
  
  if(base::length(well_plate_list) == 0){
    
    warning("Well plate list is empty.")
    
  }
  
  if(base::is.character(well_plates) & base::length(well_plate_list) >= 1){
    
    confuns::check_one_of(
      input = well_plates, 
      against = base::names(well_plate_list)
    )
    
    well_plate_list <- well_plate_list[well_plates]
    
  }
  
  return(well_plate_list)
  
})

#' @rdname getWellPlates
#' @export
setMethod(f = "getWellPlates", signature = "Cypro", definition = function(object, well_plates = NULL){
  
  exp_design <- getExperimentDesign(object)
  
  well_plate_list <- getWellPlates(object = exp_design, well_plates = well_plates)
  
  return(well_plate_list)
  
})


#' @title Extract cell well plate data (by cell id)
#' 
#' @description Obtain cell grouping variables according to the experiment 
#' design.
#' 
#' Not to be confused with the well plate layout which is stored in data.frames
#' of class \code{layout_df} that can be obtained via \code{getLayoutDf()}.
#'
#' @inherit argument_dummy params
#' 
#' @details \code{getWellPlateDf()} is the only function of those that 
#' start with \code{getWellPlate*()} that extracts its return value - the 
#' data.frame - from slot @@cdata of the \code{Cypro} object. 
#' 
#' @return A data.frame.
#' @export

setGeneric(name = "getWellPlateDf", def = function(object, ...){
  
  standardGeneric(f = "getWellPlateDf")
  
})


#' @rdname getWellPlateDf
#' @export
setMethod(f = "getWellPlateDf", signature = "Cdata", definition = function(object){
  
  tibble::as_tibble(object@well_plate)
  
})

#' @rdname getWellPlateDf
#' @export
setMethod(f = "getWellPlateDf", signature = "Cypro", definition = function(object){
  
  cdata_object <- getCdata(object)
  
  wp_df <- getWellPlateDf(object = cdata_object)
  
  return(wp_df)
  
})


#' @title Obtain well plate directories
#' 
#' @description Extracts the directories assigned to each well plate. 
#' 
#' @inherit argument_dummy params
#' 
#' @export

setGeneric(name = "getWellPlateDirectories", def = function(object, ...){
  
  standardGeneric(f = "getWellPlateDirectories")
  
})

#' @rdname getWellPlateDirectories
#' @export
setMethod(
  f = "getWellPlateDirectories",
  signature = "Cypro",
  definition = function(object, well_plates = NULL, named = TRUE){
  
  well_plate_list <- getWellPlates(object, well_plates = well_plates)
  
  wp_names <- base::names(well_plate_list)
  
  if(byFolder(object)){
    
    out <- 
      purrr::map(.x = well_plate_list, .f = function(x){
        
        dir <- 
          base::ifelse(
            test = isOfLength(x@directory, 0), 
            yes = NA_character_, 
            no = x@directory
          )
        
        return(dir)
        
      }) %>%
      purrr::flatten_chr()
    
    if(base::isTRUE(named)){
      
      out <- purrr::set_names(out, nm = wp_names)
      
    } else {
      
      out <- base::unname(out)
      
    }
    
  } else {
    
    out <- 
      purrr::map(.x = well_plate_list, .f = function(x){
        
        if(isOfLength(x@directory, l = 0)){
          
          dir <- NA_character_
          
        } else {
          
          dir <- x@directory
          
        }
        
        return(dir)
        
      }) 
    
    if(base::isTRUE(named)){
      
      out <- purrr::set_names(out, nm = wp_names)
      
    } else {
      
      out <- base::unname(out)
      
    }
    
  }
  
  return(out)
  
})


#' @title Extract well plate indices
#' 
#' @description Obtain the indices of well plates. Well plates are indexed
#' according to the order in which their layout is designed during the 
#' session in \code{designExperimen()}.
#'
#' @inherit argument_dummy params 
#' @param return Character value. Either \emph{'tibble'} or \emph{'vector'}.
#'
#' @return Either a tibble or a vector.
#' @export
#'

setGeneric(name = "getWellPlateIndices", def = function(object, ...){
  
  standardGeneric(f = "getWellPlateIndices")
  
})

#' @rdname getWellPlateIndices
#' @export
setMethod(f = "getWellPlateIndices", signature = "Cypro", definition = function(object, return = "tibble"){
  
  wps <- getWellPlates(object)
  
  wp_names <- base::names(wps)
  
  wp_indices <- 
    purrr::map_int(.x = wps, .f = ~ methods::slot(.x, "index") %>% base::as.integer())
  
  if(return == "tibble"){
    
    res <- 
      base::data.frame(
        well_plate_name = wp_names, 
        well_plate_index = wp_indices
      ) %>% 
      tibble::as_tibble()
    
  } else if(return == "vector"){
    
    res <- 
      purrr::set_names(wp_indices, wp_names)
    
  }
  
  return(res)
  
})


#' @title Extract well plate names
#' 
#' @description Obtain names of the well plate stored in the experiment design 
#' in form of a character vector.
#'
#' @inherit argument_dummy params
#'
#' @return A character vector. 
#' @export

setGeneric(name = "getWellPlateNames", def = function(object){
  
  standardGeneric(f = "getWellPlateNames")
  
})

#' @rdname getWellPlateNames
#' @export
setMethod(f = "getWellPlateNames", signature = "ExperimentDesign", def = function(object){
  
  return(base::names(object@well_plates))
  
})

#' @rdname getWellPlateNames
#' @export 
setMethod(f = "getWellPlateNames", signature = "Cypro", def = function(object){
  
  exp_design <- getExperimentDesign(object)
  
  wp_names <- getWellPlateNames(exp_design)
  
  return(wp_names)
  
})

#' @title Extract well plate variable names of cell data
#' 
#' @description Obtain names of variables that group cells according to 
#' the well plate layouts. 
#' 
#' @inherit getClusterVariableNames params
#' 
#' @return A character vector. 
#' 
#' @seealso starts_with(), ends_with(), contains(), matches()
#' 
#' @export

setGeneric(name = "getWellPlateVariableNames", def = function(object, ...){
  
  standardGeneric(f = "getWellPlateVariableNames")
  
})

#' @rdname getWellPlateVariableNames
#' @export
setMethod(f = "getWellPlateVariableNames", signature = "Cypro", definition = function(object, ...){
  
  check_object(object)
  
  wp_names <- 
    getWellPlateDf(object) %>% 
    dplyr::select(-cell_id) %>% 
    base::colnames() %>% 
    confuns::vselect(input = ., ...) 
  
  return(wp_names)
  
})



#' @title Obtain well plate type
#' 
#' @description Extracts the type of the well plate. 
#'
#' @inherit argument_dummy params 
#'
#' @return
#' @export
#'
setGeneric(name = "getWellPlateType", def = function(object, ...){
  
  standardGeneric(f = "getWellPlateType")
  
})

#' @rdname getWellPlateType
#' @export
setMethod(f = "getWellPlateType", signature = "WellPlate", function(object, ...){
  
  return(object@type)
  
})

#' @rdname getWellPlateType
#' @export
setMethod(f = "getWellPlateType", signature = "layout_df", function(object, ...){
  
  wp_type <- base::attr(x = object, which = "well_plate_type")
  
  return(wp_type)
  
})

#' @rdname getWellPlateType
#' @export
setMethod(f = "getWellPlateType", signature = "layout_df_mp", function(object, ...){
  
  wp_type <- base::attr(x = object, which = "well_plate_type")
  
  return(wp_type)
  
})

#' @title Obtain wells and well rois
#' 
#' @description Extracts unique well or well regions of interest of a well plate 
#' in form of a vector. 
#'
#' @inherit argument_dummy params 
#' 
#' @details In all methods, data is eventually extracted from the layout data.frames.
#'
#' @return A character vector.
#' @export
#'

setGeneric(name = "getWells", def = function(object,  info_status = info_status_levels){
  
  standardGeneric(f = "getWells")
  
})

#' @rdname getWells
#' @export
setMethod(f = "getWells", signature = "layout_df", definition = function(object, info_status = info_status_levels){
  
  nestLayoutDf(object) %>% 
    dplyr::filter(info_status %in% {{info_status}}) %>% 
    dplyr::pull(var = "well") %>% 
    base::as.character()
  
})

#' @rdname getWells
#' @export
setMethod(f = "getWells", signature = "layout_df_mp", definition = function(object, info_status = info_status_levels){
  
  nestLayoutDf(object) %>% 
    dplyr::filter(info_status %in% {{info_status}}) %>% 
    dplyr::pull(var = "well") %>% 
    base::as.character()
  
})

#' @rdname getWells
#' @export
setMethod(f = "getWells", signature = "WellPlate", definition = function(object, info_status = info_status_level){
  
  getLayoutDf(object) %>% 
    getWells() %>% 
    base::as.character()
  
})

#' @rdname getWells
#' @export
setGeneric(name = "getWellRois", def = function(object, info_status = info_status_levels){
  
  standardGeneric(f = "getWellRois")
  
})

#' @rdname getWells
#' @export
setMethod(f = "getWellRois", signature = "layout_df", definition = function(object, info_status = info_status_levels){
  
  unnestLayoutDf(object) %>% 
    dplyr::filter(info_status %in% {{info_status}}) %>% 
    dplyr::pull(var = "well_roi") %>% 
    base::as.character()
  
})

#' @rdname getWells
#' @export
setMethod(f = "getWellRois", signature = "layout_df_mp", definition = function(object, info_status = info_status_levels){
  
  unnestLayoutDf(object) %>% 
    dplyr::filter(info_status %in% {{info_status}}) %>% 
    dplyr::pull(var = "well_roi") %>% 
    base::as.character()
  
})

#' @rdname getWells
#' @export
setMethod(f = "getWellRois", signature = "WellPlate", definition = function(object, info_status = info_status_level){
  
  getLayoutDf(object) %>% 
    getWellRois() 
    
})



# V -----------------------------------------------------------------------

#' @title Obtain variable assignment
#' 
#' @description Extracts the variable names of the experiment data that has been
#' assigned to the data variables that \code{cypro} knows. 
#'
#' @inherit argument_dummy params
#' @param required,optional,computable Logical values. Indicate if the respective 
#' variable type should be included or not. 
#' @param variables,modules Character vector or NULL. If character vector only 
#' the variables and modules mentioned are included.
#' @param drop_na Logical. If set to TRUE all empty values are discarded. 
#' @param flatten Logical. If set to TRUE the list is flattened to a character vector 
#' via \code{purrr::flatten_chr()}.
#' 
#' @return A named list or a named character vector. 
#' @export

setGeneric(name = "getVariableAssignment", def = function(object, ...){
  
  standardGeneric(f = "getVariableAssignment")
  
})


#' @rdname getVariableAssignment
#' @export
setMethod(
  f = "getVariableAssignment", 
  signature = "AnalysisModule",
  definition = function(object,
                        variables = NULL, 
                        required = TRUE,
                        optional = TRUE,
                        computable = TRUE){
    
    all_var_assignments <- base::vector(mode = "character")
    
    module <- object
    
    all_var_assignments <- 
      get_variable_assignment_hlpr(
        variable_list = module@variables_required,
        check = required, 
        variables = variables, 
        assignments = all_var_assignments
      )
    
    all_var_assignments <- 
      get_variable_assignment_hlpr(
        variable_list = module@variables_optional, 
        check = optional, 
        variables = variables, 
        assignments = all_var_assignments
      )
    
    all_var_assignments <-
      get_variable_assignment_hlpr(
        variable_list = module@variables_computable,
        check = computable, 
        variables = variables, 
        assignments = all_var_assignments
      )
    
    return(all_var_assignments)
    
  })

#' @rdname getVariableAssignment
#' @export
setMethod(
  f = "getVariableAssignment", 
  signature = "Cypro", 
  definition = function(object, 
                        required = TRUE, 
                        optional = TRUE,
                        computable = TRUE,
                        modules = NULL, 
                        variables = NULL, 
                        drop_na = FALSE, 
                        flatten = FALSE,
                        reverse = FALSE){
    
    module_names <- base::names(object@modules)
    
    assignments <- 
      base::vector(mode = "list",length = base::length(module_names)) %>% 
      purrr::set_names(nm = module_names)
    
    for(module_name in module_names){
      
      if(base::is.null(modules) | module_name %in% modules){
        
        module <- object@modules[[module_name]]
        
        if(base::isTRUE(module@active)){
          
          assignments[[module_name]] <- 
            getVariableAssignment(
              object = module, 
              required = required, 
              optional = optional,
              computable = computable, 
              variables = variables
            )
          
        }
        
      }
      
    }
    
    if(base::isTRUE(drop_na)){
      
      assignments <- 
        purrr::keep(.x = assignments, .p = ~ shiny::isTruthy(.x)) %>% 
        purrr::map(.x = ., .f = ~ purrr::keep(.x = .x, .p = ~ shiny::isTruthy(x = .x)))
      
    }
    
    if(base::isTRUE(flatten) | base::is.character(variables)){
      
      assignments <- purrr::flatten_chr(.x = assignments)
      
    }
    
    return(assignments)
    
  }
)

get_variable_assignment_hlpr <- function(variable_list, variables, assignments, check){
  
  if(base::isTRUE(check)){
    
    for(i in base::seq_along(variable_list)){
      
      vr <- variable_list[[i]]
      
      var_name <- vr@name_in_cypro
      
      if(base::is.null(variables) | var_name %in% variables){
        
        var_assignment <- purrr::set_names(x = vr@name_in_example, nm = var_name)
        
        assignments <- c(assignments, var_assignment)
        
      }
      
    }
    
  }
  
  return(assignments)
  
}


#' @title Obtain variable assignment 
#' 
#' @inherit getVariableAssignment params description
#' 
#' @return A character vector. 
#' 
#' @export
setGeneric(name = "getVariableAssignmentID", def = function(object, drop_na = FALSE){
  
  standardGeneric(f = "getVariableAssignmentID")
  
})


#' @rdname getVariableAssignmentID
#' @export
setMethod(
  f = "getVariableAssignmentID", 
  signature = "CyproScreening", 
  definition = function(object, drop_na = FALSE){
    
    res <- 
      getVariableAssignment(
        object = object,
        modules = "identification",
        flatten = TRUE,
        drop_na = drop_na
      )
    
    return(res)
    
  }
)


#' @rdname getVariableAssignmentID
#' @export
setMethod(
  f = "getVariableAssignmentID", 
  signature = "CyproTimeLapse", 
  definition = function(object, drop_na = FALSE){
    
    res <- 
      getVariableAssignment(
        object = object,
        modules = "identification_timelapse",
        flatten = TRUE,
        drop_na = drop_na
      )
    
    return(res)
    
  }
)

#' @rdname getVariableAssignmentID
#' @export
setMethod(
  f = "getVariableAssignmentID", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, drop_na = FALSE){
    
    res <- 
      getVariableAssignment(
        object = object,
        modules = "identification_timelapse",
        flatten = TRUE,
        drop_na = drop_na
      )
    
    return(res)
    
  }
)