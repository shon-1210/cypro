

#' @title Check content availability 
#'
#' @param evaluate The test that must be TRUE. 
#' @inherit argument_dummy params
#' @param ref_input glue reference
#' @param ref_fun glue reference
#'
check_availability <- function(evaluate, phase, ref_input, ref_fun){
  
  ce <- rlang::caller_env()
  
  object <- base::parse(text = "object") %>% base::eval(envir = ce)
  
  if(!base::isTRUE(evaluate)){
    
    msg <- glue::glue("Could not find {ref_input}{ref_phase}. You might have to use function '{ref_fun}' first.",
                      ref_phase = hlpr_glue_phase(object, phase, FALSE))
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  
}

#' @title Check object input
#' 
#' @description Makes sure that object input is of class cypro and 
#' that it contains the relevant information needed for the function.
#' @param object A valid cypro object. 
check_object <- function(object, experiment = NULL, set_up_req = "process_data", exp_type_req = NULL, module_req = NULL){
  
  input_class <- base::class(object)
  input_attribute <- base::attr(input_class, "package")
  
  if(FALSE){
    
    msg <- "Input for argument 'object' must be of class 'cypro' from package 'cypro'."
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  if(FALSE){
    
    missing_fun <- set_up_funs[[set_up_req]]
    
    msg <- glue::glue("Seems like you've missed some object processing steps. Make sure to run '{missing_fun}' first and then try again.")
    
    base::stop(msg)
    
  }
  
  if(FALSE){
    
    if(exp_type_req == "time_lapse"){
      
      if(!isTimeLapseExp(object)){
        
        msg <- "This function requires a time lapse experiment set up."
        
        base::stop(msg)
        
      }
      
    }
    
    if(exp_type_req == "one_time_imaging"){
      
      if(!object@set_up$experiment_type != "one_time_imaging"){
        
        msg <- "This function requires a one time imaging experiment set up."
        
        base::stop(msg)
        
      }
      
      
    }
    
  }
  
  
  if(FALSE){
    
    if(!isUsable(object, module = module_req)){
      
      msg <- glue::glue("This functions requires specific module usability. Module: '{module_req}'")
      
      base::stop(msg)
      
    }
    
  }
  
  base::invisible(TRUE)
  
}


#' @title Check phase input 
#' 
#' @inherit check_object params
#' @param phase Character or numeric. If character, the ordinal value referring to the phase of interest (e.g. \emph{'first'}, \emph{'second'} etc.).
#' referring to the phase of interest or \emph{'all'}. If numeric, the number referring to the phase. 
#' 
#' If set to NULL takes the phase denoted as default with \code{adjustDefault()}.
#' 
#' Ignored if the experiment design contains only one phase. 
#' 
#' @param max_phase Numeric value or NULL. If numeric it regulates the maximal number of phases allowed. 
#' 
check_phase <- function(object, phase, max_phases = NULL){
  
  if(base::is.null(phase)){
    
    phase <- 1
    
  }
  
  phase_names <- getPhaseNames(object)
    
    if(base::is.numeric(phase)){
      
      lp <- base::length(phase_names)
      
      is_vec(phase, mode = "numeric", max.length = lp)
      
      if(base::max(phase) > lp){
        
        stop(glue::glue("Input for argument 'phase' must not exceed the number of phases of the experiment which is {lp}."))
        
      }
      
      phase <- phase_names[phase]
      
    }
    
    
    if(base::is.numeric(max_phases)){
      
      if(max_phases == 1){
        
        is_vec(x = phase, mode = "character", of.length = max_phases)
        
      } else {
        
        is_vec(x = phase, mode = "character", max.length = max_phases)
        
      }
      
    }
    
    if(base::all(phase == "all") & base::is.null(max_phases)){
      
      phase <- getPhaseNames(object)
      
    } else if(base::is.numeric(max_phases) && base::length(phase) > max_phases){
      
      msg <- glue::glue("Length of input for argument phase must be equal to or lower than {max_phases}.")
      
      give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
      
    } else if(base::all(phase == "all") && base::is.numeric(max_phases) && base::length(getPhases(object) > max_phases)){
      
      msg <- glue::glue("Length of input for argument phase must be equal to or lower than {max_phases}. All phases sum up to {base::length(getPhases(object))}.")
      
      give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
      
    } else {
      
      check_one_of(
        input = phase, 
        against = getPhaseNames(object)
      )
      
    }
  
  return(phase)
  
}


#' @title Check that phase is specified manually
check_phase_manually <- function(object, phase = NULL){
  
  if(multiplePhases(object)){
    
    if(base::is.null(phase)){
      
      base::stop("Creating a subset of your data impacts downstream analysis. Please specify argument 'phase' manually to ensure that you are referring to the right one.")
      
    }
    
  }
  
}

#' @title Detect missing variables
#' 
#' @description Returns \code{variable} if it was not detected in the column names 
#' of the specified data.frame. To be used in \code{purrr::map()} with argument \code{.x}
#' set to a vector of all used variable names.
#' @param variable_name Character value. The name of the variable to be checked.
#' @param df A read in cell track data.frame.
#'

check_df_variables <- function(variable_name, df){
  
  cnames <- base::colnames(df)
  
  if(variable_name %in% cnames){
    
    NULL
    
  } else if(base::any(stringr::str_detect(cnames, pattern = variable_name))){
    
    NULL
    
  } else {
    
    base::return(variable_name)
    
  }
  
}

find_missing_variables_shiny <- check_df_variables


#' @title Detect poorly added variables 
#' 
#' @description Makes sure that the data.frame that is added to the cypro object
#' after a call to add*Variables() contains as many rows as there are cells
#' in the cypro object. 
#' 
check_nrow <- function(df, n_rows, ref){
  
  df_n_rows <- base::nrow(df)
  
  if(df_n_rows != n_rows){
    
    msg <- 
      glue::glue(
        "Number of rows of new {ref} data.frame ({df_n_rows}) does not correspond to number",
        "of cells in cypro-object ({n_rows}). Attempting to fix with 'dplyr::distinct()'."
      )
    
    confuns::give_feedback(msg = msg, verbose = TRUE)
    
    df_new <- dplyr::distinct(.data = df)
    
    df_new_n_rows <- base::nrow(df_new)
    
    if(df_new_n_rows != n_rows){
      
      msg <- 
        glue::glue(
          "Number of rows of new {ref} data.frame ({df_new_n_rows}) still does not correspond to number",
          "of cells in cypro-object ({n_rows}). Please adjust argument in put of the the add*()-function",
          "that you used."
        )
      
      confuns::give_feedback(msg = msg, fdb.fn = "stop")
      
    } else {
      
      confuns::give_feedback(msg = glue::glue("Number of rows fit. Adding {ref} data.frame to cypro-object."), verbose = TRUE)
      
      df <- df_new
      
    }
    
  }
  
  return(df)
  
}

#' @title Detect protected variables
#' @description Makes sure that renaming variables does not conflict with 
#' protected variable names,
check_renamed_variables <- function(cnames){
  
  found_pos <- base::which(protected_vars %in% cnames)
  
  found_vars <- protected_vars[found_pos]
  
  if(base::length(found_vars) >= 1){
    
    msg <- 
      glue::glue(
        "'{ref_found_vars} {ref1} invalid new {ref2}.", 
        ref_found_vars = glue::glue_collapse(x = found_vars, sep = "', '", last = "' and '"), 
        ref1 = confuns::adapt_reference(found_vars, "is an", "are"), 
        ref2 = confuns::adapt_reference(found_vars, "name", "names")
      )
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  } 
  
  
}


#' @title Input check of summarize_with
check_summarize_with <- function(summarize_with, max_length = 1){
  
  confuns::is_vec(x = summarize_with, mode = "character", max.length = max_length)
  
  confuns::check_one_of(
    input = summarize_with, 
    against = base::names(stat_funs)
  )
  
  
}


#' @title Check track data.frame 
#'
#' @param track_df A data.frame in which each observation refers to a cell at a given frame and 
#' that contains the following variables: 
#' 
#'  \describe{
#'   \item{\emph{cell_id}}{Character. Refers to the cell id's.}
#'   \item{\emph{x_coords}}{Numeric. Refers to the cell's x-coordinates.}
#'   \item{\emph{y_coords}}{Numeric. Refers to the cell's y-coordinates.}
#'   \item{\emph{dfo}}{Numeric. The distances from the first position tracked. (Distance from origin).}
#'   \item{\emph{dfpl}}{Numeric. The distances from the last point.}
#'   \item{\emph{speed}}{Numeric. Refers to the instantaneuous speed.}
#'   \item{\emph{afo}}{Numeric. The angle from the position of origin.}
#'   \item{\emph{aflp}}{Numeric. The angle from the last position.}
#'   \item{\emph{frame_num}}{Numeric. The frame number the observation refers to.}
#'   \item{\emph{frame_time}}{Numeric. The frame number multiplied with the interval between two frames}
#'   \item{\emph{frame_itvl}}{Character. The frame time combined with the intervals unit}
#'   }
#'
#' @return
#' @export
#'

check_track_df <- function(track_df){}



#' @title Detect double directories
#'
#' @param well_plate_list A list of well plate lists.
#'
#' @return A character value that contains either \emph{'unique'} or the 
#' directories that are not unique. 

check_wp_directories <- function(well_plate_list){
  
  well_plate_list <- 
    purrr::discard(.x = well_plate_list, 
                   .p = ~ base::is.null(.x[["directory"]]))
  
  count_dirs <- 
    purrr::map_chr(.x = well_plate_list, "directory") %>% 
    base::table() %>% 
    base::as.data.frame() %>% 
    magrittr::set_colnames(value = c("dir", "count"))
  
  if(base::any(count_dirs$count != 1)){
    
    return_value <- 
      dplyr::filter(.data = count_dirs, count != 1) %>% 
      dplyr::pull(var = "dir") %>% 
      stringr::str_c(collapse = "', '")
    
  } else {
    
    return_value <- "unique"
    
  }
  
  base::return(return_value)
  
}


#' @title Check well plate name
#' 
#' @inherit check_object params
#' @param well_plate Character value. The name of the well plate of interest. Valid inputs can be obtained 
#' via \code{getWellPlateNames()}.
#' 
check_wp_name <- function(object, well_plate){
  
  wp_names <- getWellPlateNames(object)
  
  if(base::all(well_plate == "")){
    
    well_plate <- wp_names[1]
    
    if(base::length(wp_names) > 1){
      
      base::message(glue::glue("Defaulting to first well plate found: '{wp_names[1]}'."))
      
    }
    
  } else {
    
    well_plate <- 
      confuns::check_vector(
        input = well_plate, 
        against = wp_names, 
        verbose = TRUE, 
        ref.input = "input for argument 'well_plate'",
        ref.against = "valid well plate names")
    
  }
  
  return(well_plate)
  
}








# a -----------------------------------------------------------------------




# f -----------------------------------------------------------------------







# m -----------------------------------------------------------------------

check_module_cell_id_frame_num <- function(df, object){
  
  if(FALSE){ # isActive(object, module_name = "mitosis")
    
    out <- NULL
    
  } else {
    
    name_in_example_cell_id <-
      getVariableAssignment(object, variables = "cell_id") %>%
      base::unname()
    
    name_in_example_frame_num <-
      getVariableAssignment(object, variables = "frame_num") %>%
      base::unname()
    
    name_in_app <- "Frame number' & 'Cell ID"
    
    if(name_in_example_cell_id %in% base::colnames(df)){
      
      identifier_df <- 
        dplyr::select(df, dplyr::all_of(c(name_in_example_frame_num, name_in_example_cell_id)))
      
      n_obs <- base::nrow(identifier_df)
      
      n_unique_obs <- 
        dplyr::distinct(identifier_df) %>% 
        base::nrow()
      
      if(n_obs > n_unique_obs){
        
        id_vars <-
          getVariableAssignment(
            object = object,
            modules = "identification_timelapse", 
            flatten = TRUE, 
            drop_na = TRUE
            ) %>% 
          confuns::scollapse()
        
        out <- 
          glue::glue(
            "Identification variables '{id_vars}' do not ", 
            "uniquely identify each observation."
          ) %>% 
          write_variable_problem(name_in_app = name_in_app)
        
      } else {
        
        out <- NULL
        
      } 
      
    } else {
      
      out <- 
        glue::glue(
          "Can not validate content of Frame-variable '{name_in_example_frame_num}' ",
          "as Cell ID-variable '{name_in_example_cell_id}' is missing.") %>% 
        write_variable_problem(name_in_app = name_in_app)
      
    }
    
  }
  
  return(out)
  
}

# v -----------------------------------------------------------------------



#' @title Variable specific content checks
#' 
#' @description These functions make sure that the content of the data.frame
#' variables assigned to variables known by \code{cypro}. See details for more.
#' 
#' @param df The input data.frame. 
#' @param name_in_cypro Character value. The name of the data variable 
#' as \code{cypro} knows it.
#' @param object The \code{Cypro} object.
#' 
#' @details These functions are added to the @@slot check_content of each 
#' \code{DataVariable} object. They are called mainly during \code{validateInputDf()}.
#' See it's documentation for more information of how it works. 
#' 
#' Every \code{check_var_*()} function must take the same arguments: \code{df}, 
#' \code{name_in_cypro} and \code{object} in that order. It must do the following: 
#' 
#' 1. Extract the assigned variable from the data.frame. 
#' 2. Make sure that the variable fits the basic requirements regarding class / type. If it 
#' does not fit the class and type requirements but is convertible the function converts it.
#' 3. Beyond class/type requirements the function checks the content for variable required
#' specifics. E.g. the content of \code{var_well} can not be a collection of any strings but has to 
#' meet the proper well naming conventions. 
#' 
#' Output: The output is one of the following two options: 
#' 
#' a) If the variable passed everything (or could be adjusted to be valid) and is valid 
#' the \code{check_var_()*}-function only returns the variable valid variable (not the data.frame.)
#' b) If anything did not work out or the output variable is invalid the function
#' returns an informative message about what went wrong in form of a character value. 

check_var_angle <- function(df, name_in_cypro, object = NULL){
  
  name_in_example <- 
    getVariableAssignment(object, variables = name_in_cypro) %>% 
    base::unname()
  
  if(base::is.na(name_in_example)){
    
    out <- NULL
    
  } else {
    
    name_in_app <- cypro_variable_names[[name_in_cypro]]
    
    var <- df[[name_in_example]]
    
    check_res <- check_and_convert_numeric_var(var = var, ref = name_in_example)
    
    if(base::is.null(check_res$problem)){
      
      var <- check_res$var
      
      min_var <- base::min(var, na.rm = TRUE)
      
      max_var <- base::max(var, na.rm = TRUE)
      
      if(min_var < 0 | max_var > 360){
        
        out <- 
          glue::glue("Values of {name_in_app}-variable '{name_in_example}' do not range from 0 to 360.") %>% 
          write_variable_problem(name_in_app = name_in_app)
        
      } else {
        
        out <- var
        
      }
      
    } else {
      
      out <- write_variable_problem(problem = check_res$problem, name_in_app = name_in_app)
      
    }
    
  }
  
  return(out)
  
}


#' @rdname check_var_angle
#' @section Function details:
#' Constructs and outputs a preliminary cell id from all assigned optional identifier
#' variables to make sure that the variables identify cells uniquely across all regions
#' of interest if the example data.frame loaded contains data from more than one ROI. 
check_var_cell_id <- function(df, name_in_cypro, object = NULL){
  
  # (must not be NA therefore no if else)
  
  name_in_example <- 
    getVariableAssignment(object, variables = "cell_id") %>%
    base::unname()
  
  name_in_app <- cypro_module_names[[name_in_cypro]]
  
  var <- df[[name_in_example]]
  
  check_res <- check_and_convert_grouping_var(var = var, ref = name_in_example)
  
  if(base::is.null(check_res$problem)){
    
    layout_vars <- 
      getVariableAssignment(
        object = object,
        variables = c("well_plate", "well", "roi", "well_roi")
        )
      
    if(!base::any(base::is.na(layout_vars[c("well", "roi")]))){
      
      layout_vars <- layout_vars[layout_vars != layout_vars["well_roi"]]
      
    } else if(!base::is.na(layout_vars["well_roi"])){
      
      layout_vars <- layout_vars[layout_vars %in% layout_vars[c("well", "roi")]]
      
    }
    
    layout_vars <- purrr::discard(.x = layout_vars, .p = base::is.na)
    
    id_vars <- base::unname(c(name_in_example, layout_vars))
    
    out <- 
      dplyr::select(.data = df, dplyr::all_of(x = id_vars)) %>% 
      tidyr::unite(col = "final_id", dplyr::all_of(id_vars), sep = "_") %>% 
      dplyr::pull(name = "final_id")
    
  } else {
    
    out <- write_variable_problem(problem = check_res$problem, name_in_app = name_in_app)
    
  }
  
  return(out)
  
}

check_var_character <- function(df, name_in_cypro, object = NULL){
  
  name_in_example <- getVariableAssignment(object, variables = name_in_cypro)
  
  if(base::is.na(name_in_example)){
    
    out <- NULL
    
  } else {
    
    name_in_app <- cypro_variable_names[[name_in_cypro]]
    
    var <- df[[name_in_example]]
    
    check_res <- check_and_convert_grouping_var(var = var, ref = name_in_example)
    
    if(base::is.null(check_res$problem)){
      
      out <- check_res$var
      
    } else {
      
      out <- write_variable_problem(problem = check_res$problem, name_in_app = name_in_app)
      
    }
    
  }
  
  return(out)
  
}

check_var_frame_num <- function(df, name_in_cypro, object = NULL){
  
  # (must not be NA therefore no if else)
  
  name_in_example <- 
    getVariableAssignment(object, variables = "frame_num") %>%
    base::unname()
  
  var <- df[[name_in_example]]
  
  check_res <- check_and_convert_numeric_var(var = var, ref = name_in_example)
  
  if(base::is.null(check_res$problem)){
  
    var <- check_res$var
    
    min_var <- base::min(var, na.rm = TRUE)
    
    if(min_var == 0){
      
      out <- var + 1
      
      out <- base::as.integer(out)
      
    } else if(min_var == 1){
      
      out <- base::as.integer(var)
      
    } else {
      
      out <- 
        glue::glue("Values of 'Frame number'-variable '{name_in_example}' must have their minimum at 1 (or 0) not at {min_var}.") %>% 
        write_variable_problem(name_in_app = "Frame number")
      
    } 
    
  } else {
    
    out <- write_variable_problem(problem = check_res$problem, name_in_app = "Frame number")
    
  }
  
  return(out)
  
}

check_var_numeric <- function(df, name_in_cypro, object = NULL){
  
  name_in_example <- getVariableAssignment(object, variables = name_in_cypro) %>% base::unname()
  
  if(base::is.na(name_in_example)){
    
    out <- NULL
    
  } else {
    
    name_in_app <- cypro_variable_names[[name_in_cypro]]
    
    var <- df[[name_in_example]]
    
    check_res <- check_and_convert_numeric_var(var = var, ref = name_in_example)
    
    if(base::is.null(check_res$problem)){
      
      out <- check_res$var
      
    } else {
      
      out <- write_variable_problem(problem = check_res$problem, name_in_app = name_in_app)
      
    }
    
  }
  
  return(out)
  
}

check_var_roi <- function(df, name_in_cypro, object = NULL){
  
  name_in_example <- 
    getVariableAssignment(object, variables = name_in_cypro) %>%
    base::unname()
  
  if(base::is.na(name_in_example)){
    
    out <- NULL
    
  } else {
    
    name_in_app <- cypro_variable_names[[name_in_cypro]]
    
    var <- df[[name_in_example]]
    
    check_res <- check_and_convert_grouping_var(var = var, ref = name_in_example)
    
    if(base::is.null(check_res$problem)){
      
      var <- check_res$var
      
      var <- 
        extractRoiInfo(var) %>% 
        adjustRoiInfo()
      
      all_valid <- validateRois(var) %>% base::all()
      
      if(base::isTRUE(all_valid)){
        
        out <- var
        
      } else {
        
        out <- 
          glue::glue("Attempt to extract ROI information resulted in NAs.") %>% 
          write_variable_problem(name_in_app = name_in_app)
        
      }
      
    } else {
      
      out <- write_variable_problem(problem = check_res$problem, name_in_app = name_in_app)
      
    }
    
  }
  
  return(out)
  
}

check_var_well <- function(df, name_in_cypro, object = NULL){
  
  name_in_example <-
    getVariableAssignment(object, variables = name_in_cypro) %>%
    base::unname()
  
  if(base::is.na(name_in_example)){
    
    out <- NULL
    
  } else {
    
    name_in_app <- cypro_variable_names[[name_in_cypro]]
    
    var <- df[[name_in_example]]
    
    check_res <- check_and_convert_grouping_var(var = var, ref = name_in_example)
    
    if(base::is.null(check_res$problem)){
      
      var <- check_res$var
      
      var <- 
        extractWellInfo(var) %>% 
        adjustWellInfo()
      
      all_valid <- validateWells(var) %>% base::all()
      
      if(base::isTRUE(all_valid)){
        
        out <- var
        
      } else {
        
        out <- 
          glue::glue("Attempt to extract well information resulted in NAs.") %>% 
          write_variable_problem(name_in_app = name_in_app)
        
      }
      
    } else {
      
      out <- write_variable_problem(problem = check_res$problem, name_in_app = name_in_app)
      
    }
    
  }
  
  return(out)
  
}

check_var_well_roi <- function(df, name_in_cypro, object = NULL){
  
  var_assignment <- 
    getVariableAssignment(
      object = object,
      variables = c("cell_id", "well", "roi", "well_roi")
    )
  
  name_in_example <- 
    getVariableAssignment(object, variables = name_in_cypro) %>%
    base::unname()
  
  if(base::all(base::is.na(var_assignment[c("well", "roi", "well_roi")]))){
    
    out <- NULL
    
  } else if(base::any(base::is.na(var_assignment[c("well", "roi")]))){
      
      if(base::is.na(name_in_example)){
        
        out <- NULL
        
      } else {
        
        name_in_app <- cypro_variable_names[[name_in_cypro]]
        
        var <- df[[name_in_example]]
        
        check_res <- check_and_convert_grouping_var(var = var, ref = name_in_example)
        
        if(base::is.null(check_res$proble)){
          
          var <- check_res$var
          
          var <- 
            extractWellRoiInfo(var) %>% 
            adjustWellRoiInfo()
          
          all_valid <- validateWellRois(var) %>% base::all()
          
          if(base::isTRUE(all_valid)){
            
            out <- var
            
          } else {
            
            out <- 
              glue::glue("Attempt to extract well-roi information resulted in NAs.") %>% 
              write_variable_problem(name_in_app = name_in_app)
            
          }
          
        } else {
          
          out <- write_variable_problem(problem = check_res$problem, name_in_app = name_in_app)
          
        }
        
      }
      
    } else {
      
      # single variables 'well' and 'roi' hold priority over the combined var
      well_in_example <- var_assignment["well"]
      roi_in_example <- var_assignment["roi"]
      
      # the data.frame df contains already checked variables 
      # well and roi, if the checking resulted in errors the 
      # output of this function isn't going to be used anyway
      well_var <- df[[well_in_example]]
      roi_var <- df[[roi_in_example]]
      
      out <- stringr::str_c(well_var, roi_var, sep = "_")
      
    }
  
  return(out)
  
}

check_var_well_plate <- function(df, name_in_cypro, object = NULL){
  
  well_plate_names <- getWellPlateNames(object)
  
  if(nWellPlates(object) == 1){
    
    out <- base::rep(well_plate_names, base::nrow(df))
    
  } else {
    
    name_in_example <- getVariableAssignment(object, variables = name_in_cypro) %>% base::unname()
    
    if(base::is.na(name_in_example)){
      
      out <- NULL
      
    } else {
      
      name_in_app <- cypro_variable_names[[name_in_cypro]]
      
      var <- df[[name_in_example]]
      
      check_res <- check_and_convert_grouping_var(var = var, ref = name_in_example)
      
      if(base::is.null(check_res$problem)){
        
        var <- check_res$var
        
        var_well_plate_names <- base::unique(var) %>% base::sort()
        
        unknown_well_plates <- var_well_plate_names[!var_well_plate_names %in% well_plate_names]
        
        if(base::length(unknown_well_plates) >= 1){
          
          ref <- confuns::scollapse(unknown_well_plates)
          
          out <- 
            glue::glue("Unkown well plates: '{ref}'") %>% 
            write_variable_problem(name_in_app = name_in_app)
          
        } else {
          
          out <- var
          
        }
        
      } else {
        
        out <- write_variable_problem(problem = check_res$problem, name_in_app)
        
      }
      
    }
    
  }
  
  return(out)
  
}























