




# exported ----------------------------------------------------------------


# I -----------------------------------------------------------------------
#' @title Validate input data.frame content
#' 
#' @description Validates that the validity of an input data.frame. See details 
#' for more. 
#'
#' @inherit argument_dummy params 
#' @param df A data.frame.
#' 
#' @details A data.frame that is about to be loaded and added to the data of 
#' the \code{Cypro} object must meet certain requirements. 
#' 
#' First, during \code{assignVariables()} the variable names of the future data input 
#' have been assigned to data variables that \code{cypro} knows, such as \emph{x_coords}, 
#' \emph{cell_id}, \emph{frame_num} etc. \code{validateInputDf()} checks if all 
#' these assigned variable names exist in the input data.frame. 
#' 
#' Secondly, if all variable names exist \code{validateInputDf()} checks if 
#' each  variable of the input data.frame meets all the requirements of 
#' the data variable it has been assigned to by using the function that is stored
#' in slot @@check_content of the S4-class \code{AssignableVariable} by which 
#' every assignable data variable \code{cypro} knows is represented. This can range from simple type testing via 
#' \code{is.numeric()} to more sophisticated checks. E.g the variable \emph{Angle from last point (aflp)}
#' of the migration analysis can only contain values ranging between 0 and 360.
#' 
#' Variable names that are stored under additional variables in the \code{Cypro} object are looked 
#' up if argument \code{check_additional} is set to TRUE. If an additional variable is not part of the input data.frame
#' the output list will contain a slot named after it with a corresponding message.
#'
#' @return A list named according to each variable found in the input data.frame provided
#' with argument \code{df}.
#' 
#' Slots of the list that are occupied by \code{AssignableVariable}s are named according to their
#' \code{name_in_cypro}. If a variable has met all requirements it is stored in the returned list
#' as it was returned by it's @@check_content-function. If not, the corresponding error message
#' occupies the variables slot in the output list.
#' 
#' Slots of the list that are occupied by additional variables are named according to their 
#' name in the input data.frame.
#' 
#' @export

setGeneric(name = "validateInputDf", def = function(object, df, ...){
  
  standardGeneric(f = "validateInputDf")
  
})

#' @rdname validateInputDf
#' @export
setMethod(
  f = "validateInputDf",
  signature = "Cypro",
  definition = function(object, df, check_vars = TRUE, check_additional = TRUE){
  
  used_modules <- object@modules[getActiveModuleNames(object)]
  
  out <- list()
  
  # module specific vars
  if(base::isTRUE(check_vars)){
    
    for(m in base::seq_along(used_modules)){
      
      module <- used_modules[[m]]
      
      # optional
      res <- 
        validate_variables_hlpr(
          variables = module@variables_optional,
          df = df,
          out_list = out,
          object = object
        )
      
      df <- res$df
      out <- res$out_list
      
      # required
      res <- 
        validate_variables_hlpr(
          variables = module@variables_required,
          df = df,
          out_list = out,
          object = object
        )
      
      df <- res$df
      out <- res$out_list
      
      # computable
      res <- 
        validate_variables_hlpr(
          variables = module@variables_computable, 
          df = df, 
          out_list = out,
          object = object
        )
      
      df <- res$df 
      out <- res$out_list
      
      # additional variable spanning checks
      add_out <- list()
      
      for(f in base::seq_along(module@check_fns)){
        
        check_fn_name <-
          base::names(module@check_fns)[f] %>% 
          stringr::str_c("check", sep = "_")
        
        check_fn <- module@check_fns[[f]]
        
        add_out[[check_fn_name]] <- check_fn(df = df, object = object)
        
      }
      
      out <- c(out, add_out)
      
    }
    
  }
  
  # additional
  if(base::isTRUE(check_additional)){
    
    additional_variable_names <- getAdditionalVariableNames(object)
    
    df_names <- base::colnames(df)
    
    # grouping
    for(vg in base::seq_along(additional_variable_names$grouping)){
      
      variable_name <- additional_variable_names$grouping[vg]
      
      var <- df[[variable_name]]
      
      res <- 
        check_and_convert_grouping_var(var = var, ref = variable_name)
      
      if(base::is.null(res$problem)){
        
        out[[variable_name]] <- res$var
        
      } else {
        
        out[[variable_name]] <- res$problem
        
      }
      
    }
    
    # numeric check
    for(vn in base::seq_along(additional_variable_names$numeric)){
      
      variable_name <- additional_variable_names$numeric[vn]
      
      var <- df[[variable_name]]
      
      res <- 
        check_and_convert_numeric_var(var = var, ref = variable_name)
      
      if(base::is.null(res$problem)){
        
        out[[variable_name]] <- res$var
        
      } else {
        
        out[[variable_name]] <- res$problem
        
      }
      
    }
    
  }
  
  return(out)
  
})



validate_variables_hlpr <- function(variables, df, out_list, object){
  
  for(i in base::seq_along(variables)){
    
    vr <- variables[[i]]
    
    name_in_example <- vr@name_in_example
    
    name_in_cypro <- vr@name_in_cypro
    
    var_out <-
      vr@check_var(
        df = df,
        name_in_cypro = name_in_cypro,
        object = object
      )
    
    if(methods::is(var_out, "glue")){
      
      out_list[[name_in_cypro]] <- var_out
      
    } else {
      
      out_list[[name_in_cypro]] <- var_out
      
      if(!base::is.na(name_in_example)){
      
        df[[name_in_example]] <- var_out  
        
      }
      
    }
    
  }
  
  res <- list(out_list = out_list, df = df)
  
  return(res)
  
}


# W -----------------------------------------------------------------------

#' @title Validate well plate information
#' 
#' @description Validates the content of variables regarding the 
#' well plate layout: \emph{well}, \emph{roi} and \emph{well_roi} by 
#' using regular expressions.
#' 
#' @inherit argument_dummy params
#' 
#' @return A named logical vector of the same length as the input vector. 
#' 
#' @export 

validateWells <- function(wells, cypro_nc = FALSE){
  
  wells <- base::as.character(wells)
  
  pattern <-
    base::ifelse(
      test = base::isTRUE(cypro_nc),
      yes = rgx_well_cypro,
      no = rgx_well
    )
  
  stringr::str_detect(string = wells, pattern = pattern) %>% 
    purrr::set_names(nm = wells)
  
}

#' @rdname validateWells
#' @export

validateRois <- function(rois, cypro_nc = FALSE){
  
  rois <- base::as.character(rois)
  
  pattern <-
    base::ifelse(
      test = base::isTRUE(cypro_nc),
      yes = rgx_roi_cypro,
      no = rgx_roi
    )
  
  stringr::str_detect(string = rois, pattern = pattern) %>% 
    purrr::set_names(nm = rois)
  
}

#' @rdname validateWells
#' @export

validateWellRois <- function(well_rois, cypro_nc = FALSE){
  
  well_rois <- base::as.character(well_rois)
  
  pattern <-
    base::ifelse(
      test = base::isTRUE(cypro_nc),
      yes = rgx_well_roi_cypro,
      no = rgx_well_roi
    )
  
  stringr::str_detect(string = well_rois, pattern = pattern) %>% 
    purrr::set_names(nm = well_rois)
  
}




# V -----------------------------------------------------------------------


#' @title Validate variable assignment
#' 
#' @description Validates that each data variable has been assigned a unique 
#' variable from the input data.
#'
#' @inherit argument_dummy params 
#'
#' @return TRUE or FALSE
#' @export
#'
setGeneric(name = "validateVariableAssignment", def = function(object, ...){
  
  standardGeneric(f = "validateVariableAssignment")
  
})


#' @rdname validateVariableAssignment
#' @export
setMethod(
  f = "validateVariableAssignment",
  signature = "Cypro",
  definition = function(object, modules = NULL, in_shiny = FALSE, stop_if_false = FALSE, ...){
    
    used_names_vec <- 
      getVariableAssignment(object, modules = modules, ...) %>% 
      purrr::flatten_chr() %>% 
      purrr::discard(.x = ., .p = ~ base::is.na(.x))
    
    if(base::length(used_names_vec) == 0){
      
      fdb_fn <- base::ifelse(test = stop_if_false, yes = "stop", no = "warning")
      
      if(base::is.character(modules)){
        
        ref_modules <- 
          confuns::scollapse(modules) %>% 
          stringr::str_c(" for module(s) '", ., "'", sep = "")
        
      } else {
        
        ref_modules <- ""
        
      }
      
      msg <- glue::glue("No variables have been assigned{ref_modules}.")
      
      confuns::give_feedback(
        msg = msg, 
        in.shiny = in_shiny, 
        fdb.fn = fdb_fn, 
        with.time = FALSE
      )
      
      res <- FALSE
      
      
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
        
        res <- FALSE
        
        if(base::isTRUE(stop_if_false)){
          
          ovlp <- confuns::scollapse(overlapping_names)
          
          if(base::is.character(modules)){
            
            modules <- object@modules[modules] 
            
          } else {
            
            modules <- object@modules
            
          } 
          
          ref_modules <- 
            purrr::map_chr(.x = modules, .f = ~ .x@name_in_app) %>% 
            confuns::scollapse()
          
          msg <-
            glue::glue(
              "Duplicated variable assignment is not allowed. ",
              "Across module(s) '{ref_modules}' the following {ref} {ref2} several times: ",
              "'{ovlp}'",
              ref = confuns::adapt_reference(overlapping_names, "variable", "variables"),
              ref2 = confuns::adapt_reference(overlapping_names, "exists", "exist")
            )
          
          confuns::give_feedback(
            msg = msg, 
            fdb.fn = "stop",
            with.time = FALSE,
            in.shiny = in_shiny, 
            duration = 20
          )
          
        }
        
      } else { 
        
        res <- TRUE    
        
      }
      
    }
    
    return(res)
    
  })






# not exported ------------------------------------------------------------



# e -----------------------------------------------------------------------

validate_experiment_name <- function(exp_name, stop_if_false = FALSE){
  
  validate_simple_string(
    string = exp_name, 
    ref_invalid = "experiment name", 
    stop_if_false = stop_if_false
  )
  
}

# n -----------------------------------------------------------------------

validate_no_overlap_directories <- function(directories, fdb_if_false = FALSE, fdb_fn = "stop", in_shiny = FALSE){
  
  used_directories_count <- 
    base::table(directories) %>% 
    base::as.data.frame() 
  
  overlapping_names <- 
    magrittr::set_colnames(used_directories_count, value = c("name", "count")) %>% 
    dplyr::mutate(name = base::as.character(name)) %>% 
    dplyr::mutate(name = glue::glue("{name} ({count}x)")) %>% 
    dplyr::filter(count > 1) %>% 
    dplyr::pull(name)
  
  if(base::length(overlapping_names) >= 1){
    
    res <- FALSE
    
    if(base::isTRUE(fdb_if_false)){
      
      ovlp <- confuns::scollapse(overlapping_names)

      msg <-
        glue::glue(
          "Duplicated folder assignment is not allowed. ",
          "The following {ref} {ref2} several times: ",
          "'{ovlp}'",
          ref = confuns::adapt_reference(overlapping_names, "directory", "directories"),
          ref2 = confuns::adapt_reference(overlapping_names, "exists", "exist")
        )
      
      confuns::give_feedback(
        msg = msg, 
        fdb.fn = fdb_fn,
        with.time = FALSE,
        in.shiny = in_shiny, 
        duration = 20
      )
      
    }
    
  } else { 
    
    res <- TRUE    
    
  }
  
  return(res)
  
}

validate_no_overlap_additional_vars <- function(grouping_vars,
                                                numeric_vars,
                                                stop_if_false = TRUE,
                                                in_shiny = FALSE){
  
  used_names_vec <- c(grouping_vars, numeric_vars)
  
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
    
    res <- FALSE
    
    if(base::isTRUE(stop_if_false)){
      
      ovlp <- confuns::scollapse(overlapping_names)
      
      if(base::is.character(modules)){
        
        modules <- object@modules[modules] 
        
      } else {
        
        modules <- object@modules
        
      } 
      
      msg <-
        glue::glue(
          "Duplicated variable assignment is not allowed. ",
          "The following {ref} {ref2} several times: ",
          "'{ovlp}'",
          ref = confuns::adapt_reference(overlapping_names, "variable", "variables"),
          ref2 = confuns::adapt_reference(overlapping_names, "exists", "exist")
        )
      
      confuns::give_feedback(
        msg = msg, 
        fdb.fn = "stop",
        with.time = FALSE,
        in.shiny = in_shiny, 
        duration = 20
      )
      
    }
    
  } else { 
    
    res <- TRUE    
    
  }
  
  invisible(res)
  
}


# s -----------------------------------------------------------------------

validate_simple_string <- function(string, ref_invalid, stop_if_false = FALSE){
  
  res <- base::vector(length = 4)
  
  res[1] <- base::length(string) >= 1
  
  res[2] <- stringr::str_detect(string = string, pattern = "^[a-zA-Z]")
  
  string <- stringr::str_remove_all(string = string, pattern = "^ *")
  string <- stringr::str_remove_all(string = string, pattern = " *$")
  
  res[3] <- !stringr::str_detect(string = string, pattern = " ")
  
  res[4] <- !stringr::str_detect(string = string, pattern = "-")
  
  valid <- base::all(res)
  
  if(base::isFALSE(valid)){
    
    stop_validation_function(
      stop_if_false = stop_if_false, 
      ref_invalid = ref_invalid, 
      feedback_slot = "invalid_simple_string"
    )
    
  }
  
  return(valid)
  
}



# w -----------------------------------------------------------------------

validate_well_plate_name <- function(wp_name, stop_if_false = FALSE){
  
  validate_simple_string(
    string = wp_name, 
    ref_invalid = "well plate name", 
    stop_if_false = stop_if_false
  )
  
}







remove_empty_space <- function(string){
  
  if(base::is.character(string)){
    
    string <- 
      stringr::str_remove_all(string = string, pattern = "^ *") %>% 
      stringr::str_remove_all(pattern = " *$")
  }

  return(string)
  
}




#?
stop_validation_function <- function(stop_if_false,
                                     ref_invalid,
                                     feedback,
                                     feedback_add = "",
                                     feedback_slot = NULL){
  
  if(base::isTRUE(stop_if_false)){
    
    if(base::is.character(feedback_slot)){
      
      feedback <- feedback_list[[feedback_slot]]
      
    } else {
      
      feedback <- ""
      
    }
    
    msg <- glue::glue("Invalid {ref_invalid}. {feedback} {feedback_add}")
      
    confuns::give_feedback(
      msg = msg, 
      with.time = FALSE,
      fdb.fn = "stop", 
      in_shiny = in_shiny
    )
    
  }
  
  
}

