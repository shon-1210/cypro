




# c -----------------------------------------------------------------------

#' @rdname is_cypro_df
#' @export
is_cluster_df <- function(df, stop_if_false = FALSE){
  
  df <- dplyr::select(df, -dplyr::any_of("cell_id"))
  
  out <- logical(2)
  
  out[1] <- 
    check_data_frame(
      df = df, 
      var.class = purrr::map(df, function(x){return("factor")}) %>%
        purrr::set_names(nm = base::names(df)), 
      verbose = stop_if_false
    )
  
  out[2] <- isOfClass(df, valid_class = "cluster_df")
  
  if(base::isFALSE(out[2]) && base::isTRUE(stop_if_false)){
    
    stop(valid_content_but_inappropriate_class("cluster_df"))
    
  }
  
  out <- base::all(out)
  
  return(out)
  
}



#' @title Test completeness
#' 
#' @description Tests if the data.frame has been completed.
#'
#' @inherit as_cypro_df params
#' 
#' @details Only relevant for \code{cypro_df} data.frames of subclass
#' \code{stats_df} or \code{tracks_df}.
#'
#' @return TRUE or FALSE
#' 
#' @seealso \code{completeStatsDf()}, \code{completeTracksDf()}
#' 
#' @export
#'

is_complete <- function(df){
  
  UseMethod(generic = "is_complete", object = df)
  
}

#' @rdname is_complete
#' @export
is_complete.cypro_df <- function(df){
  
  out <- base::attr(df, which = "complete")
  
  base::isTRUE(out)
  
}




#' @title Test known data.frame classes
#' 
#' @description Logical tests that check input data.frame for its class
#' and its content. 
#' 
#' @param df A data.frame.
#' @inherit argument_dummy params
#' 
#' @details Tests if the input data.frame is of the respective class by 
#' checking its attributes. Additionally tests if the content of the data.frame
#' matches the requirements. If both tests evaluate to TRUE the function 
#' returns TRUE else it returns FALSE.
#'
#' @return TRUE or FALSE
#' @export
#'
is_cypro_df <- function(df, stop_if_false = FALSE){
  
  out <- logical(2)
  
  out[1] <- 
    check_data_frame(
      var.class = var_classes_cypro_df, 
      verbose = stop_if_false
    )
  
  out[2] <- isOfClass(df, valid_class = "cypro_df")
  
  if(base::isFALSE(out[2]) && base::isTRUE(stop_if_false)){
    
    stop(valid_content_but_inappropriate_class("cypro_df"))
    
  }
  
  out <- base::all(out)
  
  return(out)
  
}



# w -----------------------------------------------------------------------

#' @rdname is_cypro_df
#' @export
is_well_plate_df <- function(df, stop_if_false = FALSE){
  
  out <- logical(2)
  
  out[1] <- 
    check_data_frame(
      df = df, 
      var.class = vars_well_plate_df, 
      verbose = stop_if_false
    )
  
  out[2] <- isOfClass(df, valid_class = "well_plate_df")
  
  if(base::isFALSE(out[2]) && base::isTRUE(stop_if_false)){
    
    stop(valid_content_but_inappropriate_class("well_plate_df"))
    
  }
  
  out <- base::all(out)
  
  return(out)
  
}







# A -----------------------------------------------------------------------

#' @title Test module activity 
#' 
#' @description Tests if the module's slot @@active is set to TRUE.
#' 
#' @inherit argument_dummy params
#' 
#' @details The slot @@active indicates whether the user declared to want to 
#' use the module during \code{assignVariables()}. A module becomes \emph{usable}
#' if all assigned variables are valid. 
#' 
#' @return TRUE or FALSE
#' 
#' @export

setGeneric(name = "isActive", def = function(object, ..., stop_if_false = FALSE){
  
  standardGeneric(f = "isActive")
  
})

#' @rdname isActive
#' @export
setMethod(f = "isActive", signature = "Cypro", definition = function(object, module_name, ..., stop_if_false = FALSE){
  
  res <-
    getModule(object = object, module_name = module_name) %>% 
    isActive()
  
  if(base::isFALSE(res) && base::isTRUE(stop_if_false)){
    
    stop(glue::glue("Module '{module_name}' is inactive."))
    
  }
  
  return(res)
  
})

#' @rdname isActive
#' @export
setMethod(f = "isActive", signature = "AnalysisModule", definition = function(object, ..., stop_if_false = FALSE){
  
  res <- object@active
  
  module_name <- object@name_in_cypro
  
  if(base::isFALSE(res) && base::isTRUE(stop_if_false)){
    
    stop(glue::glue("Module '{module_name}' is inactive."))
    
  }
  return(res)
  
})


# C -----------------------------------------------------------------------

#' @rdname is_cypro_df
#' @export
isClusterDf <- is_cluster_df


#' @title Test example data.frame
#' 
#' @description Tests if the data file loaded as an example data.frame during 
#' \code{assignVariables()} covers the whole experiment.
#' 
#' @inherit argument_dummy params
#' 
#' @return TRUE or FALSE
#' 
#' @export
setGeneric(name = "isCompleteExampleDf", def = function(object){
  
  standardGeneric(f = "isCompleteExampleDf")
  
})

#' @rdname isCompleteExampleDf
#' @export
setMethod(f = "isCompleteExampleDf", signature = "Cypro", function(object){
  
  warning("isCompleteExampleDf is used here")
  
  loading_fn <- suggestLoadingFunction(object)
  
  if(loading_fn != "loadByFile"){
    
    res <- FALSE
    
  } else {
    
    example_df <- getExampleDf(object)
    
    id_assignment <- getVariableAssignmentID(object, drop_na = FALSE)
    
    wp_names <- getWellPlateNames(object)
    
    n_wp_names <- base::length(wp_names)
    
    if(n_wp_names == 1){
      
      res <- TRUE  
      
    } else {
      
      wp_names_example <- 
        dplyr::pull(example_df, var = {{wp_assignment}}) %>%
        base::unique()
      
      if(base::all(wp_names %in% wp_names_example)){
        
        res <- TRUE
        
      } else {
        
        res <- FALSE
        
      }
      
    }
    
  }
  
  return(res)
  
  
})



#' @rdname is_complete
#' @export
isComplete <- function(df){
  
  UseMethod(generic = "isComplete", object = df)
  
}

#' @rdname is_complete
#' @export
isComplete.cypro_df <- is_complete.cypro_df




#' @rdname is_cypro_df
#' @export
isCyproDf <- is_cypro_df


# E -----------------------------------------------------------------------


# L -----------------------------------------------------------------------

#' @title Test layout data.frame
#' 
#' @description Tests if the input data.frame is of class \code{layout_df}.
#'
#' @param df A data.frame.
#'
#' @return TRUE or FALSE.
#' @export
#'
isLayoutDf <- function(df){
  
  if("layout_df" %in% base::class(df)){
    
    res <- TRUE
    
  } else {
    
    res <- FALSE
    
  }
  
  return(res)
  
}




#' @title Test if loaded
#' 
#' @description Tests if the data file has been loaded yet.
#'
#' @inherit argument_dummy params
#'
#' @return TRUE or FALSE
#' @export
#'
setGeneric(name = "isLoaded", def = function(object){
  
  standardGeneric(f = "isLoaded")
  
})

#' @rdname isLoaded
#' @export
setMethod(f = "isLoaded", signature = "DataFile", definition = function(object){
  
  content <- object@content
  
  res <- !base::identical(x = content, y = list())
  
  return(res)
  
})



# M -----------------------------------------------------------------------

#' @title Test layout data.frame modification
#' 
#' @description Checks if the input data.frame has been modified by \code{setWellInfo()}.
#' 
#' @param df  A data.frame.
#' 
#' @return TRUE or FALSE.
#' 
isModifiedLayoutDf <- function(df){
  
  base::stopifnot(isLayoutDf(df))
  
  cell_line <- base::any(!base::is.na(df$cell_line))
  condition <- base::any(!base::is.na(df$condition))
  
  modified <- base::any(c(cell_line, condition))
  
  return(modified)  
  
}

#' @title Test multiple phases
#' 
#' @description Tests if the provided object derives from a multiple phase experiment.
#'
#' @inherit argument_dummy params
#'
#' @return TRUE or FALSE
#' @export
#'
setGeneric(name = "isMultiplePhase", def = function(object, stop_if_false = FALSE){
  
  standardGeneric(f = "isMultiplePhase")
  
})


#' @rdname isMultiplePhase
#' @export
setMethod(f = "isMultiplePhase", signature = "WellPlate", function(object, stop_if_false = FALSE){
  
  res <- base::is.numeric(base::attr(object@layout, "n_phases"))
  
  if(base::isFALSE(res) && base::isTRUE(stop_if_false)){
    
    stop("Well plate '{object@name}' of '{object@experiment}' does not derive from multiple phase experiment.")
    
  }
  
  return(res)
  
})

#' @rdname isMultiplePhase
#' @export
setMethod(f = "isMultiplePhase", signature = "ExperimentDesign", function(object, stop_if_false = FALSE){
  
  res <- object@type == "CyproTimeLapseMP"
  
  if(base::isFalse(res) && base::isTRUE(stop_if_false)){
    
    stop("Experiment design of '{object@experiment}' does not include multiple phases.")
    
  }
  
  return(res)
  
  
})

#' @rdname isMultiplePhase
#' @export
setMethod(f = "isMultiplePhase", signature = "Cypro", function(object, stop_if_false = FALSE){
  
  res <- methods::is(object, class2 = "CyproTimeLapseMP")
  
  if(base::isFALSE(res) && base::isTRUE(stop_if_false)){
    
    stop("Experiment design of '{object@name}' does not include multiple phases.")
    
  }
  
  return(res)
  
})





# N -----------------------------------------------------------------------




#' @title Test layout data.frame 
#' 
#' @description Tests if the layout data.frame exists in nested form.
#'
#' @param df A data.frame of class \code{layout_df}.
#'
#' @return TRUE or FALSE
#' 
#' @export
isNested <- function(df){
  
  UseMethod(generic = "isNested", object = df)
  
}

#' @rdname isNested
#' @export
isNested.layout_df <- function(df){
  
  out <- "roi_info" %in% base::colnames(df)
  
  return(out)
  
}



# O -----------------------------------------------------------------------


isOfLength <- function(x, l){
  
  res <- base::length(x) == l
  
  return(res)
  
}



# S -----------------------------------------------------------------------

#' @title Test single phase
#' 
#' @description Tests if the provided object derives from a single phase experiment.
#'
#' @inherit argument_dummy params
#'
#' @return TRUE or FALSE
#' @export
#'
setGeneric(name = "isSinglePhase", def = function(object, ...){
  
  standardGeneric(f = "isSinglePhase")
  
})

#' @rdname isSinglePhase
#' @export
setMethod(f = "isSinglePhase", signature = "WellPlate", function(object, stop_if_false = FALSE){
  
  res <- bas::is.null(base::attr(object@layout, "n_phases"))
  
  if(base::isFALSE(res) && base::isTRUE(stop_if_false)){
    
    stop("Well plate '{object@name}' of '{object@experiment}' derives from multiple phase experiment.")
    
  }
  
  return(res)
  
})

#' @rdname isSinglePhase
#' @export
setMethod(f = "isSinglePhase", signature = "ExperimentDesign", function(object, stop_if_false = FALSE){
  
  res <- object@type != "CyproTimeLapseMP"
  
  if(base::isFalse(res) && base::isTRUE(stop_if_false)){
    
    stop("Experiment design of '{object@experiment}' includes multiple phases.")
    
  }
  
  return(res)
  
  
})

#' @rdname isSinglePhase
#' @export
setMethod(f = "isSinglePhase", signature = "Cypro", function(object, stop_if_false = FALSE){
  
  res <- !methods::is(object, class2 = "CyproTimeLapseMP")
  
  if(base::isFALSE(res) && base::isTRUE(stop_if_false)){
    
    stop("Experiment design of '{object@experiment}' includes multiple phases.")
    
  }
  
  return(res)
  
})

# U -----------------------------------------------------------------------


#' @title Test module usability
#' 
#' @description Tests if the module's slot @@active is set to TRUE AND 
#' tests if all required variables have been assigned to a variable 
#' in the input example data.frame that meets the variable specific requirements.
#' 
#' @inherit argument_dummy params
#' 
#' @return TRUE or FALSE
#' 

setGeneric(name = "isUsable", def = function(object, ..., fdb_if_false = FALSE, fdb_fn = "warning", verbose = FALSE){
  
  standardGeneric(f = "isUsable") 
  
})

#' @rdname isUsable
#' @export
setMethod(f = "isUsable", signature = "AnalysisModule", definition = function(object, ..., fdb_if_false = FALSE, fdb_fn = "warning", verbose = FALSE){
  
  errors <- base::vector(mode = "character")
  
  errors[1] <- base::ifelse(test = base::isTRUE(object@active), yes = NA_character_, no = "@active is set to FALSE")
  
  all_vars_assigned <- 
    purrr::map_lgl(.x = object@variables_required, .f = ~ !base::is.na(.x@name_in_example)) %>% 
    base::all()
  
  errors[2] <- base::ifelse(test = all_vars_assigned, yes = NA_character_, no = "required variables have not been assigned")
  
  res <- base::all(base::is.na(errors))
  
  if(base::isFALSE(res) && base::isTRUE(fdb_if_false)){
    
    reasoning <- 
      purrr::discard(.x = errors, .p = ~ base::is.na(.x)) %>% 
      stringr::str_c(., collapse = " and ")
    
    msg <- glue::glue("Module '{object@name_in_app}' is not usable as {reasoning}.")
    
    confuns::give_feedback(msg = msg, fdb.fn = fdb_fn, ...)
    
  } else if(base::isTRUE(res)){
    
    msg <- glue::glue("Module '{object@name_in_app}' is usable.")
    
    confuns::give_feedback(msg = msg, ..., verbose = verbose)
    
  }
  
  return(res)
  
})

#' @rdname isUsable
#' @export
setMethod(f = "isUsable", signature = "Cypro", definition = function(object, module_name, ..., fdb_if_false = FALSE, fdb_fn = "warning", verbose = FALSE){
  
  module <- getModule(object = object, module_name = module_name)
  
  res <- isUsable(object = module, fdb_if_false = fdb_if_false, fdb_fn = fdb_fn, ..., verbose = verbose)
  
  return(res)
  
})

















# W -----------------------------------------------------------------------

#' @rdname is_cypro_df
#' @export
isCyproDf <- is_cypro_df