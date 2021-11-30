# a -----------------------------------------------------------------------


#' @title Function to make well vectors compatible with cypros terminology
#' @export
adjust_well_vec <- function(well_vec){
  
  row_letters <- stringr::str_extract(string = well_vec, pattern = "[A-Z]")
  
  col_numbers <-
    stringr::str_extract(string = well_vec, pattern = "\\d{1,2}$") %>% 
    stringr::str_remove(pattern = "^0")
  
  clean_well_vec <- stringr::str_c(row_letters, col_numbers, sep = "")
  
  return(clean_well_vec)
  
}

#' @title Function to make well roi vectors compatible with cypros terminology
#' @export
adjust_well_roi_vec <- function(well_roi_vec){
  
  well_vec <-
    stringr::str_extract(string = well_roi_vec, pattern = "[A-Z]\\d{1,2}") %>% 
    adjust_well_vec()
  
  roi_vec <- 
    stringr::str_extract(string = well_roi_vec, pattern = "\\d{1,2}$") %>% 
    stringr::str_remove(pattern = "^0")
  
  clean_well_roi_vec <- 
    stringr::str_c(well_vec, roi_vec, sep = "_")
  
  return(clean_well_roi_vec)
  
}



# c -----------------------------------------------------------------------

count_valid_dirs <- function(vec){
  
  not_na <- !base::is.na(vec)
  
  n_not_na <- base::sum(not_na)
  
  return(n_not_na)
  
}


cypro_object_ready <- function(object, verbose = TRUE){
  
  give_feedback(
    msg = glue::glue("{base::class(object)} object '{object@experiment}' is all set for downstream analysis."), 
    verbose = verbose
  )
  
}

cypro_object_is_being_subsetted <- function(object, by, verbose = TRUE, prev_subset = NULL){
  
  if(!isOfClass(prev_subset, "CyproSubset")){
    
    exp_name <- object@experiment
    
    obj_class <-
      base::class(object) %>% 
      base::as.character()
    
    give_feedback(
      msg = glue::glue("Subsetting {obj_class} object '{exp_name}' by {by}."), 
      verbose = verbose
    )
    
  }
  
  NULL
  
}

# d -----------------------------------------------------------------------

data_status_by_well <- function(vec){
  
  if(base::all(vec == 1)){
    
    out <- "Complete"
    
  } else if(base::any(vec > 1)){
    
    out <- "Ambiguous"
    
  } else if(base::all(vec == 0)){
    
    out <- "Missing"
    
  } else if(base::any(vec == 0)){
    
    out <- "Incomplete"
    
  }
  
  return(out)
  
  
}

#' @rdname selectWells
#' @export

deselectWells <- function(df, wells = NULL){
  
  UseMethod(generic = "deselectWells", object = df)
  
}

#' @rdname selectWells
#' @export

deselectWells.layout_df <- function(df, wells = NULL){
  
  if(base::is.null(wells)){
    
    wells <- base::unique(df$well)
    
  }
  
  check_one_of(
    input = wells, 
    against = base::unique(df$well), 
    fdb.fn = "warning", 
    with.time = FALSE
  )
  
  out <- 
    dplyr::mutate(
      .data = df, 
      selected = dplyr::if_else(
        condition = well %in% {{wells}},
        true = FALSE,
        false = selected
      )
    )
  
  return(out)
  
  
  
}


#' @inherit confuns::detectOutliers title description params
#' @export
setGeneric(name = "detectOutliers", def = getGeneric(f = "detectOutliers", package = "confuns"))

#' @rdname detectOutliers
#' @export
setMethod(
  f = "detectOutliers",
  signature = "OutlierDetection",
  definition = getMethod(f = "detectOutliers",
                         signature = "OutlierDetection",
                         where = asNamespace(ns = "confuns"))
)

#' @rdname detectOutliers
#' @export 
setMethod(
  f = "detectOutliers",
  signature = "CyproScreening",
  definition = function(object, method, across = NULL, verbose = TRUE){
    
    outlier_object <- getOutlierDetection(object)
    
    outlier_object <-
      detectOutliers(object = outlier_object, method = method, across = across, verbose = verbose)
    
    object <- setOutlierDetection(object, outlier_object = outlier_object)
    
    return(object)
    
  })

# e -----------------------------------------------------------------------


#' @title Empty data files 
#' 
#' @description Empties slot @@content of objects of class \code{DataFile}.
#'
#' @inherit argument_dummy params
#' @param transferred Logical value. Indicates that the emptying takes place due to 
#' the content beeing transferred to an object of class \code{Cdata}.
#' 
#' @details Empties slot @@content of objects of class \code{DataFile} that were
#' created for data loading and sets slot @@transferred to TRUE indicating 
#' that the content has been merged via \code{transferData()} and transferred
#' to the \code{Cypro} objects @@cdata slot in form of proper \code{cypro_df} data.frames.
#'
#' @return The adjusted input object. 
#' @export
#'
setGeneric(name = "emptyDataFiles", def = function(object, ...){
  
  standardGeneric(f = "emptyDataFiles")
  
})

#' @rdname emptyDataFiles
#' @export
setMethod(f = "emptyDataFiles", signature = "WellPlate", definition = function(object, transferred = TRUE){
  
  object@files <- 
    purrr::map(object@files, .f = function(object){
      
      if(!containsErrors(object)){
        
        object@content <- list()
        object@transferred <- transferred
        
      }
      
      return(object)
      
    })
  
  return(object)
  
})

#' @rdname emptyDataFiles
#' @export
setMethod(f = "emptyDataFiles", signature = "ExperimentDesign", definition = function(object, transferred = TRUE){
  
  object@well_plates <- 
    purrr::map(
      .x = object@well_plates, 
      .f = ~ emptyDataFiles(.x)
    )
  
  return(object)
  
})

#' @rdname emptyDataFiles
#' @export
setMethod(f = "emptyDataFiles", signature = "Cypro", definition = function(object, transferred = TRUE){
  
  exp_design <- 
    getExperimentDesign(object) %>% 
    emptyDataFiles()
  
  object <- 
    setExperimentDesign(object, exp_design = exp_design)
  
  return(object)
  
})





# f -----------------------------------------------------------------------

fdb_empty_slot <- function(eval, ref, stop_if_empty = FALSE){
  
  if(base::isTRUE(eval) && stop_if_empty){
    
    give_feedback(
      msg = glue::glue("Slot @{ref} is empty."), 
      fdb.fn = "stop", 
      with.time = FALSE
    )
    
  }
  
  return(eval)
  
}

file_dir_validity <- function(dir){
  
  if(base::length(dir) == 0){
    
    out <- "Missing"
    
  } else if(base::length(dir) > 1){
    
    out <- "Ambiguous"
    
  } else if(base::is.na(dir)){
    
    out <- "Missing"
    
  } else if(base::length(1) & base::is.character(dir)){
    
    out <- "Valid"
    
  }
  
  return(out)
  
}

flatten_file_paths <- function(input_dir){
  
  root <-
    stringr::str_remove(input_dir$root, pattern = "\\(") %>% 
    stringr::str_remove(pattern = "\\)")
  
  paths <-
    purrr::map(.x = input_dir$files, .f = ~ .x) %>% 
    purrr::map(.f = ~ purrr::discard(.x = .x, .p = ~ .x == "")) %>% 
    purrr::map(.f = ~ purrr::flatten_chr(.x)) %>% 
    purrr::map(.f = ~ stringr::str_c(.x, collapse = "/") %>% stringr::str_c(root, ., sep = "/")) %>% 
    purrr::flatten_chr()
  
  
  return(paths)
  
}

flatten_folder_path <- function(input_dir){
  
  root <-
    stringr::str_remove(input_dir$root, pattern = "\\(") %>% 
    stringr::str_remove(pattern = "\\)")
  
  path <-
    purrr::map_chr(.x = input_dir$path, ~ base::return(.x)) %>% 
    purrr::discard(.p = ~ .x == "") %>% 
    stringr::str_c(collapse = "/") %>% 
    stringr::str_c(root, ., sep = "/")
  
  
  return(path)
  
}

flatten_input_dir <- function(input_dir, by_folder){
  
  if(base::isTRUE(by_folder)){
    
    out <- flatten_folder_path(input_dir = input_dir)
    
  } else {
    
    out <- flatten_file_paths(input_dir = input_dir)
    
  }
  
  return(out)
  
}



# i -----------------------------------------------------------------------


#' @title Initiate empty cypro object 
#' 
#' @description Initiates an empty cypro object and 
#' adds the version under which is is created.
#'
#' @param ... 
#'
initiateEmptyCyproObject <- function(...){
  
  object <- methods::new(Class = "cypro", ...)
  
  object@version <- current_version
  
  base::return(object)
  
}



# j -----------------------------------------------------------------------


#' @title Join cell data frames
#' 
#' @description Joins different variable types of the cell data to a
#' data.frame by the key variable \emph{cell_id}.
#'
#' @inherit argument_dummy params
#' @param df A data.frame of class \code{cypro_df}.
#'
#' @return A data.frame.
#' @export
#'
setGeneric(name = "joinWith", def = function(object, df, ...){
  
  standardGeneric(f = "joinWith")
  
})

#' @rdname joinWith
#' @export
setMethod(f = "joinWith", signature = "Cdata", definition = function(object,
                                                                     df,
                                                                     with_cluster = FALSE, 
                                                                     with_meta = FALSE, 
                                                                     with_well_plate = FALSE, 
                                                                     with_everything = FALSE){
  
  if(base::isTRUE(with_everything)){
    
    with_cluster <- TRUE
    with_meta <- TRUE
    with_well_plate <- TRUE
    
  }
  
  if(base::isTRUE(with_cluster)){
    
    df <- dplyr::left_join(x = df, y = object@cluster, by = "cell_id")
    
  }
  
  if(base::isTRUE(with_meta)){
    
    df <- dplyr::left_join(x = df, y = object@meta, by = "cell_id")
    
  }
  
  if(base::isTRUE(with_well_plate)){
    
    df <- dplyr::left_join(x = df, y = object@well_plate, by = "cell_id")
    
  }
  
  df <- tibble::as_tibble(df)
  
  return(df)
  
})

#' @rdname joinWith
#' @export
setMethod(
  f = "joinWith",
  signature = "CdataTimeLapseMP",
  definition = function(object,
                        df,
                        with_cluster = FALSE, 
                        with_meta = FALSE, 
                        with_well_plate = FALSE, 
                        with_everything = FALSE){
    
    phase <- get_phase(df)
    
    if(base::isTRUE(with_everything)){
      
      with_cluster <- TRUE
      with_meta <- TRUE
      with_well_plate <- TRUE
      
    }
    
    if(base::isTRUE(with_cluster)){
      
      df <- dplyr::left_join(x = df, y = object@cluster[[phase]], by = "cell_id")
      
    }
    
    if(base::isTRUE(with_meta)){
      
      df <- dplyr::left_join(x = df, y = object@meta[[phase]], by = "cell_id")
      
    }
    
    if(base::isTRUE(with_well_plate)){
      
      df <- dplyr::left_join(x = df, y = object@well_plate, by = "cell_id")
      
    }
    
    df <- tibble::as_tibble(df)
    
    return(df)
    
  })

# m -----------------------------------------------------------------------


#' @title Merge multiple phase conditions
#' 
#' @description Merges the list variable \emph{condition}
#' of multiple phase layout data.frames to a character variable.
#'
#' @param df A layout data.frame of class \code{layout_df_mp}.
#'
#' @return The input data.frame.
#' @export
#'
merge_condition <- function(df, ...){
  
  UseMethod(generic = "merge_condition", object = df)
  
}

#' @rdname merge_condition
#' @export
merge_condition.layout_df_mp <- function(df, collapse_with = " -> ", phases = NULL){
  
  is_vec(phases, mode = "numeric", skip.allow = TRUE, skip.val = NULL)
  
  if(base::is.numeric(phases)){
    
    base::stopifnot(base::max(phases) <= nPhases(df))
    
  } else {
    
    phases <- 1:nPhases(df)
    
  }
  
  df$condition <- 
    purrr::map_chr(
      .x = df$condition, 
      .f = function(conds){
        
        conds <- base::as.character(conds[1,phases])
        
        conds[conds == "NA"] <- NA_character_
        
        if(base::all(base::is.na(conds))){
          
          out <- NA_character_
          
        } else if(dplyr::n_distinct(conds) == 1 & base::length(phases) > 1){
          
          out <- 
            base::as.character(conds) %>% 
            base::unique() %>% 
            stringr::str_c("All: ", ., sep = "")
          
        } else {
          
          conds <- base::as.character(conds)
          
          if(base::length(conds) == 1){
            
            out <- conds
            
          } else {
            
            out <- 
              stringr::str_c(phases, ". ", conds, sep = "") %>% 
              stringr::str_c(collapse = collapse_with)
            
          }
          
        }
        
        return(out)
        
      }
    ) 
  
  return(df)
  
}

#' @rdname merge_condition
#' @export
mergeCondition <- function(df, ...){
  
  UseMethod(generic = "mergeCondition", object = df)
  
}

#' @rdname merge_condition
#' @export
mergeCondition.layout_df_mp <- merge_condition.layout_df_mp