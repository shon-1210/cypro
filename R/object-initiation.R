
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







#' @title Object initiation: Step 3
#' 
#' @description Processes the data and constructs all needed slots. Afterwards the 
#' cypro object is set for all subsequent analysis and visualization steps. 
#'
#' @inherit argument_dummy params
#'
#' @inherit updated_object return
#' @export
#'
processDataX <- function(object,
                        summarize_with = c("max", "min", "mean", "median"),
                        verbose = TRUE){
  
  check_object(object, set_up_req = "load_data")
  
  # add default list 
  object@default <- c(object@default, default_list)
  
  confuns::check_one_of(
    input = summarize_with, 
    against = base::names(stat_funs)
  )
  
  if(base::isTRUE(object@set_up$progress$process_data)){
    
    base::stop("Object has already been processed.")
    
  }

  # update progress slot
  object@set_up$progress$process_data <- TRUE
  
  confuns::give_feedback(msg = "Processing data.", verbose = verbose)
  
  # set up different data slots ---------------------------------------------
  
  # set up wp data
  object <- set_up_cdata_well_plate(object, verbose = verbose)
  
  # set up meta data 
  object <- set_up_cdata_meta(object, verbose = verbose)
  
  # set up cluster data 
  object <- set_up_cdata_cluster(object, verbose = verbose)
  
  # set up tracks 
  object <- set_up_cdata_tracks(object, verbose = verbose)
  
  # set up stats 
  if(isTimeLapseExp(object)){
    
    object <- 
      set_up_cdata_stats(
        object = object,
        summarize_with = summarize_with,
        verbose = verbose
        )  
    
  }
  
  # set up variable data
  object <- set_up_vdata(object, verbose = verbose)
  
  # miscellaneous -----------------------------------------------------------
  
  if(multiplePhases(object)){
    
    all_phases <- getPhases(object)
    
    object@qcheck$na_count <- 
      purrr::map(
        .x = all_phases,
        .f =
          ~ compute_n_missing_values(
              object = object,
              phase = .x,
              verbose = verbose
            )
        ) %>% 
      purrr::set_names(nm = all_phases)
    
  } else if(isTimeLapseExp(object)) {
    
    object@qcheck$na_count <- 
      compute_n_missing_values(object, verbose = verbose)
    
  } else {
    
    # no NA counting by cell id in non time lapse experiments 
    
  }
  
  # remove wp_df in favor of wp_df_eval
  object@well_plates <-
    purrr::map(.x = object@well_plates, .f = function(wp_list){
                 
                 wp_list$wp_df <- NULL
                 
                 base::return(wp_list)
                 
                 })
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}


