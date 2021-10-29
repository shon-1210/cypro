
#' @include compute.R logical-tests.R r-objects.R
#' 
NULL

#' @include logical-tests.R
NULL


# module blueprint --------------------------------------------------------

# each known variable in cypro has it's own list of information & tests
# a variable can appear in different modules


# module variable requirements --------------------------------------------
# candidate check (for picker outputs) return TRUE or FALSE
is_grouping_candidate <- function(var){
  
  var <- var[!base::is.na(var)]
  
  if(base::is.character(var) | base::is.factor(var)){
    
    res <- TRUE
    
    # if not character of factor -> numeric
  } else if(!base::any(stringr::str_detect(var, pattern = "\\."))){ # . in var -> double
    
    res <- TRUE
    
  } else {
    
    res <- FALSE
    
  }
  
  return(res)
  
}

is_numeric_candidate <- function(var){
  
  if(base::is.numeric(var)){
    
    res <- TRUE
    
  } else if(base::is.character(var)){
    
    n_na <- base::is.na(var) %>% base::sum()
    
    var_numeric <- base::suppressWarnings({ base::as.numeric(var) })
    
    n_na_new <- base::is.na(var_numeric) %>% base::sum()
    
    if(n_na_new > n_na){
      
      res <- FALSE
      
    } else {
      
      res <- TRUE
      
    }
    
  } else {
    
    res <- FALSE
    
  }
  
  return(res)
  
}

# module variable tests ---------------------------------------------------



# content check (attempts to convert if possible), returns:
# variable if valid 
# variable + warning in case of NA introduction
# error in case of only NAs after conversion or if var is NULL



# identifier variables ----------------------------------------------------


check_and_convert_vars_frame_num_and_cell_id <- function(var_frame_num,
                                                         ref_frame_num,
                                                         var_cell_id,
                                                         ref_cell_id,
                                                         in_shiny){
  
  var_frame_num <- 
    check_and_convert_numeric_var(
      var = var_frame_num,
      ref = ref_frame_num,
      in_shiny = in_shiny
      )
  
  var_cell_id <- 
    check_and_convert_grouping_var(
      var = var_cell_id,
      ref = ref_cell_id,
      in_shiny = in_shiny
      )
  
  identifier_df <-
    base::data.frame(cell_id = var_cell_id, frame = var_frame_num) %>% 
    magrittr::set_colnames(value = c(ref_cell_id, ref_frame_num))
  
  n_obs <- base::nrow(identifier_df)
  
  n_unique_obs <- 
    dplyr::distinct(identifier_df) %>% 
    base::nrow()
  
  if(n_obs > n_unique_obs){
    
    msg  <- 
      glue::glue(
        "Frame-variable {ref_frame_num} and Cell ID-variable {ref_cell_id} do not", 
        "uniquely identify each observation. Invalid variable assignment."
        )
    
    confuns::give_feedback(
      msg = msg,
      fdb.fn = "stop",
      in.shiny = in_shiny,
      with.time = FALSE
      )
    
  } else {
    
    var_list <- base::as.list(identifier_df)
    
    return(var_list)
    
  }
  
}



