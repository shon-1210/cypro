
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
check_and_convert_grouping_var <- function(var, ref, in_shiny = FALSE){
  
  n_nas <- base::is.na(var) %>% base::sum()
  
  l_var <- base::length(var)
  
  if(base::is.null(var)){
    
    msg <- glue::glue( "Variable '{ref}' is missing." )
    
    confuns::give_feedback(
      msg = msg, 
      fdb.fn = "stop", 
      in.shiny = in_shiny, 
      with.time = FALSE
    )
    
  } else if(!base::is.character(var)){
    
    var <- base::as.character(var)
    
    n_nas_new <- base::is.na(var) %>% base::sum()
    
    if(n_nas_new == l_var){
      
      msg <- 
        glue::glue(
          "Attempt to convert variable '{ref}' to type 'character' resulted in only NAs. ",
          "Variable is invalid."
          )
      
      confuns::give_feedback(
        msg = msg, 
        fdb.fn = "stop", 
        in.shiny = in_shiny, 
        with.time = FALSE
      )
      
    } else if(n_nas < n_nas_new){
      
      new_nas <- n_nas_new - n_nas
      
      msg <- glue::glue("Converting variable '{ref}' to character resulted in {new_nas} NAs.")
      
      confuns::give_feedback(
        msg = msg, 
        fdb.fn = "warning", 
        in.shiny = in_shiny, 
        with.time = FALSE
      )
      
    }
    
  }
  
  return(var)
  
}

check_and_convert_numeric_var <- function(var, ref, in_shiny = FALSE){
  
  n_nas <- base::is.na(var) %>% base::sum()
  
  l_var <- base::length(var)
  
  if(base::is.null(var)){
    
    msg <- glue::glue( "Variable '{ref}' is missing." )
    
    confuns::give_feedback(
      msg = msg, 
      fdb.fn = "stop", 
      in.shiny = in_shiny, 
      with.time = FALSE
    )
    
  } else if(!base::is.numeric(var)){
    
    if(base::is.factor(var)){
      
      # convert to character first in case of unordered levels
      var <- base::as.character(var)
      
    }
    
    var <- base::as.numeric(var)
    
    n_nas_new <- base::is.na(var) %>% base::sum()
    
    if(n_nas_new == l_var){
      
       msg <-
         glue::glue(
           "Attempt to convert variable '{ref}' to type 'numeric' resulted in only NAs. ",
           "Variable is invalid."
           )
       
       confuns::give_feedback(
         msg = msg,
         fdb.fn = "stop",
         in.shiny = in_shiny,
         with.time = FALSE
         )
       
      
    } else if(n_nas < n_nas_new){
      
      new_nas <- n_nas_new - n_nas
      
      msg <- 
        glue::glue("Converting variable '{ref}' to numeric resulted in {new_nas} NAs")
      
      confuns::give_feedback(
        msg = msg,
        fdb.fn = "warning",
        in.shiny = in_shiny,
        with.time = FALSE
        )
      
    }
    
  }
  
  return(var)
  
}


# variable type descriptions ----------------------------------------------

general_grouping_variable_info <- 
  list(
    descr = "Variables that contain categorical data that can be used to group observations.",
    requirements = req_grouping_var, 
    name_in_app = "Grouping Data",
    name_in_cypro = "grouping_variables",
    name_in_example = base::character(0), # can contain several names,
    check_candidate = is_grouping_candidate,
    check_content = check_and_convert_grouping_var
  )

general_numeric_variable_info <- 
  list(
    descr = "Variables that contain numeric data that can be used for analysis.",
    requirements = req_numeric_var, 
    name_in_app = "Numeric Data",
    name_in_cypro = "numeric_variables",
    name_in_example = base::character(0), # can contain several names,
    check_candidate = is_numeric_candidate,
    check_content = check_and_convert_numeric_var
  )



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


# analysis modules --------------------------------------------------------



# data loading reading modules --------------------------------------------

# additional variables
module_additional_data_variables <- 
  list(
    exp_type = "both",
    helper_text = "none needed",
    pretty_name = "none needed",
    name = "additional_data_variables",
    variables = list(
      grouping_variable_info = general_grouping_variable_info,
      numeric_variable_info = general_numeric_variable_info
    )
  )

# module collection -------------------------------------------------------



