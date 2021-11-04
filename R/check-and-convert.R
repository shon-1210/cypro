


check_and_convert_grouping_var <- function(var, ref, ...){
  
  n_nas <- base::is.na(var) %>% base::sum()
  
  l_var <- base::length(var)
  
  res <- list(var = var, problem = NULL)
  
  if(base::is.null(var)){
    
    res$var <- FALSE
    res$problem <- glue::glue( "Variable '{ref}' is missing." )
    
  } else if(!base::is.character(var)){
    
    var <- base::as.character(var)
    
    n_nas_new <- base::is.na(var) %>% base::sum()
    
    if(n_nas_new == l_var){
      
      res$var <- FALSE
      
      res$problem <-
        glue::glue(
          "Attempt to convert variable '{ref}' to type 'categorical' resulted in only NAs. ",
          "Variable is invalid."
        )
      
    } else if(n_nas < n_nas_new){
      
      new_nas <- n_nas_new - n_nas
      
      res$var <- FALSE
      res$problem <- glue::glue("Converting variable '{ref}' type 'categorical' resulted in {new_nas} NAs")
      
    } else {
      
      res$var <- var
      
    }
    
  }
  
  return(res)
  
}

check_and_convert_numeric_var <- function(var, ref, ...){
  
  n_nas <- base::is.na(var) %>% base::sum()
  
  l_var <- base::length(var)
  
  res <- list(var = var, problem = NULL)
  
  if(base::is.null(var)){
    
    res$var <- FALSE
    res$problem <- glue::glue( "Variable '{ref}' is missing." )
    
  } else if(!base::is.numeric(var)){
    
    if(base::is.factor(var)){
      
      # convert to character first in case of unordered levels
      var <- base::as.character(var)
      
    }
    
    var <- base::as.numeric(var)
    
    n_nas_new <- base::is.na(var) %>% base::sum()
    
    if(n_nas_new == l_var){
      
      res$var <- FALSE
      
      res$problem <-
        glue::glue(
          "Attempt to convert variable '{ref}' to type 'numeric' resulted in only NAs. ",
          "Variable is invalid."
        )
      
    } else if(n_nas < n_nas_new){
      
      new_nas <- n_nas_new - n_nas
      
      res$var <- FALSE
      res$problem <- glue::glue("Converting variable '{ref}' to numeric resulted in {new_nas} NAs")
      
    } else {
      
      res$var <- var
    }
    
  }
  
  return(res)
  
}

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