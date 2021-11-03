


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