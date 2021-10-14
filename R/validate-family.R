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


validate_experiment_name <- function(exp_name, stop_if_false = FALSE){
  
  validate_simple_string(
    string = exp_name, 
    ref_invalid = "experiment name", 
    stop_if_false = stop_if_false
  )
  
}


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

