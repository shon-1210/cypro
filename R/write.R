

#' @param variable An S4 object of class \code{AssignableVariable}.

write_variable_description <- function(variable){
  
  description <- 
    c(
      variable@descr_variable, 
      "",
      glue::glue(
        "Within cypro and all its functions the variable selected as ",
        "'{variable@name_in_app}' is renamed and referred to as ",
        "'{variable@name_in_cypro}'."
      ), 
      "",
      stringr::str_c("Requirement: ", variable@descr_requirements)
    )
  
  return(description)
  
}



write_variable_problem <- function(problem, name_in_app){
  
  if(FALSE){
    
    glue::glue("Problem with input variable assigned to '{name_in_app}': {problem}")
    
  }
  
  return(problem)
  
}


