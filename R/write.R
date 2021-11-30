

write_basic_subset_text <- function(object){
  
  n_discarded <- base::length(object@ids_discarded)
  n_remaining <- base::length(object@ids_remaining)
  
  nd_text <- glue::glue("Discarded: {n_discarded}")
  nr_text <- glue::glue("Remaining: {n_remaining}")
  
  # -----
  
  reasoning <- object@reasoning
  reason_text <- glue::glue("Reasoning: {reasoning}")
  
  # -----
  
  new_text <- glue::glue("New name: {object@new_name}")
  parent_text <- glue::glue("Parent name: {object@parent_name}")
  
  out <- c(nd_text, nr_text, reason_text, new_text, parent_text)
  
  return(out)
  
}

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


