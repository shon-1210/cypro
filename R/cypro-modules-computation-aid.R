

# Computation of computable variables -------------------------------------

# info extraction

get_computable_variable_names <- function(object, module){
  
  text <- stringr::str_c("module", module, sep = "_")
  
  module_info <- 
    base::parse(text = text) %>%
    base::eval()
  
  computable_vars <- 
    module_info$variables %>% 
    purrr::keep(.p = ~ .x$relevance == "computable") %>% 
    base::names()
  
  return(computable_vars)
  
}

get_denoted_variable_names <- function(object, module, verbose = FALSE){
  
  module_list <- object@modules[[module]]
  
  denoted_vars <- 
    module_list[["variable_denotation"]] %>% 
    purrr::keep(.p = ~ .x != "not selected") %>% 
    base::names()
  
  return(denoted_vars)
  
}


get_not_denoted_variable_names <- function(object, module, verbose = FALSE){

  module_list <- object@modules[[module]]
  
  not_denoted_vars <- 
    module_list[["variable_denotation"]] %>% 
    purrr::keep(.p = ~ .x == "not selected") %>% 
    base::names()
  
  if(base::length(not_denoted_vars) == 0){
    
    msg <- glue::glue("All variables have been denoted for module '{module}'.")
    
    confuns::give_feedback(msg = msg, verbose = verbose, with.time = FALSE)
    
  }
  
  return(not_denoted_vars)
  
}


get_used_module_names <- function(object){
  
  object@modules %>% base::names()
  
}


get_variable_names_to_be_computed <- function(object, module){
  
  computable <-
    get_computable_variable_names(object, module = module)
  
  not_denoted <-
    get_not_denoted_variable_names(object, module = module)
  
  if(base::length(not_denoted) >= 1){
    
    vars_to_compute <- 
      confuns::vselect(computable, contains(not_denoted))
    
  } else {
    
    vars_to_compute <- base::character(0)
        
  }
  
  return(vars_to_compute)
  
}



