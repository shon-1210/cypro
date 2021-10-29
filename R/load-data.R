


load_data <- function(object,
                      use_example = FALSE,
                      var_well_plate = "well_plate",
                      var_well = "well", 
                      var_well_roi = "well_roi",
                      verbose = TRUE){
  
  if(base::isTRUE(use_example)){
    
    confuns::give_feedback(msg = "Using example data.frame.", verbose = verbose)
    
    # check input
    
    confuns::are_values(c("var_well_plate", "var_well", "var_well_roi"), mode = "character")
    
    input_freq <- base::table(c(var_well_plate, var_well, var_well_roi))
      
    if(base::any(input_freq > 1)){
      
      stop("Input for 'var_'-arguments must not overlap.")
      
    }
    
    known_vars <-
      getVariableAssignment(object, clean = TRUE, flatten = TRUE) %>% 
      base::unname()
    
    df <-
      getExampleDf(object) %>% 
      dplyr::select(-dplyr::all_of(known_vars))
    
    remaining_vars <- base::colnames(df)
    
    if(nWellPlates(object) != 1){
      
      confuns::check_one_of(input = var_well_plate, against = remaining_vars)
      
    }
    
    confuns::check_one_of(input = var_well, against = remaining_vars)
    
    confuns::check_one_of(input = var_well_roi, against = remaining_vars)
    
    
    
  }
  
  
}