

get_conditions <- function(object, ...){
  
  check_object(object)
  assign_default(object)
  
  getMetaDf(object) %>%
    dplyr::pull(cell_line) %>%
    base::levels()
  
}






setImageDirectories <- function(object,
                                folder,
                                directories,
                                rgx_well_plate, 
                                rgx_well, 
                                rgx_roi){}




expected_number_of_files <- function(well_plate, object){
  
  ld_fn <- suggestLoadingFunction(object)
  
  if(ld_fn == "by_roi"){
    
    n_files <- nRois(well_plate) * nWells(well_plate)
    
  } else if(ld_fn == "by_well"){
    
    n_files <- nWells(well_plate)
    
  } else {
    
    n_files <- 1
    
  }
  
  return(n_files)
  
}













