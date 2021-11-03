

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



































