


# c -----------------------------------------------------------------------

make_cell_id <- function(cell_id, well_plate_index, well, roi){
  
  stringr::str_c(cell_id,well_plate_index, well, roi, sep = "|")
  
}



# f -----------------------------------------------------------------------

make_frame_vec <- function(n_frames, ref = "Frame"){
  
  confuns::is_value(n_frames, mode = "numeric")
  
  english::ordinal(x = 1:n_frames) %>% 
    confuns::make_capital_letters(collapse.with = NULL) %>% 
    stringr::str_c(., "Frame", sep = " ")
  
}



# t -----------------------------------------------------------------------


make_time_frame_vec <- function(time_vec, frame_vec){
  
  stringr::str_c(time_vec, frame_vec, sep = " / ")
  
}

make_time_vec <- function(n_frames, interval, interval_unit){
  
  stringr::str_c(1:(n_frames * interval), interval_unit, sep = " ")
  
}


