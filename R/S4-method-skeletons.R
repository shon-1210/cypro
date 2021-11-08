


# time lapse
get_frame_time_seq <- function(object){
  
  check_object(object)
  assign_default(object)
  
  getTracksDf(object) %>% 
    dplyr::arrange(frame_num) %>% 
    dplyr::pull(var = "frame_time") %>% 
    base::unique()
  
}




# screening and simple time lapse
get_missing_values_df <- function(object, ...){
  
  check_object(object)
  
  na_count_df <- object@qcheck$na_count
  
  return(na_count_df)
  
}

# multiple phases
get_missing_values_df_mp <- function(object, phase = NULL,...){
  
  check_object(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  na_count_df <- object@qcheck$na_count[[phase]]
  
  return(na_count_df)
  
}




