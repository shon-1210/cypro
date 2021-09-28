


#' @title Keep cypro object up to data
#' 
#' @description Ensures that the cypro object is constructed according
#' to the latest version of the package. 
#'
#' @inherit argument_dummy
#'
#' @return A cypro object whose architecture is up to date with the latest version.
#' @export
#'
updateCyproObject <- function(object){
  
  stopifnot(base::class(object) == "cypro")
  
  # include old Cypro objects
  all_slot_names <- methods::slotNames(object)
  
  content <- 
    purrr::map(all_slot_names, .f = purrr::safely(~ methods::slot(object, name = .x))) %>% 
    purrr::set_names(nm = all_slot_names)
  
  success <- 
    purrr::keep(.x = content, .p = ~ base::is.null(.x[["error"]]))
  
  success_names <- base::names(success)
  
  errors <- 
    purrr::keep(.x = content, .p = ~ !base::is.null(.x[["error"]]))
  
  error_names <- base::names(errors)
  
  version <- object@version
  
  # updating 
  if(base::identical(version, current_version)){
    
    stop("Updating not possible. Input object already has been updated to the newest version.")
    
  } else if(!base::is.list(version)){
    
    stop("Updating not possible. Please initiate the cypro object again.")
    
  }
  
  # patch 3 well_image => well_roi
  if(version$major == 0 & version$minor < 3){
    
    confuns::give_feedback(
      msg = "Patch 0.2.0 -> 0.3.0: Change in terminology - well_image => well_roi",
      verbose = TRUE
    )
    
    object@cdata$well_plate <-
      dplyr::rename(object@cdata$well_plate, well_roi = well_image)
    
    object@well_plates <- 
      purrr::map(object@well_plates, .f = function(wp_list){
        
        wp_list[["wp_df_eval"]] <-
          dplyr::rename(wp_list[["wp_df_eval"]], rois_per_well = ipw, well_roi_files = well_image_files)
        
        return(wp_list)
        
      })
    
    object@version$minor <- 3
    
  }
  
  
  # update slots 
  new_object <- initiateEmptyCyproObject()
  
  for(slot in success_names){
    
    methods::slot(new_object, name = slot) <- 
      methods::slot(object, name = slot)
    
    new_object@version <- current_version
    
  }
  
  confuns::give_feedback(msg = "Done.", verbose = TRUE)
  
  # return
  base::return(new_object)
  
}