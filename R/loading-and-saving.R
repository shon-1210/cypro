#' @title Save cypro object
#'
#' @inherit check_object params
#'
#' @export

saveCyproObject <- function(object, verbose = TRUE){
  
  
  if(base::is.character(object@information$directory_cto)){
    
    object@information$storage_directory <- 
      object@information$directory_cto
    
    object@information$directory_cto <- NULL
    
  }
  
  dir <- object@information$storage_directory

  confuns::give_feedback(
    msg = glue::glue("Saving cypro object under '{dir}'."), 
    verbose = verbose
  )
    
  base::saveRDS(object, file = dir)

  confuns::give_feedback(msg = "Done.", verbose = verbose)
    
}