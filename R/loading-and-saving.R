#' @title Save cypro object
#'
#' @inherit check_object params
#'
#' @export

saveCyproObject <- function(object, verbose = TRUE){
  
  dir <- object@information$storage_directory

  if(!base::is.character(dir)){
    
    base::stop("No storage directory set. Use `setStorageDirectory()` first.")
    
  }
  
  confuns::give_feedback(
    msg = glue::glue("Saving cypro object under '{dir}'."), 
    verbose = verbose
  )
    
  base::saveRDS(object, file = dir)

  confuns::give_feedback(msg = "Done.", verbose = verbose)
    
}