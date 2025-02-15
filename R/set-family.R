

#' @title Set analysis objects
#' 
#' @description Sets a convenient object and empties its data slot. 
#'
#' @inherit argument_dummy params 
#' @inherit updated_object return
#' 
#' @export
#'
setClusterConv <- function(object, cluster_object, method, variable_set, phase){
  
  cluster_object@data <- matrix()
  
  if(multiplePhases(object)){
    
    object@analysis$clustering[[method]][[variable_set]][[phase]] <- 
      cluster_object
    
  } else {
    
    object@analysis$clustering[[method]][[variable_set]] <- 
      cluster_object
    
  }
  
  base::return(object)
  
}

#' @rdname setClusterConv
#' @export
setDimRedConv <- function(object, dim_red_object, method, variable_set, phase){
  
  dim_red_object@data <- matrix()
  
  if(multiplePhases(object)){
    
    object@analysis$dim_red[[method]][[variable_set]][[phase]] <- 
      dim_red_object
    
  } else {
    
    object@analysis$dim_red[[method]][[variable_set]] <- 
      dim_red_object
    
  }
  
  base::return(object)
  
}

#' @rdname setClusterConv
#' @export
setCorrConv <- function(object, corr_object, variable_set, phase){

  corr_object@variables_discrete <- base::character()
    
  corr_object@data <- base::matrix()
  corr_object@meta <- dplyr::select(corr_object@meta, key)
  
  if(multiplePhases(object)){
    
    object@analysis$correlation[[variable_set]][[phase]] <- corr_object
    
  } else {
    
    object@analysis$correlation[[variable_set]] <- corr_object
    
  }
  
  base::return(object)
  
}


#' @title Set cell data.frame
#' 
#' @inherit argument_dummy params 
#' @inherit updated_object return
#' 
#' 
setCellDf <- function(object, slot, df, phase){
  
  if(multiplePhases(object)){
    
    object@cdata[[slot]][[phase]] <- df
    
  } else {
    
    object@cdata[[slot]] <- df
    
  }
  
  base::return(object)
  
}


#' @title Set cypro default
#' 
#' @inherit argument_dummy params 
#' @inherit updated_object return
#' 
#' @export
setDefaultInstructions <- function(object){
  
  object@default <- default_list
  
  return(object)
  
}



#' @title Set default storage directory
#' 
#' @inherit argument_dummy params 
#' @inherit updated_object return
#' 
#' @param directory Character value. The directory under which 
#' the object is supposed to be stored via \code{saveCyproObject()}.
#'
setStorageDirectory <- function(object, directory){
  
  confuns::is_value(directory, "character")
  
  if(!stringr::str_detect(directory, pattern = "\\.RDS$")){
    
    base::stop("Input directory has to end with '.RDS'")
    
  }
  
  object@information$storage_directory <- directory
  
  base::return(object)  
  
}




