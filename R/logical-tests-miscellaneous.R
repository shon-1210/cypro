





# b -----------------------------------------------------------------------


byFile <- function(object){
  
  ld_fn <- suggestLoadingFunction(object)
  
  out <- ld_fn == "by_file"
  
  return(out)
  
}

byFolder <- function(object){
  
  ld_fn <- suggestLoadingFunction(object)
  
  out <- ld_fn %in% c("by_roi", "by_well")
  
  return(out)
  
}

byWellPlate <- function(object){
  
  ld_fn <- suggestLoadingFunction(object)
  
  out <- ld_fn == "by_well_plate"
  
  return(out)
  
}
