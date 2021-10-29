

debug_assign <- function(x, as){
  
  if(base::isTRUE(debug)){
  
    base::assign(x = as, value = x, envir = .GlobalEnv)  
    
  }
  
}


debug_print <- function(x){
  
  if(base::isTRUE(debug)){
    
    print(x)
    
  }
  
}
