



# module blueprint --------------------------------------------------------

# each known variable in cypro has it's own list of information & tests
# several modules are allowed to need one and the same variable 
module_variables_blueprint <- 
  list(
    descr = "What information does the variable contain?",
    name_in_cypro = "blueprint",
    name_in_example = "X Blueprint with empty space", # some string selected in shiny app
    relevance = c("computable", "needed", "optional"), # one of these three
    test = function(var){ return(TRUE) } # a function making sure the var matches requirements 
  )

module_blueprint <- 
  list(
    pretty_name = "Blueprint & Co", 
    name = "blueprint", 
    helper_text = "An example module input that allows to analyze stuff.", 
    variables = list(module_variables_blueprint = module_variables_blueprint)
  )


# module variable requirements --------------------------------------------

numeric_cypro_var <- "Must be numeric or convertable to numeric."


# module variable tests ---------------------------------------------------

character_cypro_test <- function(var, ref){
  
  n_nas <- base::is.na(var) %>% base::sum()
  
  l_var <- base::length(l_var)
  
  if(!base::is.character(var)){
    
    var <- base::as.character(var)
    
    n_nas_new <- base::is.na(var) %>% base::sum()
    
    if(n_nas_new == l_var){
      
      stop(glue::glue("Converting variable '{ref}' to character resulted in only NAs. Invalid variable assignment."))
      
    } else if(n_nas < n_nas_new){
      
      new_nas <- n_nas_new - n_nas
      
      warning(glue::glue("Converting variable '{ref}' to character resulted in {new_nas} NAs."))
      
    }
    
  }
  
  return(var)
  
}

numeric_cypro_test <- function(var, ref){
  
  n_nas <- base::is.na(var) %>% base::sum()
  
  l_var <- base::length(var)
  
  if(!base::is.numeric(var)){
    
    if(base::is.factor(var)){
      
      # convert to character first in case of unordered levels
      var <- base::as.character(var)
      
    }
    
    var <- base::as.numeric(var)
    
    n_nas_new <- base::is.na(var) %>% base::sum()
    
    if(n_nas_new == l_var){
      
      stop(glue::glue("Converting variable '{ref}' to numeric resulted in only NAs. Invalid variable assignment."))
      
    } else if(n_nas < n_nas_new){
      
      new_nas <- n_nas_new - n_nas
      
      warning(glue::glue("Converting variable '{ref}' to numeric resulted in {new_nas} NAs"))
      
    }
    
  }
  
  return(var)
  
}


# identifier variables ----------------------------------------------------

var_cell_id <- 
  list(
    descr = "Variable that identifies cells throughout all images by number or by a character ID.", 
    requirements = "Numeric or character as long as every cell has a unique value.", 
    name_in_cypro = "cell_id", 
    name_in_example = base:::character(1), 
    relevance = "needed",
    test = function(var, ref, timelapse){
      
      if(base::isTRUE(timelapse)){
        
        # check in check_var_frame_cell_id() function together with frame
        return(var)
        
      } else {
        
        n_nas <- base::is.na(var) %>% base::sum()
        
        l_var <- base::length(var)
        
        n_ids <- dplyr::n_distinct(var)
        
        if(l_var > n_ids && n_nas == 0){
          
          stop("There are more cells in your example file than there are unique ids in variable '{ref}'.",
               "Invalid variable assignment for Cell ID.")
          
        } else {
          
          return(var)
          
        }
        
      }
      
    }
  )


var_frame <- 
  list(
    descr = "Variable that identifies each frame in an image stack by number.", 
    requirements = "Numeric or convertable to numeric.", 
    name_in_cypro = "frame", 
    name_in_example = base:::character(1),
    relevance = "needed",
    test = function(var, ref){
      
      # check in check_var_frame_cell_id() function together with cell_id
      return(var)
      
    }
  )


check_var_frame_cell_id <- function(var, ref, var_cell_id, ref_cell_id){
  
  numeric_cypro_test(var = var, ref = ref)
  
  character_cypro_test(var = var_cell_id, ref = ref_cell_id)
  
  identifier_df <- base::data.frame(cell_id = var_cell_id, frame = var)
  
  n_obs <- base::nrow(identifier_df)
  
  n_unique_obs <- 
    dplyr::distinct(identifier_df) %>% 
    base::nrow()
  
  if(n_obs > n_unique_obs){
    
    stop("Frame-variable {ref} and Cell ID-variable {ref_cell_id} do not", 
         "uniquely identify each observation. Invalid variable assignment.")
    
  } 
  
  return(var)
  
}

# module variables  -------------------------------------------------------

var_dflp <- 
  list(
    descr = "The distance a cell has travelled from last frame to current one.", 
    requirements = numeric_cypro_var, 
    name_in_cypro = "dflp", 
    name_in_example = base::character(1), 
    relevance = "computable",
    test = numeric_cypro_test
  )


var_dfo <- 
  list(
    descr = "The distance a cell has travelled from first frame to current one.", 
    requirements = numeric_cypro_var, 
    name_in_cypro = "dflp", 
    name_in_example = base::character(1), 
    relevance = "computable",
    test = numeric_cypro_test
  )


var_speed <- 
  list(
    descr = "The distance from the last point divided by the time elapsed between the acquisition of two subsequent frames.", 
    requirements = numeric_cypro_var, 
    name_in_cypro = "dflp", 
    name_in_example = base::character(1), 
    relevance = "computable",
    test = numeric_cypro_test
  )


var_x_coords <- 
  list(
    descr = "Refers to the x-coordinates of a cell.",
    requirements = numeric_cypro_var,
    name_in_cypro = "x_coords", 
    name_in_example = base:::character(1), 
    relevance = "needed",
    test = numeric_cypro_test
  )


var_y_coords <- 
  list(
    descr = "Refers to the y-coordinates of a cell.",
    requirements = numeric_cypro_var,
    name_in_cypro = "y_coords", 
    name_in_example = base:::character(1), 
    relevance = "needed",
    test = numeric_cypro_test
  )


# modules one by one  -----------------------------------------------------

module_blueprint <- 
  list(
    pretty_name = "Blueprint & Co", 
    name = "blueprint", 
    helper_text = "An example module input that allows to analyze stuff.", 
    variables = list(module_variables_blueprint = module_variables_blueprint)
  )

module_migration <- 
  list(helper_text = "Analyze and quantify locomotion of cells in time lapse experiments.", 
       name = "migration", 
       pretty_name = "Cellular Migration", 
       variables = list(
         x_coords = var_x_coords, 
         y_coords = var_y_coords, 
         dfo = var_dfo, 
         dflp = var_dflp, 
         speed = var_speed
       ))













# module collection -------------------------------------------------------

cypro_modules <- 
  list(
    
  )