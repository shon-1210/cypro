

compute_module_variables <- function(track_df, object, verbose){
  
  used_modules <- get_used_module_names(object)
  
  track_df <- dplyr::group_by(track_df, cell_id)
  
  for(i in base::seq_along(used_modules)){
    
    used_module <- used_modules[i]
    
    module_info <- 
      stringr::str_c("module", used_module, sep = "_") %>% 
      base::parse(text = .) %>% 
      base::eval()
    
    computable_vars <-
      get_computable_variable_names(object, module = used_module)
    
    vars_to_compute <- 
      get_variable_names_to_be_computed(object, module = used_module)
    
    if(base::length(vars_to_compute) >= 1){
      
      confuns::give_feedback(
        msg = glue::glue("Module: '{module_info$pretty_name}'"),
        verbose = verbose
        )
      
      for(var_to_compute in module_info$computation_order){
        
        if(var_to_compute %in% vars_to_compute){
          
          variable_info <- module_info$variables[[var_to_compute]]
          
          var_name_in_app <- variable_info$name_in_app
            
          confuns::give_feedback(
            msg = glue::glue("Variable: '{var_name_in_app}' ('{var_to_compute}')"), 
            verbose = verbose
          )
          
          fn <- variable_info$compute_with
          
          args <- list(track_df = track_df, object = object)
          
          args <- c(args, variable_info$compute_with_args)
          
          track_df <- rlang::invoke(.fn = fn, .args = args)
          
        }
        
      }
      
      replace_val <- module_info$replace_na_first_frame
      
      for(cvar in computable_vars){
        
        track_df <- 
          dplyr::mutate(
            .data = track_df, 
            {{cvar}} := dplyr::case_when(
              frame_num == 1 & base::is.na(!!rlang::sym(cvar)) ~ {{replace_val}}, 
              TRUE ~ !!rlang::sym(cvar)
            )
          )
        
      }
      
    }
    
  }
  
  return(track_df)
  
}





