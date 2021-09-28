


# Z-score -----------------------------------------------------------------

compute_zscore <- function(df){
  
  df <-
    dplyr::mutate(
      .data = df,
      dplyr::across(.cols = where(base::is.numeric) & -dplyr::any_of(x = c("x_coords", "y_coords")),
                    .fns = function(x){
      
      res <- (x - base::mean(x)) / stats::sd(x)
      
      return(res)
      
    }))

  return(df)
  
}

#' @title Normalize data with zscore
#' 
#' @description Normalizes numeric data variables (apart from x-/y- coordinates)
#' by computing their zscore. In case of timelapse experiments the stat data.frame is affected. 
#'
#' @param object 
#' @param across Character value or NULL. If character, the normalization 
#' is conducted separately for all groups of the grouping variable.
#' Must be a variable from the well plate data.frame. 
#'
#' @return A cypro object. 
#' @export
#'
normalizeZscore <- function(object, across = NULL, verbose = TRUE){
  
  check_object(object)
  
  if(isNormalizedWithZscore(object)){
    
    stop("Data of this object has already been normalized with method 'zscore'.")
    
  }
  
  if(base::is.character(across)){
    
    confuns::check_one_of(
      input = across, 
      against = well_plate_vars
    )
  
    ref_across <- glue::glue(" across '{across}'")
      
  } else {
    
    ref_across <- ""
    
  }
  
  msg <- glue::glue("Normalizing variables{ref_across}.")
  
  confuns::give_feedback(msg = msg, verbose = verbose)
  
  if(isTimeLapseExp(object)){
    
    if(multiplePhases(object)){
      
      object@cdata$stats <- 
        purrr::map(.x = getPhases(object), function(phase){
          
          stats_df <- 
            getStatsDf(
              object = object,
              with_well_plate = TRUE,
              drop_na = FALSE, 
              phase = phase
              )
          
          if(base::is.character(across)){
            
            stats_df <- 
              purrr::map_df(
                .x = getGroupNames(object, grouping_variable = across, phase = NULL),
                .f = function(group){
                  
                  res_df <- 
                    dplyr::filter(stats_df, !!rlang::sym(across) == {{group}}) %>% 
                    compute_zscore()
                  
                  return(res_df)
                  
                }
                )
            
          }
          
          stats_df <- dplyr::select(stats_df, -dplyr::any_of(well_plate_vars))
          
          return(stats_df)
          
        }) %>% 
        purrr::set_names(getPhases(object))
      
    } else {
      
      df <- 
        getStatsDf(
          object = object,
          with_well_plate = TRUE,
          drop_na = FALSE
        )
      
      if(base::is.character(across)){
        
        df <- 
          purrr::map_df(
            .x = getGroupNames(object, grouping_variable = across, phase = NULL),
            .f = function(group){
              
              res_df <- 
                dplyr::filter(df, !!rlang::sym(across) == {{group}}) %>% 
                compute_zscore()
              
              return(res_df)
              
            }
          )
        
      } else {
        
        df <- compute_zscore(df = df)
        
      }
      
      df <- dplyr::select(df, -dplyr::any_of(well_plate_vars))
      
      object <- setCellDf(object, slot = "stats", df = df)
      
    }
    
  } else {
    
    df <- getDataFrame(object, with_well_plate = TRUE)
    
    if(base::is.character(across)){
      
      df <- 
        purrr::map_df(
          .x = getGroupNames(object, grouping_variable = across, phase = NULL),
          .f = function(group){
            
            res_df <- 
              dplyr::filter(df, !!rlang::sym(across) == {{group}}) %>% 
              compute_zscore()
            
            return(res_df)
            
          }
        )
      
    } else {
      
      df <- compute_zscore(df)
      
    }
    
    df <- dplyr::select(df, -dplyr::any_of(well_plate_vars))
    
    object@cdata$tracks <- df
    
  }
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  object@information$normalization$zscore <- list(across = across, conducted = base::Sys.time())
  
  return(object)
  
}



