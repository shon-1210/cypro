




#' @title Coerce to \code{layout_df} data.frame
#' 
#' @description Converts input data.frame to layout data.frames.
#'
#' @inherit argument_dummy params
#' @param df A data.frame.
#'
#' @return The input data.frame with adjusted attributes.
#' @export
#'

as_layout_df <- function(df, well_plate_type){
  
  UseMethod(generic = "as_layout_df")
  
}

#' @rdname as_layout_df
#' @export
as_layout_df_mp <- function(df, well_plate_type, n_phases){
  
  UseMethod(generic = "as_layout_df_mp")
  
}

#' @rdname as_layout_df
#' @export
as_layout_df.data.frame <- function(df, well_plate_type){
  
  is_value(well_plate_type, mode = "character")
  
  check_one_of(
    input = well_plate_type, 
    against = well_plate_info$type
  )
  
  classes_df <- 
    base::class(df) %>% 
    vselect(-any_of("layout_df"))
  
  base::class(df) <- c("layout_df", classes_df)
  
  base::attr(x = df, which = "well_plate_type") <- well_plate_type
  
  return(df)
  
}

#' @rdname as_layout_df
#' @export
as_layout_df_mp.data.frame <- function(df, well_plate_type = NULL, n_phases){
  
  is_value(well_plate_type, mode = "character", skip.allow = TRUE, skip.val = NULL)
  is_value(n_phases, mode = "numeric")
  
  classes_df <- 
    base::class(df) %>% 
    vselect(-any_of("layout_df_mp"))
  
  base::class(df) <- c("layout_df_mp", classes_df)
  
  if(!base::is.character(well_plate_type)){
    
    wpt_set <- base::attr(x = layout_df, which = "well_plate_type")
    
    if(!base::is.character(wpt_set)){
      
      stop("Please specify argument 'well_plate_type'.")
      
    }
    
  } else {
    
    check_one_of(
      input = well_plate_type, 
      against = well_plate_info$type
    )
    
    base::attr(x = layout_df, which = "well_plate_type") <- well_plate_type
    
  }
  
  base::attr(df, which = "n_phases") <- n_phases
  
  return(df)
  
}