




# E -----------------------------------------------------------------------


#' @title Create empty Cypro object
#' 
#' @description Wrapper for manual construction of the \code{Cypro} object. 
#'
#' @param class Character value. The Cypro class of the object. 
#' @param exp_name Character value. The name of the experiment.
#' @param ... Additional arguments given to the constructor function \code{methods::new()}.
#'
#' @return An object of the denoted class. 
#' @export
#'
createEmptyCyproObject <- function(class = "CyproTimeLapse", exp_name = "test", ...){
  
  confuns::check_one_of(
    input = class, 
    against = cypro_classes
  )
  
  confuns::is_value(x = exp_name, mode = "character")
  
  object <- methods::new(Class = class)
  
  object@experiment <- exp_name
  
  object@progress <- methods::new(Class = "Progress")
  
  if(stringr::str_detect(class, pattern = "TimeLapse")){
    
    object@modules <- cypro_modules$time_lapse
    
  } else if(class == "CyproScreening") {
    
    object@modules <- cypro_modules$screening
    
  } else {
    
    warning("Can not set module without information about the experiment design.")
    
  }
  
  return(object)
  
}



# L -----------------------------------------------------------------------

#' @title Create well plate layout
#' 
#' @description Creates a data.frame of class \code{layout_df} or \code{layout_df_mp}
#' representing the well plate layout.
#'
#' @param well_plate_name Character value. The name of the well plate. 
#' @param well_plate_type Character value. One of \emph{'2x3, '3x4', '4x6, '6x8', '8x12'}. Denotes
#' overall structure of the well plate.
#' @param phases Numeric value. Denotes the number of phases the 
#' experiment design includes. If not 0, The data.frame is set up 
#' to contain an additional column that keeps track of the changes in condition 
#' from phase to phase.  
#'
#' @return A data.frame of class \code{layout_df} or \code{layout_df_mp} if 
#' \code{phase} is numeric.
#' 
#' @export
#'
createLayoutDf <- function(well_plate_name = "wp", 
                           well_plate_type = "8x12",
                           n_rois = 1,
                           n_phases = 0){
  
  confuns::are_values(c("well_plate_name", "well_plate_type"), mode = "character")
  
  confuns::check_one_of(
    input = well_plate_type, 
    against = well_plate_info$type
  )
  
  well_plate_used <- 
    dplyr::filter(well_plate_info, type == {{well_plate_type}})
  
  layout_df <- 
    tidyr::expand_grid(
      row_num = 1:well_plate_used$rows, 
      col_num = 1:well_plate_used$cols
    ) %>%
    dplyr::mutate(
      well_plate_name = {{well_plate_name}},
      row_letter = base::LETTERS[row_num],
      well = stringr::str_c(row_letter, col_num, sep = ""), 
      info_status = base::factor(x = "Missing", levels = info_status_levels),
      cell_line = NA_character_,
      condition = NA_character_, 
      n_rois = {{n_rois}},
      n_files = 0,
      data_status = base::factor(x = "Incomplete", levels = data_status_levels),
      selected = FALSE
    ) %>% 
    dplyr::select(
      well_plate_name, dplyr::everything()
    )
  
  layout_df$roi_info <- purrr::map(.x = layout_df$well, .f = function(x){
    
    data.frame(well = x, roi = 1:n_rois) %>%  
      dplyr::mutate(well_roi = stringr::str_c(well, roi, sep = "_")) %>% 
      dplyr::select(-well) %>% 
      tibble::as_tibble()
    
  })
  
  # set primary attributes
  base::attr(x = layout_df, which = "class") <- c("layout_df", base::class(layout_df))
  
  base::attr(x = layout_df, which = "roi_info_vars") <- c("well_roi") 
  
  base::attr(x = layout_df, which = "well_plate_type") <- well_plate_type
  
  if(n_phases != 0){
    
    phases <- 1:n_phases
    
    phases_names <- 
      english::ordinal(x = base::seq_along(phases)) %>%
      stringr::str_c("condition", ., "phase", sep = "_")
    
    layout_df$condition <- 
      purrr::map(.x = base::seq_along(layout_df$well), 
                 .f = function(x){
                   
                   base::matrix(ncol = base::length(phases_names), nrow = 1) %>% 
                     base::as.data.frame() %>% 
                     magrittr::set_colnames(value = phases_names)
                   
                 })
    
    base::attr(x = layout_df, "n_phases") <- n_phases
    
    base::attr(x = layout_df, which = "class") <- c("layout_df_mp", base::class(layout_df))
    
  }
  
  return(layout_df)
  
}