



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
                           rois_per_well = 1,
                           n_phases = 0){
  
  confuns::are_values(c("well_plate_name", "well_plate_type"), mode = "character")
  
  confuns::check_one_of(
    input = well_plate_type, 
    against = well_plate_info$type
  )
  
  # row- and column number of current well plate
  well_plate_used <- 
    dplyr::filter(well_plate_info, type == {{well_plate_type}})
  
  # data.frame (obs => well)
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
      cell_line = NA,
      condition = NA, 
      cell_line = base::as.character(cell_line), 
      condition = base::as.character(condition),
      well_plate_type = {{well_plate_type}}, 
      rois_per_well = {{rois_per_well}},
      selected = FALSE
    ) %>% 
    dplyr::select(
      well_plate_name, well_plate_type, dplyr::everything()
    )
  
  base::attr(x = layout_df, which = "class") <- c("layout_df", base::class(layout_df))
  
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