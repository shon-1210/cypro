

#' @title Return css status
#' 
css_status <- function(evaluate, case_true = "success", case_false = "warning"){
  
  if(base::isTRUE(evaluate)){
    
    return(case_true)
    
  } else {
    
    return(case_false)
    
  }
  
}


hovered_well_info_shiny <- function(df){
  
  UseMethod(generic = "hovered_well_info_shiny", object = df)
  
}


#' @title Create printable well info df
#'
hovered_well_info_shiny <- function(df){
  
  dplyr::select(
    .data = df,
    `Well:` = well,
    `Status:` = info_status,
    `Cell Line:` = cell_line,
    `Condition:` = condition
  ) 
  
}


#' @rdname hovered_well_info_shiny
#' @export
hovered_well_info_shiny_mp <- function(df){
  
  n_phases <- base::attr(df, "n_phases")
  
  c_df <- df$condition[[1]]
  
  c_vec <- 
    base::as.character(c_df[1,]) %>%
    stringr::str_c(1:n_phases, .,sep = ".") %>% 
    confuns::scollapse(sep = ", ", last = " and ")
  
  df_res <- 
    dplyr::select(
      .data = df,
      `Well:` = well,
      `Status:` = info_status,
      `Cell Line:` = cell_line
    ) %>% 
    dplyr::mutate(
      `Conditions:` = {{c_vec}}
    )
  
  return(df_res)
  
}


#' @title Visualize the well plate
#'
#' @param wp_df A well-plate data.frame.
#' @param selected_wells_df A subsetted well-plate data.frame or NULL.
#' @param aes_fill Character value. Variable of \code{wp_df} to map on the 
#' fill-aesthetic.
#' @param aes_color Character value. Variable of \code{wp_df} to map on the 
#' color-aesthetic.
#' @param pt_size Numeric value. Size of points that represent the wells. 
#' @param pt_stroke Numeric value. Size of border that indicates what has been 
#' mapped on the color-aesthetic.
#' @param border Numeric value. Distance between the outer wells and the plate's
#' margin.
#'
#' @inherit ggplot_return return
#'

plot_well_plate_shiny <- function(wp_df,
                                  selected_wells_df = NULL,
                                  aes_fill,
                                  fill_values = NULL, 
                                  fill_guide = FALSE,
                                  aes_color,
                                  color_values,
                                  color_guide = TRUE,
                                  pt_size = 13.5,
                                  pt_stroke = 2,
                                  border = 0.75){
  
  limit_x <- base::max(wp_df$col_num) + border
  limit_y <- base::max(wp_df$row_num) + border
  
  if(base::is.data.frame(selected_wells_df)){
    
    geom_point_add_on <-  
      ggplot2::geom_point( data = selected_wells_df, fill = "red",
                           size = pt_size, shape = 21, alpha = 1,
                           stroke = pt_stroke ) 
    
  } else {
    
    geom_point_add_on <- NULL
    
  }
  
  if(base::is.null(fill_values)){
    
    fill_add_on <-
      confuns::scale_color_add_on(
        aes = "fill",
        variable = wp_df[[aes_fill]],
        clrp.adjust = c("unknown" = "lightgrey", "unknown & unknown" = "lightgrey"),
        clrp = "milo",
        guide = FALSE
      ) 
    
  } else {
    
    fill_add_on <- 
      ggplot2::scale_fill_manual(values = fill_values, drop = FALSE, guide = FALSE) 
    
  }
  
  if(base::isTRUE(color_guide)){
    
    color_guide <- ggplot2::guide_legend(override.aes = list(size = 15, shape = 21))
    
  } 
  
  if(base::isTRUE(fill_guide)){
    
    fill_guide <- ggplot2::guide_legend(override.aes = list(size = 15, shape = 21))
    
  } 
  
  
  # plot output
  ggplot2::ggplot(data = wp_df, mapping = ggplot2::aes(x = col_num,y = row_num)) + 
    ggplot2::geom_point(
      data = wp_df, 
      mapping = ggplot2::aes(fill = .data[[aes_fill]], color = .data[[aes_color]]),
      size = pt_size, shape = 21, alpha = 0.9, stroke = pt_stroke, 
    ) + 
    geom_point_add_on +
    ggplot2::geom_text(mapping = ggplot2::aes(label = well)) +
    ggforce::geom_mark_rect(
      mapping = ggplot2::aes(x = col_num, y = row_num, color = group), 
      color = "black", size = 1, expand = ggplot2::unit(15, "mm")
    ) +
    ggplot2::scale_x_continuous(limits = c(-border, limit_x)) +
    ggplot2::scale_y_reverse(limits = c(border + limit_y, -border)) +
    ggplot2::theme_void() +
    #ggplot2::scale_color_manual(values = color_values, drop = FALSE) +
    confuns::scale_color_add_on(aes = "color", variable = wp_df[[aes_color]], clrp.adjust = color_values, clrp = "milo") + 
    fill_add_on +
    ggplot2::guides(
      color = color_guide, 
      fill = fill_guide
    )
  
  
}




#' @title Quality check histogram
#'
#' @param track_summary_df Output of \code{quality_check_summary()}
#' @param aes_x Character value. 
#' @param aes_fill Character value.
#' @param lab_x Character value.
#' @param lab_fill Character value.

plot_qc_histogram_shiny <- function(track_summary_df, 
                                    aes_x = "skipped_meas", 
                                    lab_x = "Measurements",
                                    legend_position = "none"){
  
  labels_breaks <-
    dplyr::pull(track_summary_df, var = {{aes_x}}) %>% 
    base::unique()
  
  ggplot2::ggplot(data = track_summary_df, mapping = ggplot2::aes(x = .data[[aes_x]])) + 
    ggplot2::geom_histogram(position = "stack", color = "black", binwidth = 1) + 
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(color = "grey"), 
      axis.text.x = ggplot2::element_text(vjust = 5), 
      plot.title = ggplot2::element_text(face = "bold", size = 15), 
      legend.position = "none"
    ) + 
    ggplot2::scale_x_continuous(labels = labels_breaks, breaks = labels_breaks) +
    ggplot2::labs(x = lab_x, y = NULL)
  
}


#' @title Quality check barplot
#'
#' @param df A data.frame 

plot_qc_barplot_shiny <- function(df, aes_x, aes_fill, bar_position){
  
  ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = forcats::fct_infreq(.data[[aes_x]]))) + 
    ggplot2::geom_bar(mapping = ggplot2::aes(fill = .data[[aes_fill]]),
                      color = "black", position = bar_position) + 
    ggplot2::theme_minimal() + 
    ggplot2::labs(x = NULL, y = NULL, fill = NULL,
                  subtitle = stringr::str_c("n = ", base::nrow(df)))
  
}

#' @title Summarize tracking quality
#'
#' @param track_df A track data.frame

quality_check_summary_shiny <- function(track_df){
  
  dplyr::group_by(.data = track_df, cell_id) %>% 
    dplyr::filter(!frame_added) %>% 
    dplyr::summarise(
      last_meas = base::max(frame_num), 
      first_meas = base::min(frame_num), 
      total_meas = dplyr::n(), 
      skipped_meas = base::length(first_meas:last_meas) - total_meas
    )
  
}


#' @title Read example file 
#' 
#' @description Reads in the example table. 
#' 
read_example_file_shiny <- function(directory){
  
  if(stringr::str_detect(string = directory, pattern = ".csv$")){
    
    df <- 
      base::suppressMessages({
        
        base::suppressWarnings({
          
          readr::read_csv(file = directory)
          
        })
        
      })
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".xlsx$")){
    
    df <- readxl::read_xlsx(path = directory, sheet = 1)
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".xls")){
    
    df <- readxl::read_xls(path = directory, sheet = 1)
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".txt")){
    
    df <- utils::read.delim(file = directory, header = TRUE)
    
  }
  
  if(tibble::has_rownames(df)){
    
    df <- tibble::rownames_to_column(df, var = "rownames")
    
  }
  
  df <- dplyr::mutate_if(df, .predicate = is.numeric, .funs = base::round, digits = 2)  
  
  return(df)
  
}


#' @title Show shiny - notifications
#'
#' @param in_shiny Logical value. 
#' @param ui Given to \code{shiny::showNotification()}.
#' @param type Given to \code{shiny::showNotification()}.
#' @param ... More arguments given \code{shiny::showNotification()}.
#'
#' @return A shiny notification.

shiny_fdb <- function(in_shiny = TRUE, ui, type = "message", ...){
  
  if(base::isTRUE(in_shiny)){
    
    shiny::showNotification(ui = ui, type = type, ...)
    
  }
  
}