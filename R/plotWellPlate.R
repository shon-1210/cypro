



#' @title Visualize well plates
#' 
#' @description Plots a well plate.
#'
#' @param object 
#' @param ... 
#'
#' @export
#'
setGeneric(name = "plotWellPlate", def = function(object,
                                                  clr_by = "file_status", 
                                                  plot_type = "tile",
                                                  well_alpha = 0.9,
                                                  well_size = 13.5, 
                                                  display_border = TRUE, 
                                                  border_clr = "black", 
                                                  border_size = 1,
                                                  display_edge = TRUE,
                                                  edge_clr = "black",
                                                  edge_size = 1,
                                                  display_labels = TRUE,
                                                  labels_clr = "black",
                                                  clrp = "milo",
                                                  clrp_adjust = NULL,
                                                  ncol = NULL,
                                                  nrow = NULL,
                                                  ...){
  
  standardGeneric(f = "plotWellPlate")
  
})



#' @rdname plotWellPlate
#' @export
setMethod(
  f = "plotWellPlate",
  signature = "layout_df",
  definition = function(object, 
                        clr_by = "file_status", 
                        plot_type = "tile",
                        well_alpha = 0.9,
                        well_size = 13.5, 
                        display_border = TRUE, 
                        border_clr = "black", 
                        border_size = 1,
                        display_edge = TRUE,
                        edge_clr = "black",
                        edge_size = 1,
                        display_labels = TRUE,
                        labels_clr = "black",
                        clrp = "milo",
                        clrp_adjust = NULL,
                        ncol = NULL,
                        nrow = NULL
                        ){
    
    layout_df <- object
    
    border <- 0.75
    limit_x <- base::max(layout_df$col_num) + border
    limit_y <- base::max(layout_df$row_num) + border
    
    clrp_adjust <- c(clrp_adjust, cypro_clrp_adjust[[clr_by]])
    
    # create base line ggplot
    base_line_plot <- 
      ggplot2::ggplot(data = layout_df, mapping = ggplot2::aes(x = col_num,y = row_num)) +
      ggplot2::theme_classic() + 
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(), 
        strip.background = ggplot2::element_rect(fill = ggplot2::alpha("steelblue", 0.75)),
        panel.background = ggplot2::element_blank(), 
        axis.line = ggplot2::element_blank(), 
        axis.ticks = ggplot2::element_blank(), 
        axis.text = ggplot2::element_blank()
      ) +
      ggplot2::scale_x_continuous(limits = c(-border, limit_x)) +
      ggplot2::scale_y_reverse(limits = c(border + limit_y, -border)) +
      ggplot2::labs(x = NULL, y = NULL, fill = make_pretty_name(clr_by)) + 
      scale_color_add_on(
        aes = "fill", 
        clrp = clrp, 
        clrp.adjust = clrp_adjust,
        variable = layout_df[[clr_by]]
      )
    
    
    # add facets
    if(dplyr::n_distinct(layout_df$well_plate_name) > 1){
      
      base_line_plot <- 
        base_line_plot + 
        ggplot2::facet_wrap(facets = . ~ well_plate_name, ncol = ncol, nrow = nrow)
      
    }
    
    
    # geometry well
    if(plot_type == "well"){
      
      well_add_on <- 
        ggplot2::geom_point(
          data = layout_df,
          mapping = ggplot2::aes(fill = .data[[clr_by]]),
          alpha = well_alpha,
          shape = 21,
          size = well_size,
          stroke = border_size, 
          color = border_clr
        )
      
    } else if(plot_type == "tile"){
      
      if(base::isTRUE(display_border)){
        
        well_add_on <- 
          ggplot2::geom_tile(
            data = layout_df,
            mapping = ggplot2::aes(fill = .data[[clr_by]]),
            alpha = well_alpha,
            color = border_clr,
            size = border_size
          )
        
      } else {
        
        well_add_on <- 
          ggplot2::geom_tile(
            data = layout_df,
            mapping = ggplot2::aes(fill = .data[[clr_by]]),
            alpha = well_alpha
          )
        
      }
      
    }
    
    # geometry well plate edge
    if(base::isTRUE(display_edge)){
      
      edge_add_on <- 
        ggforce::geom_mark_rect(
          mapping = ggplot2::aes(x = col_num, y = row_num, color = well_plate_name),
          size = edge_size, expand = ggplot2::unit(15, "mm"), color = edge_clr
        ) 
      
    } else {
      
      edge_add_on <- NULL
      
    }
    
    # geometry labels
    if(base::isTRUE(display_labels)){
      
      text_add_on <- ggplot2::geom_text(mapping = ggplot2::aes(label = well), color = labels_clr)
      
    } else {
      
      text_add_on <- NULL
      
    }
    
    
    # final plot output
    base_line_plot + 
      well_add_on + 
      edge_add_on +
      text_add_on
    
  })
