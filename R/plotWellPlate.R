



#' @title Visualize well plates
#' 
#' @description Plots a well plate.
#'
#' @param object 
#' @param ... 
#'
#' @export
#'
setGeneric(name = "plotWellPlate", def = function(object, ...){
  
  standardGeneric(f = "plotWellPlate")
  
})


#' @rdname plotWellPlate
#' @export
setMethod(
  f = "plotWellPlate",
  signature = "layout_df",
  definition = function(object, 
                        color_by = NULL, 
                        fill_by = NULL,
                        plot_type = "tile",
                        well_alpha = 0.9,
                        well_color = "black",
                        well_size = 13.5, 
                        well_stroke = 2,
                        display_border = TRUE, 
                        border_color = "black", 
                        border_size = 1,
                        display_edge = TRUE,
                        edge_color = "black",
                        edge_size = 1,
                        display_labels = TRUE,
                        labels_color = "black",
                        clrp = "milo",
                        clrp_adjust = NULL,
                        ncol = NULL,
                        nrow = NULL, 
                        selected_wells = NULL){
    
    layout_df <- object
    
    plot_well_plate_hlpr(
      layout_df = layout_df, 
      color_by = color_by, 
      fill_by = fill_by,
      plot_type = plot_type, 
      well_alpha = well_alpha,
      well_color = well_color,
      well_size = well_size, 
      well_stroke = well_stroke,
      display_border = display_border, 
      border_color = border_color, 
      border_size = border_size, 
      display_edge = display_edge, 
      edge_color = edge_color, 
      edge_size = edge_size, 
      display_labels = display_labels, 
      labels_color = labels_color, 
      clrp = clrp,
      clrp_adjust = clrp_adjust, 
      ncol = ncol, 
      nrow = nrow, 
      selected_wells = selected_wells)
    
  })

#' @rdname plotWellPlate
#' @export
setMethod(
  f = "plotWellPlate",
  signature = "layout_df_mp",
  definition = function(object, 
                        color_by = NULL, 
                        fill_by = NULL,
                        plot_type = "tile",
                        well_alpha = 0.9,
                        well_color = "black",
                        well_size = 13.5, 
                        well_stroke = 2,
                        display_border = TRUE, 
                        border_color = "black", 
                        border_size = 1,
                        display_edge = TRUE,
                        edge_color = "black",
                        edge_size = 1,
                        display_labels = TRUE,
                        labels_color = "black",
                        clrp = "milo",
                        clrp_adjust = NULL,
                        ncol = NULL,
                        nrow = NULL,
                        selected_wells = NULL){
    
    layout_df <- merge_condition(object)
    
    plot_well_plate_hlpr(
      layout_df = layout_df, 
      fill_by = fill_by,
      color_by = color_by, 
      plot_type = plot_type, 
      well_alpha = well_alpha,
      well_color = well_color,
      well_size = well_size, 
      well_stroke = well_stroke,
      display_border = display_border, 
      border_color = border_color, 
      border_size = border_size, 
      display_edge = display_edge, 
      edge_color = edge_color, 
      edge_size = edge_size, 
      display_labels = display_labels, 
      labels_color = labels_color, 
      clrp = clrp,
      clrp_adjust = clrp_adjust, 
      ncol = ncol, 
      nrow = nrow, 
      selected_wells = selected_wells
    )
    
  })


#' @rdname plotWellPlate
#' @export
setMethod(
  f = "plotWellPlate", 
  signature = "Cypro", 
  definition = function(object, 
                        well_plate = NULL, 
                        color_by = NULL, 
                        fill_by = NULL,
                        plot_type = "tile",
                        well_alpha = 0.9,
                        well_color = "black",
                        well_size = 13.5, 
                        well_stroke = 2,
                        display_border = TRUE, 
                        border_color = "black", 
                        border_size = 1,
                        display_edge = TRUE,
                        edge_color = "black",
                        edge_size = 1,
                        display_labels = TRUE,
                        labels_color = "black",
                        clrp = "milo",
                        clrp_adjust = NULL,
                        ncol = NULL,
                        nrow = NULL,
                        selected_wells = NULL){
    
    layout_df <- getLayoutDf(object, well_plate)
    
    plotWellPlate(
      object = layout_df, 
      color_by = color_by, 
      fill_by = fill_by,
      plot_type = plot_type, 
      well_alpha = well_alpha,
      well_color = well_color,
      well_size = well_size, 
      well_stroke = well_stroke,
      display_border = display_border, 
      border_color = border_color, 
      border_size = border_size, 
      display_edge = display_edge, 
      edge_color = edge_color, 
      edge_size = edge_size, 
      display_labels = display_labels, 
      labels_color = labels_color, 
      clrp = clrp,
      clrp_adjust = clrp_adjust, 
      ncol = ncol, 
      nrow = nrow, 
      selected_wells = selected_wells
    )
    
  }
)

plot_well_plate_hlpr <- function(layout_df, 
                                 color_by = "file_status", 
                                 fill_by = "condition",
                                 plot_type = "tile",
                                 well_alpha = 0.9,
                                 well_color = "black",
                                 well_fill = "white",
                                 well_size = 13.5, 
                                 well_stroke = 2,
                                 display_border = TRUE, 
                                 border_color = "black", 
                                 border_size = 1,
                                 display_edge = TRUE,
                                 edge_color = "black",
                                 edge_size = 1,
                                 display_labels = TRUE,
                                 labels_color = "black",
                                 clrp = "milo",
                                 clrp_adjust = NULL,
                                 clrsp = "inferno",
                                 ncol = NULL,
                                 nrow = NULL,
                                 selected_wells = NULL,
                                 ...){
  
  alpha <- well_alpha
  color <- well_color
  fill <- well_fill
  size <- well_size
  stroke <- well_stroke
  
  border <- 0.75
  limit_x <- base::max(layout_df$col_num) + border
  limit_y <- base::max(layout_df$row_num) + border
  
  layout_names <- 
    dplyr::select_if(layout_df, .predicate = ~ !base::is.list(.x)) %>% 
    dplyr::select(-selected) %>% 
    base::colnames()
  
  check_one_of(
    input = color_by, 
    against = layout_names
  )
  
  check_one_of(
    input = fill_by,
    against = layout_names
  )
  
  check_one_of(
    input = plot_type, 
    against = c("tile", "well")
  )
  
  if(base::is.character(color_by)){
  
    clrp_adjust <- c(clrp_adjust, cypro_clrp_adjust[[color_by]])  
    
    var_color <- layout_df[[color_by]]
    
  } else {
    
    var_color <- "discrete"
    
  }
  
  if(base::is.character(fill_by)){
    
    clrp_adjust <- c(clrp_adjust, cypro_clrp_adjust[[fill_by]])
    
    var_fill <- layout_df[[fill_by]]
    
  } else {
    
    var_fill <- "discrete"
  }
  
  if(base::is.character(selected_wells)){
    
    layout_df <- 
      dplyr::mutate(
        .data = layout_df, 
        selected = dplyr::case_when(
          well %in% {{selected_wells}} ~ TRUE, 
          TRUE ~ FALSE
        )
      )
    
  }
  
  unselected_df <- dplyr::filter(layout_df, !selected)
  selected_df <- dplyr::filter(layout_df, selected)
  
  # create base line ggplot
  base_line_plot <- 
    ggplot2::ggplot(data = unselected_df, mapping = ggplot2::aes(x = col_num,y = row_num)) +
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
    ggplot2::labs(
      x = NULL, 
      y = NULL,
      color = make_pretty_name(color_by),
      fill = make_pretty_name(fill_by) 
      ) + 
    scale_color_add_on(
      aes = "fill", clrp = clrp, clrp.adjust = clrp_adjust,
      variable = var_fill, clrsp = clrsp, ...
    ) + 
    scale_color_add_on(
      aes = "color", clrp = clrp, clrp.adjust = clrp_adjust, 
      variable = var_color, clrsp, ...
      )
  
  
  # add facets
  if(dplyr::n_distinct(layout_df$well_plate_name) > 1){
    
    base_line_plot <- 
      base_line_plot + 
      ggplot2::facet_wrap(facets = . ~ well_plate_name, ncol = ncol, nrow = nrow)
    
  }
  
  # selected wells add on 
  selected_wells_add_on <- 
    create_well_add_on(
      df = selected_df, 
      plot_type = plot_type, 
      alpha = 1, 
      color = "black", 
      fill = "red", 
      size = size,
      stroke = stroke, 
      color_by = NULL, 
      fill_by = NULL, 
      display_border = display_border, 
      border_color = "black",
      border_size = border_size
    )
  
  # wells add on 
  well_add_on <- 
    create_well_add_on(
      df = unselected_df,
      plot_type = plot_type,
      alpha = alpha, 
      color = color, 
      fill = fill,
      size = size,
      stroke = stroke,
      color_by = color_by,
      fill_by = fill_by, 
      display_border = display_border, 
      border_color = border_color, 
      border_size = border_size
    )
  
  # well plate edge add on 
  if(base::isTRUE(display_edge)){
    
    edge_add_on <- 
      ggforce::geom_mark_rect(
        mapping = ggplot2::aes(x = col_num, y = row_num, color = well_plate_name),
        size = edge_size, expand = ggplot2::unit(15, "mm"), color = edge_color,
        data = layout_df
      ) 
    
  } else {
    
    edge_add_on <- NULL
    
  }
  
  # labels add on 
  if(base::isTRUE(display_labels)){
    
    text_add_on <- 
      ggplot2::geom_text(
        mapping = ggplot2::aes(label = well),
        color = labels_color,
        data = layout_df
        )
    
  } else {
    
    text_add_on <- NULL
    
  }
  
  # final plot output
  base_line_plot + 
    well_add_on + 
    selected_wells_add_on +
    edge_add_on +
    text_add_on
  
}