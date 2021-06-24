
#' @title ggplot_return
#' @return A ggplot. 
ggplot_return <- function(){}


#' @title ggplot_return
#' @return A ggplot. 
ggplot_family <- function(){}


#' @title Plot cell count 
#' 
#' @description Visualizes the number and distribution of cells across 
#' a discrete feature of choice. 
#'
#' @inherit argument_dummy params
#' 
#' @param color_by Character value. Denotes the discrete variable with 
#' which to color the columns of the plot. Defaults to input for 
#' argument \code{across}.
#'
#' @inherit ggplot_return return
#' @export
#'

plotCellCount <- function(object, across, color_by = across, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  stat_df <-
    getStatsDf(object,
               with_cluster = TRUE, 
               with_meta = TRUE,
               with_well_plate = TRUE,
               verbose = FALSE,
               phase = phase)
  
  confuns::check_one_of(
    input = across, 
    against = base::colnames(stat_df)
  )
  
  confuns::check_one_of(
    input = color_by, 
    against = base::colnames(stat_df)
  )
  
  ggplot2::ggplot(data = stat_df, mapping = ggplot2::aes(x = .data[[across]])) + 
    ggplot2::geom_bar(mapping = ggplot2::aes(fill = .data[[color_by]]), color = "black") + 
    ggplot2::theme_classic() + 
    ggplot2::labs(y = "Count", x = NULL) + 
    confuns::scale_color_add_on(aes = "fill", variable = "discrete", clrp = "milo")
  
}


#' @title Plot a scatterplot
#' 
#' @description Convenient wrapper around a variety of scatterplot functionalities. See details for more. 
#' 
#' @inherit argument_dummy params
#'
#' @param variable_x,variable_y Character values. The numeric variables to be plotted on the x-axis and
#' y-axis. Use \code{getNumericVariableNames()} to obtain all valid input options.
#' @param display_corr Logical value. If set to TRUE the correlation of the x- and y-variable is calculated 
#' and displayed on the plot (or each subplot if \code{across} is specified.) 
#' @param corr_method Character value. Denotes the method with which to compute the correlation values if 
#' \code{display_corr} is set to TRUE. Defaults to \emph{'pearson'}.
#' @param corr_pmin Numeric value. The minimum p-value that is displayed as a number. Everything below it is
#' displayed as \emph{ < \code{corr_pmin}}.
#' @param corr_pos_x,corr_pos_y Numeric values or NULL. Specify the exact position of the text used to display the correlation 
#' results. If set to NULL defaults to left upper corner of the plot.
#' @param corr_text_sep Character value. The string that separates correlation value and respective p-value.
#' @param corr_text_size Numeric value. The size used to print the correlation results.
#' 
#' @details Argument \code{across} can be specified as a character vector of length 2. If so, argument 
#' \code{across_subset} must be a list of character vectors whereby the names of the list correspond to the input for \code{across}. 
#'
#' @inherit ggplot_family return
#' @export
#'
plotScatterplot <- function(object, 
                            variable_x, 
                            variable_y,
                            phase = NULL,
                            across = NULL,
                            across_subset = NULL,
                            relevel = TRUE,
                            ncol = NULL,
                            nrow = NULL,
                            color_aes = NULL, 
                            color_by = NULL,
                            scales = "fixed",
                            space = "fixed",
                            pt_alpha = 0.9,
                            pt_clr = NULL,
                            pt_clrp = NULL, 
                            pt_fill = NULL,
                            pt_shape = NULL,
                            pt_size = NULL,
                            display_smooth = FALSE,
                            smooth_alpha = NULL,
                            smooth_clr = NULL,
                            smooth_method = NULL,
                            smooth_se = NULL,
                            smooth_size = NULL,
                            display_corr = FALSE,
                            corr_method = "pearson",
                            corr_pmin = 5e-05,
                            corr_pos_x = NULL,
                            corr_pos_y = NULL,
                            corr_text_sep = "\n",
                            corr_text_size = 1,
                            ...){
  
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  stats_df <- 
    getStatsDf(
      object = object,
      phase = phase,
      with_meta = TRUE, 
      with_cluster = TRUE,
      with_well_plate = TRUE,
      drop_na = FALSE, 
      verbose = FALSE) %>% 
    dplyr::select(-cell_id)
  
  confuns::plot_scatterplot(
    df = stats_df, 
    x = variable_x, 
    y = variable_y, 
    across = across, 
    across.subset = across_subset,
    relevel = relevel,
    ncol = ncol,
    nrow = nrow,
    clr.aes = color_aes,
    clr.by = color_by,
    scales = scales,
    space = space,
    pt.alpha = pt_alpha,
    pt.clr = pt_clr,
    pt.clrp = pt_clrp,
    pt.fill = pt_fill,
    pt.shape = pt_shape,
    pt.size = pt_size,
    display.smooth = display_smooth,
    smooth.alpha = smooth_alpha,
    smooth.clr = smooth_clr,
    smooth.method = smooth_method,
    smooth.se = smooth_se,
    smooth.size = smooth_size,
    display.corr = display_corr,
    corr.method = corr_method,
    corr.p.min = corr_pmin,
    corr.pos.x = corr_pos_x,
    corr.pos.y = corr_pos_y,
    corr.text.sep = corr_text_sep,
    corr.text.size = corr_text_size,
    ...
  )
  
}


#' @title Plot the well plate set up 
#' 
#' @inherit argument_dummy params
#' @param plot_type Character value. Either \emph{'well'} or \emph{'tile'}.
#' Affects the geometry with which the wells are displayed. (Either 
#' with \code{ggplot2::geom_point()} or with \code{ggplot2::geom_tile()}).
#' @param display_border Logical value. If set to TRUE a line is drawn around 
#' the geometric objects displaying the wells. 
#' @param border_clr Character value. Denotes the color of the wells borders. 
#' @param border_size Numeric value. Denotes the thickness of the wells borders.
#' @param display_edge Logical value. If set to TRUE \code{ggforce::geom_mark_rect()}
#' is used to display the edges of the well plate.
#' @param edge_clr Character value. Denotes the color of the well plates edge. 
#' @param well_size Numeric value. Denotes the size with which each well is plotted
#' if argument \code{plot_type} is set to \emph{'well'}. 
#'
#' @inherit ggplot_return return 
#' @export
#'

plotWellPlate <- function(object, 
                          phase = NULL, 
                          well_plates = NULL,
                          color_by = "condition", 
                          summarize_with = "median",
                          plot_type = "well",
                          well_size = 13.5,
                          display_border = TRUE, 
                          border_clr = "black",
                          border_size = 1,
                          display_edge = TRUE, 
                          edge_clr = "black",
                          display_labels = TRUE,
                          alpha = 0.9,
                          clrp = "milo",
                          clrp_adjust = NULL,
                          clrsp = "Blues",
                          ncol = NULL, 
                          nrow = NULL, 
                          ...){
  
  # check input
  check_object(object, set_up_req = "experiment_design")
  assign_default(object)
  
  phase <- check_phase(object, phase = phase)
  
  if(base::is.null(well_plates)){
    
    well_plates <- getWellPlateNames(object)
    
  } else {
    
    confuns::check_one_of(
      input = well_plates, 
      against = getWellPlateNames(object)
    )
    
  }
  
  # color_by
  stat_vars <- getStatVariableNames(object)
  
  confuns::check_one_of(
    input = color_by, 
    against = c("cell_line", "condition", "count", stat_vars)
  )
  
  # summarize_with
  confuns::check_one_of(
    input = summarize_with, 
    against = base::names(stat_funs)
  )
  
  stat_fun <- stat_funs[[summarize_with]]
  
  
  # prepare well plate set up data.frame  
  wp_df <-
    purrr::imap_dfr(
      .x = object@well_plates[well_plates],
      .f = function(wp_list, wp_name){
      
      wp_df <- wp_list$wp_df
      
      if(base::is.null(wp_df)){
        
        wp_df <- wp_list$wp_df_eval
        
      }
      
      wp_df$well_plate_name <- wp_name
      
      base::return(wp_df)
      
    })
  
  wp_df <- 
    dplyr::select(wp_df,
      row_num, col_num, row_letter, well, availability_status, 
      cell_line, condition, group, well_plate_name
      )
  
  # extract data df
  if(multiplePhases(object) & color_by %in% c(stat_vars, "count")){
    
    data_df <- 
      purrr::map_df(.x = phase, .f = function(p){
        
        getStatsDf(object, phase = p, with_cluster = FALSE, with_meta = FALSE, with_well_plate = TRUE) %>% 
          dplyr::mutate(phase = {{p}})
        
      }) %>% 
      dplyr::group_by(well_plate_name, well, phase) 
    
    data_df$phase <- 
      stringr::str_c( confuns::make_capital_letters(data_df$phase, collapse.with = NULL), "Phase:", sep = " ")
    
    join_df <- dplyr::select(wp_df, well, row_num, col_num) 
    
    complete_well_df <- 
      tidyr::expand_grid(
        well = base::unique(wp_df$well), 
        well_plate_name = base::unique(wp_df$well_plate_name), 
        phase = base::unique(data_df$phase)
      ) %>% 
      dplyr::left_join(
        x = ., 
        y = join_df, 
        by = "well"
      )
      
    
  } else {
    
    # phase irrelevant
    phase <- 1
    
    data_df <-
      getStatsDf(
        object = object,
        phase = phase,
        with_meta = TRUE,
        with_well_plate = TRUE,
        with_cluster = FALSE
      ) %>% 
      dplyr::group_by(well_plate_name, well)
    
    join_df <- dplyr::select(wp_df, well, row_num, col_num) 
    
    complete_well_df <- 
      tidyr::expand_grid(
        well = base::unique(wp_df$well), 
        well_plate_name = base::unique(wp_df$well_plate_name)
      ) %>% 
      dplyr::left_join(
        x = ., 
        y = join_df, 
        by = "well"
      )
    
  }
  
  # set plot values 
  pt_size <- well_size
  pt_stroke <- 2
  border <- 0.75
  limit_x <- base::max(wp_df$col_num) + border
  limit_y <- base::max(wp_df$row_num) + border
  
  # calculate numeric summary if necessary
  if(!color_by %in% c("cell_line", "condition")){
    
    # numeric coloring
    color_add_on <- 
      confuns::scale_color_add_on(
        aes = "fill", variable = wp_df[[color_by]], clrsp = clrsp, 
        na.value = "lightgrey", ...
      ) 
    
    guides_add_on <- ggplot2::guides(color = FALSE)
    
    if(color_by == "count"){
      
      smrd_df <- 
        dplyr::summarise(data_df, count = dplyr::n())
      
    } else {
      
      confuns::give_feedback(
        msg = glue::glue("Summarizing variable '{color_by}' with {summarize_with}."),
        verbose = verbose
      )
      
      smrd_df <- 
        dplyr::summarise(
          .data = data_df, 
          dplyr::across(
            .cols = !!rlang::sym(color_by), 
            .fns = stat_fun
          )
        )
      
    }
    
    wp_df <- 
      dplyr::left_join(x = wp_df, y = smrd_df, by = c("well", "well_plate_name"))
    
    
    if("phase" %in% base::colnames(smrd_df)){
      
      wp_df <- 
        dplyr::left_join(x = complete_well_df, y = wp_df, by = c("row_num", "col_num", "well", "well_plate_name", "phase")) %>% 
        # column for geom_mark_rect - aes(color = )
        dplyr::mutate(well_plate_name_phase = stringr::str_c(well_plate_name, phase, sep = "_"))
      
    } else {
      
      wp_df <- 
        dplyr::left_join(x = complete_well_df, y = wp_df, by = c("row_num", "col_num", "well", "well_plate_name"))
      
    }
    
    
  } else {
    
    # discrete coloring 
    color_add_on <- 
      confuns::scale_color_add_on(
        aes = "fill", variable = wp_df[[color_by]], clrp = "milo", 
        clrp.adjust = c(clrp_adjust, "unknown" = "lightgrey", "unknown & unknown" = "lightgrey")
      )
    
    guides_add_on <- 
      ggplot2::guides(
        color = FALSE, 
        fill = ggplot2::guide_legend(override.aes = list(size = 10, shape = 21))
      )
    
  }

  # prepare add ons 
  # well plate edge
  if(base::isTRUE(display_edge)){
    
    if("phase" %in% base::colnames(data_df)){
      
      mapping_edge <- 
        ggplot2::aes(x = col_num, y = row_num, color = well_plate_name_phase)
      
    } else {
      
      mapping_edge <- 
        ggplot2::aes(x = col_num, y = row_num, color = well_plate_name)
      
    }
    
    edge_add_on <- 
      ggforce::geom_mark_rect(
        mapping = mapping_edge,
        size = 1, expand = ggplot2::unit(15, "mm")
      ) 
    
  } else {
    
    edge_add_on <- NULL
    
  }
  
  # well labels
  if(base::isTRUE(display_labels)){
    
    text_add_on <- ggplot2::geom_text(mapping = ggplot2::aes(label = well))
    
  } else {
    
    text_add_on <- NULL
    
  }
  
  # well geometries
  if(plot_type == "well"){
    
    geom_add_on <- 
      ggplot2::geom_point(
        data = wp_df,
        mapping = ggplot2::aes(fill = .data[[color_by]]),
        alpha = alpha,
        shape = 21,
        size = well_size,
        stroke = border_size, 
        color = border_clr
      )
    
  } else if(plot_type == "tile"){
    
    if(base::isTRUE(display_border)){
      
      geom_add_on <- 
        ggplot2::geom_tile(
          data = wp_df,
          mapping = ggplot2::aes(fill = .data[[color_by]]),
          alpha = alpha,
          color = border_clr,
          size = border_size
        )
      
    } else {
      
      geom_add_on <- 
        ggplot2::geom_tile(
          data = wp_df,
          mapping = ggplot2::aes(fill = .data[[color_by]])
        )
      
    }
    
  }
  
  # split by phase 
  if("phase" %in% base::colnames(data_df)){
    
    facet_add_on <- 
      ggplot2::facet_grid(rows = well_plate_name ~ phase, switch = "y")
    
  } else {
    
    facet_add_on <-
      ggplot2::facet_wrap(facets = . ~ well_plate_name, nrow = nrow, ncol = ncol)
    
  }
  
  if(color_by == "information_status"){
    
    clrp_adjust <- colors_information_status
    
    edge_adjust <- NULL
    
  } else {
    
    clrp_adjust <- c(clrp_adjust, "Discarded" = "lightgrey")
  
    if("phase" %in% base::colnames(data_df)){
      
      well_plates <- base::unique(wp_df$well_plate_name_phase)
      
    }
    
    edge_adjust <- 
      purrr::set_names(x = base::rep(edge_clr, base::length(well_plates)), well_plates)  
  }
  
  
  # plot output
  ggplot2::ggplot(data = wp_df, mapping = ggplot2::aes(x = col_num,y = row_num)) + 
    geom_add_on + 
    text_add_on +
    color_add_on +
    facet_add_on + 
    edge_add_on +
    guides_add_on + 
    ggplot2::scale_x_continuous(limits = c(-border, limit_x)) +
    ggplot2::scale_y_reverse(limits = c(border + limit_y, -border)) +
    ggplot2::labs(x = NULL, y = NULL)  + 
    confuns::scale_color_add_on(
      aes = "color", 
      variable = wp_df$well_plate_name, 
      clrp = "milo",
      clrsp = clrsp,
      clrp.adjust = edge_adjust
    ) + 
    ggplot2::theme_classic() + 
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank(), 
      strip.background = ggplot2::element_rect(fill = ggplot2::alpha("steelblue", 0.75)),
      panel.background = ggplot2::element_blank(), 
      axis.line = ggplot2::element_blank(), 
      axis.ticks = ggplot2::element_blank(), 
      axis.text = ggplot2::element_blank()
    ) 
  
}



