
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


#' @title Plot numeric variables in a heatmap
#' 
#' @description This function uses a heatmpa to visualize numeric variables. 
#' By specifying the \code{across}-argument in combination with the \code{summarize_with}-
#' argument the heatmap can visualize not only cellular profiles but summarized
#' profiles of any grouping variable such as \emph{cell_line} or \emph{condition}.
#'
#' @inherit argument_dummy params
#' @param summarize_with Character value. Denotes the function with which the 
#' numeric variables are summarized across groups if \code{across} is not specified 
#' as \emph{'cell_id'} but \emph{e.g. 'condition'}. One of \emph{'mean', 'median', 'max'} \emph{'min'}.
#' @param ... Addtional arguments given to function \code{pheatmap::pheatmap()}.
#'
#' @details Input for argument \code{across} can be \emph{'cell_id'} to focus
#' on cells. In this case the summarizing is skipped. 
#' 
#' Before visualization all values are rescaled to values from 0 to 1 within their 
#' variable for proper color coding. 
#' 
#'
#' @return A heatmap.
#' @export
#'
plotHeatmap <- function(object,
                        variable_names,
                        across = "cell_id",
                        across_subset = NULL, 
                        relevel = NULL,
                        summarize_with = "mean", 
                        drop_na = TRUE,
                        phase = NULL,
                        verbose = TRUE,
                        ...){
  
  check_object(object)
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  confuns::check_one_of(
    input = across, 
    against = c("cell_id", getGroupingVariableNames(object, phase = phase))
  )
  
  confuns::check_one_of(
    input = summarize_with, 
    against = base::names(x = stat_funs)
  )
  
  df <- 
    getStatsDf(
      object = object,
      with_grouping = TRUE,
      verbose = FALSE,
      drop_na = drop_na
    ) %>% 
    dplyr::select(dplyr::all_of(c(across, variable_names))) %>% 
    confuns::check_across_subset(
      df = ., 
      across = across, 
      across.subset = across_subset, 
      relevel = relevel
    )
  
  if(!across == "cell_id"){
    
    msg <- glue::glue("Summarizing with {summarize_with}().")
    
    confuns::give_feedback(msg = msg, verbose = verbose)
    
    df <- 
      dplyr::group_by(df, !!rlang::sym(across)) %>% 
      dplyr::summarise(
        dplyr::across(
          .cols = where(base::is.numeric),
          .fns = stat_funs[[summarize_with]]
        )
      )
    
  }
  
  plot_df <-
    dplyr::mutate(
      df,
      dplyr::across(
        .cols = where(base::is.numeric), 
        .fns = ~ scales::rescale(x = .x, to = c(0,1))
      )
    ) %>% 
    tibble::remove_rownames()
  
  mtr <- 
    tibble::column_to_rownames(smrd_df, var = {{across}}) %>% 
    base::as.matrix()
  
  hm <-  
    pheatmap::pheatmap(
      mat = mtr, 
      ...
    )
  
  return(hm)
  
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

plotWellPlateX <- function(object, 
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
  
  # extract df
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
    
    if(color_by == "count"){
    guides_add_on <- ggplot2::guides(color = FALSE)
    
      
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



