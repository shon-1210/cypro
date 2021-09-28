


#' @title Plot scaled cell migration 
#' 
#' @description Visualizes the cells migration in a typical migration plot.
#' Scales the cell's x- and y-coordinates such that all cell's migration
#' paths start from the same position.
#'
#' @inherit argument_dummy params
#' @param display_annotation Logical. If set to TRUE the number of cells (input
#' for argument \code{n_cells}) is displayed in the right upper corner of each plot.
#' 
#' @details Argument \code{across} can be a single character value or a character vector of
#' length two. If character vector, e.g. \code{across = c(\emph{'cell_line', 'condition'})}, 
#' argument \code{across_subset} - if specified - should be a corresponding list: 
#' \code{across_subset = list(cell_line = \emph{c('GCL24', 'GCL38')}, condition = c(\emph{'Ctrl', 'chlorambucil'}))}.
#' 
#' @inherit ggplot_return return
#' @export
#'

plotAllTracks <- function(object,
                          across = "condition",
                          across_subset = NULL,
                          relevel = NULL, 
                          time_subset = NULL,
                          phase = NULL,
                          n_cells = 100,
                          color_by = across[1], 
                          linetype = "solid", 
                          linesize = 0.75,
                          clrp = "milo",
                          clrp_adjust = NULL, 
                          display_annotation = TRUE,
                          set_seed = NULL, 
                          ...,
                          verbose = TRUE){
  
  check_object(object, exp_type_req = "time_lapse")
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase)
  
  #confuns::is_value(across, "character")
  
  if(base::length(phase) == 1) {
    
    confuns::check_one_of(
      input = across, 
      against = getGroupingVariableNames(object, phase = phase, verbose = FALSE)
    )
    
  } else {
    
    confuns::check_one_of(
      input = across, 
      against = c("condition", "cell_line", well_plate_vars)
    )
    
  }
  
  track_df <- 
    getTracksDf(
      object = object,
      phase = phase,
      with_meta = TRUE,
      with_cluster = TRUE,
      verbose = FALSE
    ) %>% 
    hlpr_merge_conditions(
      track_df = ., 
      phase = phase, 
      across = across, 
      verbose = verbose
    ) 
  
  if(base::is.character(across)){
    
    if(base::length(across) == 1){
      
      track_df <- 
        confuns::check_across_subset(
          df = track_df, 
          across = across, 
          across.subset = across_subset,
          relevel = relevel 
        ) %>% 
        dplyr::mutate(
          facet = !!rlang::sym(across)
        )
      
      facet_add_on <- 
        ggplot2::facet_wrap(facets = . ~ facet, ...)
      
    } else if(base::length(across) == 2) {
      
      if(!base::is.null(across)){
        
        if(!base::is.list(across)){
          
          warning("If 'across' is of length 2 input for 'across_subset' must be a list or NULL. Ignoring invalid 'across_subset' input.")
          
          across_subset <- NULL
          
        }
        
      }
      
      track_df <- 
        confuns::check_across_subset(
          df = track_df, 
          across = across[1], 
          across.subset = across_subset[[1]],
          relevel = relevel 
        ) %>% 
        confuns::check_across_subset(
          df = ., 
          across = across[2], 
          across.subset = across_subset[[2]],
          relevel = relevel 
        ) 
      
      across1 <- across[1]
      across2 <- across[2]
      
      facet_add_on <- 
        ggplot2::facet_grid(
          rows = ggplot2::vars(!!rlang::sym(across1)),
          cols = ggplot2::vars(!!rlang::sym(across2)), 
          ...
        )
      
    }
    
  }
  
  if(base::is.numeric(set_seed)){
    
    confuns::give_feedback(msg = glue::glue("Setting seed: {set_seed}."), verbose = verbose)
    
    base::set.seed(seed = set_seed)
    
  }
  
  cell_id_df <- 
    dplyr::select(track_df, dplyr::all_of(x = c("cell_id", across)), dplyr::contains("coords")) %>% 
    tidyr::drop_na() %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(dplyr::across(.cols = dplyr::all_of(across))) %>% 
    dplyr::slice_sample(n = n_cells)
  
  cell_ids <- 
    dplyr::pull(.data = cell_id_df, var = "cell_id")
  
  plot_df <- 
    dplyr::group_by(.data = track_df, cell_id) %>% 
    dplyr::filter(cell_id %in% {{cell_ids}}) %>% 
    dplyr::mutate(
      x_coords = x_coords - x_coords[1], 
      y_coords = y_coords - y_coords[1]
    ) %>% 
    dplyr::ungroup() 
  
  annotation_df <-  
    dplyr::group_by(cell_id_df, dplyr::across(.cols = dplyr::all_of(across))) %>% 
    dplyr::tally() %>% 
    dplyr::mutate(label = stringr::str_c("n", n, sep = " = "))
  
  x_range <- base::range(plot_df$x_coords)
  y_range <- base::range(plot_df$y_coords)
  
  mxm <- base::abs(base::max(c(x_range, y_range)))
  
  if(base::is.numeric(time_subset)){
    
    plot_df <- 
      dplyr::filter(.data = plot_df, frame_time <= {{time_subset}})
    
  }
  
  if(base::is.character(color_by)){
    
    mapping <- ggplot2::aes(group = cell_id, color = .data[[color_by]])
    
  } else {
    
    mapping <- ggplot2::aes(group = cell_id)
    
  }
  
  if(base::isTRUE(display_annotation)){
    
    annotation_add_on <-
      ggplot2::geom_text(
        data = annotation_df,
        x = Inf,
        y = Inf,
        vjust = 1,
        hjust = 1,
        mapping = ggplot2::aes(label = label),
        size = 3
        )
    
  } else {
    
    annotation_add_on <- list()
    
  }
  
  
  ggplot2::ggplot(data = plot_df, mapping = ggplot2::aes(x = x_coords, y = y_coords)) + 
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "lightgrey") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "lightgrey") +
    ggplot2::geom_path(mapping = mapping, linetype = linetype, size = linesize) + 
    annotation_add_on +
    ggplot2::scale_x_continuous(limits = c(-mxm, mxm)) +
    ggplot2::scale_y_continuous(limits = c(-mxm, mxm)) +
    ggplot2::theme_bw() + 
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(), 
      strip.background = ggplot2::element_blank()
    ) + 
    facet_add_on + 
    ggplot2::labs(x = NULL, y = NULL, color = across[1],
                  subtitle = glue::glue("Time: {time_subset} {getIntervalUnit(object)}")) + 
    hlpr_caption_add_on(object = object, phase = phase) + 
    confuns::scale_color_add_on(
      variable = plot_df[[across[1]]],
      clrp = clrp,
      clrp.adjust = clrp_adjust)
  
}

#' @rdname plotAllTracks
#' @export
plotRosePlot <- plotAllTracks



#' @title Plot single cell migration 
#' 
#' @description Visualizes the migration of single cells of interest. 
#'
#' @inherit argument_dummy params
#'
#' @inherit ggplot_return return
#' @export
#'

plotSingleTracks <- function(object,
                             cell_ids,
                             phase = "all",
                             size = 1,
                             color_by = NULL,
                             scales = "free"){
  
  check_object(object, module_req = "migration")
  assign_default(object)
  
  phase <- check_phase(object, phase = phase)
  
  track_df <-
    getTracksDf(object = object, phase = phase) %>% 
    dplyr::filter(cell_id %in% {{cell_ids}}) 
  
  start_df <- 
    dplyr::group_by(.data = track_df, cell_id) %>% 
    dplyr::filter(frame_num == base::min(frame_num))
  
  if(base::is.character(color_by)){
    
    mapping <- ggplot2::aes(x = x_coords, y = y_coords, color = !!rlang::sym(color_by))
    
  } else {
    
    mapping <- ggplot2::aes(x = x_coords, y = y_coords)
    
  }
  
  ggplot2::ggplot(data = track_df, mapping = mapping) + 
    ggplot2::geom_point(size = 0.75) + 
    ggplot2::geom_path(mapping = ggplot2::aes(group = cell_id),
                       size = size, arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches"))) +
    ggplot2::geom_point(data = start_df, size = 2) + 
    ggplot2::facet_wrap(facets = ~ cell_id, scales = scales) +
    ggplot2::theme_bw() + 
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank(), 
      strip.background = ggplot2::element_rect(fill = ggplot2::alpha("steelblue", 0.75))
    ) + 
    ggplot2::labs(x = "x-coordinates", y = "y-coordinates") + 
    hlpr_caption_add_on(object, phase = phase)
  
  
}






#' @title Plot cell activity over time 
#'
#' @description Visualizes the percentage of active cells over time. 
#' 
#' @inherit argument_dummy params
#' @param threshold Numeric value or NULL. If set to NULL (the default) the 
#' threshold to consider a cell 'active' is equal to \code{base::mean(speed) + base::sd(speed)}
#' 
#' @inherit ggplot_return params
#' 
#' @export

plotVelocityLineplot <- function(object, 
                                 across = "condition", 
                                 across_subset = NULL, 
                                 phase = NULL,
                                 threshold = NULL,
                                 linesize = 1,
                                 smooth = TRUE, 
                                 smooth_span = 0.25, 
                                 smooth_se = FALSE,
                                 clrp = "milo", 
                                 verbose = TRUE, 
                                 ...,
                                 in_shiny = FALSE){
  
  check_object(object, exp_type_req = "time_lapse")
  assign_default(object)
  
  # speed data shifted 
  speed_df <- 
    getTracksDf(
      object,
      with_cluster = TRUE, 
      with_meta = TRUE, 
      with_well_plate = TRUE,
      phase = phase,
      verbose = verbose
    ) %>% 
    hlpr_merge_conditions(
      track_df = ., 
      phase = phase, 
      across = across, 
      verbose = verbose
    ) %>% 
    hlpr_subset_across(
      across = across, 
      across_subset = across_subset
    ) %>% 
    tidyr::pivot_wider(
      data = ., 
      id_cols = dplyr::all_of(x = c("cell_id", "frame_itvl", across)), 
      names_from = "frame_itvl", 
      values_from = "speed"
    ) 
  
  descr_df <-
    dplyr::select(.data = speed_df, cell_id, !!rlang::sym(across))
  
  numeric_mtr <- 
    tibble::column_to_rownames(.data = speed_df, var = "cell_id") %>% 
    dplyr::select_if(.predicate = base::is.numeric) %>% 
    base::as.matrix()
  
  if(!base::is.numeric(threshold)){
    
    threshold <-
      base::mean(numeric_mtr, na.rm = TRUE) + stats::sd(numeric_mtr, na.rm = TRUE)
    
  }
  
  numeric_mtr[numeric_mtr <= threshold] <- 0
  numeric_mtr[numeric_mtr > threshold] <- 1
  
  numeric_df <- base::as.data.frame(numeric_mtr)
  
  plot_df <- 
    tibble::rownames_to_column(.data = numeric_df, var = "cell_id") %>% 
    dplyr::left_join( x = descr_df, y = ., by = "cell_id") %>% 
    dplyr::group_by(!!rlang::sym(across)) %>% 
    dplyr::mutate(group_count = dplyr::n()) %>% 
    dplyr::group_by(!!rlang::sym(across), group_count) %>% 
    dplyr::summarise(dplyr::across(.cols = base::is.numeric, .fns = ~ base::sum(.x))) %>% 
    tidyr::pivot_longer(
      cols = dplyr::ends_with(match = object@set_up$itvl_u),
      names_to = "time",
      values_to = "count_active_cells"
    ) %>% 
    dplyr::mutate(
      perc_active_cells = count_active_cells / group_count * 100,
      time = base::as.numeric(stringr::str_remove(string = time, pattern = object@set_up$itvl_u))
    ) %>% 
    dplyr::ungroup() %>% 
    tidyr::drop_na()
  
  
  if(base::isTRUE(smooth)){
    
    geom_line_add_on <-
      ggplot2::geom_smooth(
        mapping = ggplot2::aes(color = .data[[across]]),
        span = smooth_span, 
        formula = y ~ x, 
        method = "loess", 
        se = smooth_se, 
        size = linesize
      )
    
  } else {
    
    geom_line_add_on <- 
      ggplot2::geom_path(
        mapping = ggplot2::aes(group = .data[[across]], color = .data[[across]]), 
        size = linesize
      )  
    
  }
  
  ggplot2::ggplot(data = plot_df, mapping = ggplot2::aes(x = time, y = perc_active_cells)) + 
    geom_line_add_on +
    geom_line_add_on +
    ggplot2::theme_classic() + 
    ggplot2::theme(
      title = ggplot2::element_text(face = "bold", size = 10, vjust = -1),
      axis.title = ggplot2::element_text(face = "bold", size = 10),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "lightgrey")
    ) + 
    ggplot2::labs(x = stringr::str_c("Time [", object@set_up$itvl_u, "]", sep = ""), 
                  y = "Active Cells [%]") + 
    confuns::scale_color_add_on(
      aes = "color", variable = plot_df[[across]], clrp = clrp, ...
    )
  
}