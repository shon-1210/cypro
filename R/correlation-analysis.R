#' @include imported-generics.R
NULL


#' @rdname computeCorrelation
#' @export
setMethod(
  f = "computeCorrelation",
  signature = "Cypro", 
  definition = function(object, 
                        across = NULL, 
                        methods_corr = "pearson", 
                        verbose = TRUE,
                        ...){
    
    corr_obj <- getCorrelation(object)
    
    corr_obj <-
      computeCorrelation(
        object = corr_obj,
        across = across, 
        methods_corr = methods_corr, 
        verbose = verbose, 
        ...
      )
    
    object <- 
      setAnalysisAspect(
        object = object,
        analysis_aspect = corr_obj, 
        fset = "all_features"
        )
    
    return(object)
    
  }
)

#' @rdname computeCorrelation
#' @export
setMethod(
  f = "computeCorrelation",
  signature = "CyproTimeLapseMP", 
  definition = function(object, 
                        phase,
                        across = NULL, 
                        methods_corr = "pearson", 
                        verbose = TRUE,
                        ...){
    
    corr_obj <- getCorrelation(object, phase = phase)
    
    corr_obj <-
      computeCorrelation(
        object = corr_obj,
        across = across, 
        methods_corr = methods_corr, 
        verbose = verbose, 
        ...
      )
    
    object <- 
      setAnalysisAspect(
        object = object,
        analysis_aspect = corr_obj,
        fset = "all_features",
        phase = phase
        )
    
    return(object)
    
  }
)



# g -----------------------------------------------------------------------

#' @rdname getCorrDf
#' @export
setMethod(
  f = "getCorrDf", 
  signature = "Cypro", 
  definition = function(object,
                        method_corr = "pearson", 
                        across = NULL, 
                        across_subset = NULL, 
                        pval_threshold = 0.05,
                        type = "complete", 
                        diagonal = TRUE, 
                        distinct = TRUE, 
                        digits = 2,
                        sep = " & ",
                        verbose = TRUE,
                        ...){
    
    corr_obj <- getCorrelation(object)
    
    df <- 
      getCorrDf(
        object = corr_obj,
        method_corr = method_corr, 
        across = across, 
        across_subset = across_subset, 
        type = type, 
        diagonal = diagonal, 
        distinct = distinct, 
        digits = digits, 
        sep = sep, 
        verbose = verbose, 
        ...
      )
    
    return(df)
    
  }
)

#' @rdname getCorrDf
#' @export
setMethod(
  f = "getCorrDf", 
  signature = "CyproTimeLapseMP", 
  definition = function(object,
                        phase, 
                        method_corr = "pearson", 
                        across = NULL, 
                        across_subset = NULL, 
                        pval_threshold = 0.05,
                        type = "complete", 
                        diagonal = TRUE, 
                        distinct = TRUE, 
                        digits = 2,
                        sep = " & ",
                        verbose = TRUE,
                        ...){
    
    corr_obj <- getCorrelation(object, phase = phase)
    
    df <- 
      getCorrDf(
        object = corr_obj,
        method_corr = method_corr, 
        across = across, 
        across_subset = across_subset, 
        type = type, 
        diagonal = diagonal, 
        distinct = distinct, 
        digits = digits, 
        sep = sep, 
        verbose = verbose, 
        ...
      )
    
    return(df)
    
  }
)


#' @rdname getCorrMtr
#' @export
setMethod(
  f = "getCorrMtr", 
  signature = "Cypro", 
  definition = function(object,
                        method_corr = "pearson", 
                        across = NULL, 
                        across_subset = NULL, 
                        type = "complete", 
                        diagonal = TRUE, 
                        flatten = TRUE){
    
    corr_obj <- getCorrelation(object)
    
    mtr <- 
      getCorrMtr(
        object = corr_obj,
        method_corr = method_corr, 
        across = across, 
        across_subset = across_subset, 
        type = type, 
        diagonal = diagonal, 
        flatten = flatten
        )
    
    return(mtr)
    
  }
)

#' @rdname getCorrMtr
#' @export
setMethod(
  f = "getCorrMtr", 
  signature = "CyproTimeLapseMP", 
  definition = function(object,
                        phase,
                        method_corr = "pearson", 
                        across = NULL, 
                        across_subset = NULL, 
                        type = "complete", 
                        diagonal = TRUE, 
                        flatten = TRUE){
    
    corr_obj <- getCorrelation(object, phase = phase)
    
    mtr <- 
      getCorrMtr(
        object = corr_obj,
        method_corr = method_corr, 
        across = across, 
        across_subset = across_subset, 
        type = type, 
        diagonal = diagonal, 
        flatten = flatten
      )
    
    return(mtr)
    
  }
)


#' @rdname getRcorr
#' @export
setMethod(
  f = "getRcorr", 
  signature = "Cypro", 
  definition = function(object,
                        method_corr = "pearson",
                        across = NULL,
                        across_subset = NULL,
                        as_list = FALSE){
    
    corr_obj <- getCorrelation(object)
    
    out <-
      getRcorr(
        object = corr_obj, 
        method_corr = method_corr, 
        across = across, 
        across_subset = across_subset, 
        as_list = as_list
      )
    
    return(out)
    
  }
)

#' @rdname getRcorr
#' @export
setMethod(
  f = "getRcorr", 
  signature = "CyproTimeLapseMP", 
  definition = function(object,
                        phase, 
                        method_corr = "pearson",
                        across = NULL,
                        across_subset = NULL,
                        as_list = FALSE){
    
    phase <- check_phase(object, phase = phase, max_phase = 1)
    
    corr_obj <- getCorrelation(object, phase = phase)
    
    out <-
      getRcorr(
        object = corr_obj, 
        method_corr = method_corr, 
        across = across, 
        across_subset = across_subset, 
        as_list = as_list
      )
    
    return(out)
    
  }
)

# plotting ----------------------------------------------------------------

#' @rdname plotCorrplot
#' @export
setMethod(
  f = "plotCorrplot", 
  signature = "Cypro", 
  definition = function(object,
                        method_corr = "pearson",
                        across = NULL,
                        across_subset = NULL,
                        features = NULL,
                        relevel = FALSE,
                        pval_threshold = NULL,
                        type = "lower",
                        diagonal = TRUE,
                        color_low = "darkred",
                        color_high = "steelblue",
                        shape = "tile",
                        size_by_corr = TRUE,
                        size_max = 15,
                        display_values = TRUE,
                        values_alpha = 0.9,
                        values_color = "black",
                        values_digits = 2,
                        values_size = 4,
                        display_grid = TRUE,
                        grid_color = "grey",
                        grid_size = 0.5,
                        nrow = NULL,
                        ncol = NULL,
                        verbose = TRUE){
    
    corr_obj <- getCorrelation(object)
    
    plotCorrplot(
      object = corr_obj, 
      method_corr = method_corr, 
      across = across, 
      across_subset = across_subset, 
      variables_subset = features,
      relevel = relevel,
      pval_threshold = pval_threshold, 
      type = type, 
      diagonal = diagonal, 
      color_low = color_low, 
      color_high = color_high, 
      shape = shape, 
      size_by_corr = size_by_corr, 
      size_max = size_max, 
      display_values = display_values, 
      values_alpha = values_alpha, 
      values_color = values_color, 
      values_digits = values_digits, 
      values_size = values_size, 
      display_grid = display_grid,
      grid_color = grid_color,
      grid_size = grid_size, 
      nrow = nrow, 
      ncol = ncol, 
      verbose = verbose)
    
  }
)

#' @rdname plotCorrplot
#' @export
setMethod(
  f = "plotCorrplot", 
  signature = "CyproTimeLapseMP", 
  definition = function(object,
                        method_corr = "pearson",
                        across = NULL,
                        across_subset = NULL,
                        features = NULL,
                        relevel = FALSE,
                        pval_threshold = NULL,
                        type = "lower",
                        diagonal = TRUE,
                        color_low = "darkred",
                        color_high = "steelblue",
                        shape = "tile",
                        size_by_corr = TRUE,
                        size_max = 15,
                        display_values = TRUE,
                        values_alpha = 0.9,
                        values_color = "black",
                        values_digits = 2,
                        values_size = 4,
                        display_grid = TRUE,
                        grid_color = "grey",
                        grid_size = 0.5,
                        nrow = NULL,
                        ncol = NULL,
                        verbose = TRUE){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    corr_obj <- getCorrelation(object, phase = phase)
    
    plotCorrplot(
      object = corr_obj, 
      method_corr = method_corr, 
      across = across, 
      across_subset = across_subset, 
      variables_subset = features, 
      relevel = relevel,
      pval_threshold = pval_threshold, 
      type = type, 
      diagonal = diagonal, 
      color_low = color_low, 
      color_high = color_high, 
      shape = shape, 
      size_by_corr = size_by_corr, 
      size_max = size_max, 
      display_values = display_values, 
      values_alpha = values_alpha, 
      values_color = values_color, 
      values_digits = values_digits, 
      values_size = values_size, 
      display_grid = display_grid,
      grid_color = grid_color,
      grid_size = grid_size, 
      nrow = nrow, 
      ncol = ncol, 
      verbose = verbose)
    
  }
)


#' @title Plot correlation variance across groups
#' 
#' @description Uses the style of correlation matrices to visualize 
#' the standard deviation of the correlation values across groups of 
#' a grouping variable - indicating which grouping is responsible for changes
#' in correlation. 
#'
#' @inherit plotCorrplot params 
#' 
#' @param across Character vector (!). Denotes all grouping variables of interest. 
#'
#' @inherit ggplot_family return
#' @export
#'
plotCorrelationSD <- function(object, 
                              variable_set,
                              method_corr = NULL, 
                              phase = NULL,
                              across = NULL, 
                              relevel = NULL, 
                              variables_subset = NULL, 
                              signif_level = NULL, 
                              clrsp = NULL){
  stop("not in use right now")
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  corr_object <- getCorrConv(object, variable_set = variable_set, phase = phase)
  
  confuns::plot_correlation_sd(
    corr.obj = corr_object, 
    method.corr = method_corr, 
    across = across, 
    aes.fill = "sd", 
    signif.level = signif_level
  ) + 
  confuns::scale_color_add_on(
    aes = "fill", 
    clrsp = clrsp
  ) + 
    ggplot2::labs(fill = "SD")
  
}




