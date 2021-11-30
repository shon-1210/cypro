


# boxplot -----------------------------------------------------------------

#' @title Plot numeric distribution and statistical tests
#'
#' @description These functions display the distribution of numeric
#' variables for the whole sample or in a comparative manner if argument
#' \code{across} is specified. \code{plotViolinplot()} and \code{plotBoxplot}
#' allow for statistical tests such as t-test or ANOVA.
#'
#' @inherit argument_dummy params
#' @inherit variables_num params
#'
#' @param test_groupwise Character value or NULL. Specifies the groupwise statistical
#' test to be conducted. If character, one of \emph{'anova', 'kruskal.test'}. If set
#' to NULL the testing is skipped.
#' @param test_pairwise Character value or NULL. Specifies the pairwise statistical
#' test to be conducted. If character, one of \emph{'t.test', 'wilcox.test'}. If set
#' to NULL the testing is skipped.
#' @param ref_group Character value or NULL. Specifies the reference group against
#' which all other groups are compared in the test denoted in \code{test_groupwise}
#' is conducted. If set to NULL the first group found is taken.
#' @param step_increase Numeric value. Denotes the increase in fraction of total
#' height for every additional comparison to minimize overlap.
#' @param vjust Numeric value. Denotes the relative, vertical position of the results of
#' the test denoted in \code{test.groupwise}. Negative input highers, positive
#' input lowers the position.
#'
#' @param ... Additional arguments given to the respective \code{ggplot2::geom_<plot_type>()}
#' function. E.g. \code{plotViolinplot()} relies on \code{ggplot2::geom_violin()}.
#'
#' @inherit ggplot_family return
#'
#' @export

setGeneric(name = "plotBoxplot", def = function(object, ...){
  
  standardGeneric(f = "plotBoxplot")
  
})


#' @rdname plotBoxplot
#' @export
setMethod(
  f = "plotBoxplot",
  signature = "CyproScreening",
  definition = function(object, 
                        features,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        test_groupwise = NULL,
                        test_pairwise = NULL,
                        ref_group = NULL,
                        step_increase = 0.01,
                        vjust = 0,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        display_points = FALSE,
                        pt_alpha = 0.8,
                        pt_clr = "black",
                        pt_num = 100,
                        pt_size = 1.25,
                        pt_shape = 21,
                        verbose = TRUE,
                        ...){
    
    df <- 
      getFeatureDf(
        object = object,
        with_well_plate = TRUE,
        with_meta = TRUE,
        with_cluster = TRUE
      )
    
    plot_boxplot(
      df = df, 
      variables = features, 
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      test.pairwise = test_pairwise,
      test.groupwise = test_groupwise,
      ref.group = ref_group,
      step.increase = step_increase,
      vjust = vjust,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      display.points = display_points,
      pt.alpha = pt_alpha,
      pt.color = pt_clr,
      pt.num = pt_num,
      pt.shape = pt_shape,
      pt.size = pt_size,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      verbose = verbose,
      ...
    )
    
  })


#' @rdname plotBoxplot
#' @export
setMethod(
  f = "plotBoxplot",
  signature = "CyproTimeLapse",
  definition = function(object, 
                        features,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        test_groupwise = NULL,
                        test_pairwise = NULL,
                        ref_group = NULL,
                        step_increase = 0.01,
                        vjust = 0,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        display_points = FALSE,
                        pt_alpha = 0.8,
                        pt_clr = "black",
                        pt_num = 100,
                        pt_size = 1.25,
                        pt_shape = 21,
                        verbose = TRUE,
                        ...){
    
    df <- 
      getStatsDf(
        object = object,
        with_well_plate = TRUE,
        with_meta = TRUE,
        with_cluster = TRUE
      )
    
    plot_boxplot(
      df = df, 
      variables = features, 
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      test.pairwise = test_pairwise,
      test.groupwise = test_groupwise,
      ref.group = ref_group,
      step.increase = step_increase,
      vjust = vjust,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      display.points = display_points,
      pt.alpha = pt_alpha,
      pt.color = pt_clr,
      pt.num = pt_num,
      pt.shape = pt_shape,
      pt.size = pt_size,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      verbose = verbose,
      ...
    )
    
  })

#' @rdname plotBoxplot
#' @export
setMethod(
  f = "plotBoxplot",
  signature = "CyproTimeLapseMP",
  definition = function(object, 
                        features,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        test_groupwise = NULL,
                        test_pairwise = NULL,
                        ref_group = NULL,
                        step_increase = 0.01,
                        vjust = 0,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        display_points = FALSE,
                        pt_alpha = 0.8,
                        pt_clr = "black",
                        pt_num = 100,
                        pt_size = 1.25,
                        pt_shape = 21,
                        phase = NULL, 
                        verbose = TRUE,
                        ...){
    
    df <- 
      getStatsDf(
        object = object,
        with_well_plate = TRUE,
        with_meta = TRUE,
        with_cluster = TRUE, 
        phase = phase
      )
    
    plot_boxplot(
      df = df, 
      variables = features, 
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      test.pairwise = test_pairwise,
      test.groupwise = test_groupwise,
      ref.group = ref_group,
      step.increase = step_increase,
      vjust = vjust,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      display.points = display_points,
      pt.alpha = pt_alpha,
      pt.color = pt_clr,
      pt.num = pt_num,
      pt.shape = pt_shape,
      pt.size = pt_size,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      verbose = verbose,
      ...
    )
    
  })

# -----

# violinplot --------------------------------------------------------------

#' @title Plot numeric distribution and statistical tests
#'
#' @inherit plotBoxplot description params details return
#'
#' @export

setGeneric(name = "plotViolinplot", def = function(object, ...){
  
  standardGeneric(f = "plotViolinplot")
  
})


#' @rdname plotViolinplot
#' @export
setMethod(
  f = "plotViolinplot",
  signature = "CyproScreening",
  definition = function(object, 
                        features,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        test_groupwise = NULL,
                        test_pairwise = NULL,
                        ref_group = NULL,
                        step_increase = 0.01,
                        vjust = 0,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        display_points = FALSE,
                        pt_alpha = 0.8,
                        pt_clr = "black",
                        pt_num = 100,
                        pt_size = 1.25,
                        pt_shape = 21,
                        verbose = TRUE,
                        ...){
    
    df <- 
      getFeatureDf(
        object = object,
        with_well_plate = TRUE,
        with_meta = TRUE,
        with_cluster = TRUE
      )
    
    plot_violinplot(
      df = df, 
      variables = features, 
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      test.pairwise = test_pairwise,
      test.groupwise = test_groupwise,
      ref.group = ref_group,
      step.increase = step_increase,
      vjust = vjust,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      display.points = display_points,
      pt.alpha = pt_alpha,
      pt.color = pt_clr,
      pt.num = pt_num,
      pt.shape = pt_shape,
      pt.size = pt_size,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      verbose = verbose,
      ...
    )
    
  })


#' @rdname plotViolinplot
#' @export
setMethod(
  f = "plotViolinplot",
  signature = "CyproTimeLapse",
  definition = function(object, 
                        features,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        test_groupwise = NULL,
                        test_pairwise = NULL,
                        ref_group = NULL,
                        step_increase = 0.01,
                        vjust = 0,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        display_points = FALSE,
                        pt_alpha = 0.8,
                        pt_clr = "black",
                        pt_num = 100,
                        pt_size = 1.25,
                        pt_shape = 21,
                        verbose = TRUE,
                        ...){
    
    df <- 
      getStatsDf(
        object = object,
        with_well_plate = TRUE,
        with_meta = TRUE,
        with_cluster = TRUE
      )
    
    plot_violinplot(
      df = df, 
      variables = features, 
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      test.pairwise = test_pairwise,
      test.groupwise = test_groupwise,
      ref.group = ref_group,
      step.increase = step_increase,
      vjust = vjust,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      display.points = display_points,
      pt.alpha = pt_alpha,
      pt.color = pt_clr,
      pt.num = pt_num,
      pt.shape = pt_shape,
      pt.size = pt_size,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      verbose = verbose,
      ...
    )
    
  })

#' @rdname plotViolinplot
#' @export
setMethod(
  f = "plotViolinplot",
  signature = "CyproTimeLapseMP",
  definition = function(object, 
                        features,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        test_groupwise = NULL,
                        test_pairwise = NULL,
                        ref_group = NULL,
                        step_increase = 0.01,
                        vjust = 0,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        display_points = FALSE,
                        pt_alpha = 0.8,
                        pt_clr = "black",
                        pt_num = 100,
                        pt_size = 1.25,
                        pt_shape = 21,
                        phase = NULL, 
                        verbose = TRUE,
                        ...){
    
    df <- 
      getStatsDf(
        object = object,
        with_well_plate = TRUE,
        with_meta = TRUE,
        with_cluster = TRUE, 
        phase = phase
      )
    
    plot_violinplot(
      df = df, 
      variables = features, 
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      test.pairwise = test_pairwise,
      test.groupwise = test_groupwise,
      ref.group = ref_group,
      step.increase = step_increase,
      vjust = vjust,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      display.points = display_points,
      pt.alpha = pt_alpha,
      pt.color = pt_clr,
      pt.num = pt_num,
      pt.shape = pt_shape,
      pt.size = pt_size,
      clrp = clrp,
      clrp.adjust = clrp_adjust,
      verbose = verbose,
      ...
    )
    
  })

# -----


# densityplot -------------------------------------------------------------

#' @title Plot numeric distribution and statistical tests
#'
#' @inherit plotBoxplot description params details return
#'
#' @export

setGeneric(name = "plotDensityplot", def = function(object, ...){
  
  standardGeneric(f = "plotDensityplot")
  
})


#' @rdname plotDensityplot
#' @export
setMethod(
  f = "plotDensityplot",
  signature = "CyproScreening",
  definition = function(object, 
                        features,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        verbose = TRUE,
                        ...){
    
    df <- 
      getFeatureDf(
        object = object,
        with_well_plate = TRUE,
        with_meta = TRUE,
        with_cluster = TRUE
      )
    
    plot_density(
      df = df, 
      variables = features, 
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      verbose = verbose,
      ...
    )
    
  })


#' @rdname plotDensityplot
#' @export
setMethod(
  f = "plotDensityplot",
  signature = "CyproTimeLapse",
  definition = function(object, 
                        features,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        verbose = TRUE,
                        ...){
    
    df <- 
      getStatsDf(
        object = object,
        with_well_plate = TRUE,
        with_meta = TRUE,
        with_cluster = TRUE
      )
    
    plot_density(
      df = df, 
      variables = features, 
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      verbose = verbose,
      ...
    )
    
  })

#' @rdname plotDensityplot
#' @export
setMethod(
  f = "plotDensityplot",
  signature = "CyproTimeLapseMP",
  definition = function(object, 
                        features,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        phase = NULL, 
                        verbose = TRUE,
                        ...){
    
    df <- 
      getStatsDf(
        object = object,
        with_well_plate = TRUE,
        with_meta = TRUE,
        with_cluster = TRUE, 
        phase = phase
      )
    
    plot_density(
      df = df, 
      variables = features, 
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      verbose = verbose,
      ...
    )
    
  })


# -----


# histogram ---------------------------------------------------------------

#' @title Plot numeric distribution and statistical tests
#'
#' @inherit plotBoxplot description params details return
#'
#' @export

setGeneric(name = "plotHistogram", def = function(object, ...){
  
  standardGeneric(f = "plotHistogram")
  
})


#' @rdname plotHistogram
#' @export
setMethod(
  f = "plotHistogram",
  signature = "CyproScreening",
  definition = function(object, 
                        features,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        verbose = TRUE,
                        ...){
    
    df <- 
      getFeatureDf(
        object = object,
        with_well_plate = TRUE,
        with_meta = TRUE,
        with_cluster = TRUE
      )
    
    plot_histogram(
      df = df, 
      variables = features, 
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      verbose = verbose,
      ...
    )
    
  })


#' @rdname plotHistogram
#' @export
setMethod(
  f = "plotHistogram",
  signature = "CyproTimeLapse",
  definition = function(object, 
                        features,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        verbose = TRUE,
                        ...){
    
    df <- 
      getStatsDf(
        object = object,
        with_well_plate = TRUE,
        with_meta = TRUE,
        with_cluster = TRUE
      )
    
    plot_histogram(
      df = df, 
      variables = features, 
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      verbose = verbose,
      ...
    )
    
  })

#' @rdname plotHistogram
#' @export
setMethod(
  f = "plotHistogram",
  signature = "CyproTimeLapseMP",
  definition = function(object, 
                        features,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        phase = NULL, 
                        verbose = TRUE,
                        ...){
    
    df <- 
      getStatsDf(
        object = object,
        with_well_plate = TRUE,
        with_meta = TRUE,
        with_cluster = TRUE, 
        phase = phase
      )
    
    plot_histogram(
      df = df, 
      variables = features, 
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      verbose = verbose,
      ...
    )
    
  })


# -----



# ridgeplot ---------------------------------------------------------------

#' @title Plot numeric distribution and statistical tests
#'
#' @inherit plotBoxplot description params details return
#'
#' @export

setGeneric(name = "plotRidgeplot", def = function(object, ...){
  
  standardGeneric(f = "plotRidgeplot")
  
})


#' @rdname plotRidgeplot
#' @export
setMethod(
  f = "plotRidgeplot",
  signature = "CyproScreening",
  definition = function(object, 
                        features,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        verbose = TRUE,
                        ...){
    
    df <- 
      getFeatureDf(
        object = object,
        with_well_plate = TRUE,
        with_meta = TRUE,
        with_cluster = TRUE
      )
    
    plot_ridgeplot(
      df = df, 
      variables = features, 
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      verbose = verbose,
      ...
    )
    
  })


#' @rdname plotRidgeplot
#' @export
setMethod(
  f = "plotRidgeplot",
  signature = "CyproTimeLapse",
  definition = function(object, 
                        features,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        verbose = TRUE,
                        ...){
    
    df <- 
      getStatsDf(
        object = object,
        with_well_plate = TRUE,
        with_meta = TRUE,
        with_cluster = TRUE
      )
    
    plot_ridgeplot(
      df = df, 
      variables = features, 
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      verbose = verbose,
      ...
    )
    
  })

#' @rdname plotRidgeplot
#' @export
setMethod(
  f = "plotRidgeplot",
  signature = "CyproTimeLapseMP",
  definition = function(object, 
                        features,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        display_facets = TRUE,
                        scales = "free",
                        nrow = NULL,
                        ncol = NULL,
                        phase = NULL, 
                        verbose = TRUE,
                        ...){
    
    df <- 
      getStatsDf(
        object = object,
        with_well_plate = TRUE,
        with_meta = TRUE,
        with_cluster = TRUE, 
        phase = phase
      )
    
    plot_ridgeplot(
      df = df, 
      variables = features, 
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      scales = scales,
      nrow = nrow,
      ncol = ncol,
      display.facets = display_facets,
      verbose = verbose,
      ...
    )
    
  })

# -----



# barchart ----------------------------------------------------------------

#' @title Plot distribution of grouping variables
#'
#' @description Visualizes the count or the proportion of barcode spots falling
#' into certain groups via barcharts. It does so either for the whole sample or
#' in a comparing manner if \code{across} is specified.
#'
#' @inherit plotBoxplot params return
#'
#' @param variables Character vector. The grouping variables whose group count or
#' proportion you want to display. Must not contain the feature specified in
#' \code{across} - if \code{across} is not set to NULL.


setGeneric(name = "plotBarchart", def = function(object, ...){
  
  standardGeneric(f = "plotBarchart")
  
})

#' @rdname plotBarchart
#' @export
setMethod(
  f = "plotBarchart",
  signature = "Cypro", 
  definition = function(object,
                        grouping_variables,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        position = "stack",
                        display_facets = TRUE,
                        ncol = NULL,
                        nrow = NULL,
                        ...){
    
    df <-
      getGroupingDf(object) %>% 
      dplyr::select(-cell_id)
    
    plot_barchart(
      df = df,
      variables = grouping_variables,
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      display.facets = display_facets,
      nrow = nrow,
      ncol = ncol,
      clrp = clrp,
      clrp.adjust = clrp_adjust, 
      position = position, 
      ...
    )
    
  })

#' @rdname plotBarchart
#' @export
setMethod(
  f = "plotBarchart",
  signature = "CyproTimeLapseMP", 
  definition = function(object,
                        grouping_variables,
                        across = NULL,
                        across_subset = NULL,
                        relevel = TRUE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        position = "stack",
                        display_facets = TRUE,
                        ncol = NULL,
                        nrow = NULL,
                        phase = NULL, 
                        verbose = TRUE,
                        ...){
    
    df <-
      getGroupingDf(object, phase = phase) %>% 
      dplyr::select(-cell_id)
    
    plot_barchart(
      df = df,
      variables = grouping_variables,
      across = across,
      across.subset = across_subset,
      relevel = relevel,
      display.facets = display_facets,
      nrow = nrow,
      ncol = ncol,
      clrp = clrp,
      clrp.adjust = clrp_adjust, 
      position = position, 
      verbose = verbose,
      ...
    )
    
  })

# -----



#' @title Plot statistics related plots interactively
#' 
#' @description Opens an interactive application in which statistic related 
#' plots like violinplots, boxplots as well as statistical tests across 
#' groups can be conducted.
#'
#' @inherit argument_dummy params
#'
#' @export
#'

setGeneric(name = "plotStatisticsInteractive", def = function(object, ...){
  
  standardGeneric(f = "plotStatisticsInteractive")
  
})

#' @rdname plotStatisticsInteractive
#' @export
setMethod(
  f = "plotStatisticsInteractive", 
  signature = "CyproScreening",
  definition = function(object, ...){
    
    df <- 
      getFeatureDf(object, with_well_plate = TRUE, with_meta = TRUE, with_cluster = TRUE) %>% 
      dplyr::select(-cell_id)
    
    plot_statistics_interactive(df, n.across.subset = 50)
    
  })

#' @rdname plotStatisticsInteractive
#' @export
setMethod(
  f = "plotStatisticsInteractive", 
  signature = "CyproTimeLapse",
  definition = function(object, ...){
    
    df <- 
      getStatsDf(object, with_well_plate = TRUE, with_meta = TRUE, with_cluster = TRUE) %>% 
      dplyr::select(-cell_id)
    
    plot_statistics_interactive(df, n.across.subset = 50)
    
  })

#' @rdname plotStatisticsInteractive
#' @export
setMethod(
  f = "plotStatisticsInteractive", 
  signature = "CyproTimeLapseMP",
  definition = function(object, phase = NULL, ...){
    
    df <- 
      getStatsDf(
        object = object,
        phase = phase, 
        with_well_plate = TRUE,
        with_meta = TRUE, 
        with_cluster = TRUE
        ) %>% 
      dplyr::select(-cell_id)
    
    plot_statistics_interactive(df, n.across.subset = 50)
    
  })

