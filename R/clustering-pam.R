

#' @rdname computeClusteringPam
#' @export
setMethod(
  f = "computeClusteringPam", 
  signature = "Cypro", 
  definition = function(object, 
                        ks, 
                        methods_pam = "euclidean",
                        fset = "all_features",
                        verbose = TRUE){
    
    clust_obj <- getClustering(object, fset = fset)
    
    clust_obj <- 
      computeClusteringPam(
        object = clust_obj, 
        ks = ks, 
        methods_pam = methods_pam, 
        verbose = verbose
      )
    
    object <- setAnalysisAspect(object, analysis_aspect = clust_obj, fset = fset)
    
    return(object)
    
  }
)

#' @rdname computeClusteringPam
#' @export
setMethod(
  f = "computeClusteringPam", 
  signature = "CyproTimeLapseMP", 
  definition = function(object,
                        phase,
                        ks, 
                        methods_pam = "euclidean",
                        fset = "all_features",
                        verbose = TRUE){
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    clust_obj <- 
      computeClusteringPam(
        object = clust_obj, 
        ks = ks, 
        methods_pam = methods_pam, 
        verbose = verbose
      )
    
    object <- setAnalysisAspect(object, analysis_aspect = clust_obj, fset = fset, phase = phase)
    
    return(object)
    
  }
)


#' @rdname getClusterVarsPam
#' @export
setMethod(
  f = "getClusterVarsPam", 
  signature = "Cypro", 
  definition = function(object, 
                        ks, 
                        methods_pam = "euclidean",
                        fset = "all_features",
                        prefix = "", 
                        naming = "{method_pam}_k{k}"){
    
    clust_obj <- getClustering(object, fset = fset)
    
    out <- 
      getClusterVarsPam(
        object = clust_obj, 
        ks = ks, 
        methods_pam = methods_pam, 
        prefix = prefix, 
        naming = naming
      )
    
    return(out)
    
  }
)

#' @rdname getClusterVarsPam
#' @export
setMethod(
  f = "getClusterVarsPam", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, 
                        phase,
                        ks, 
                        methods_pam = "euclidean",
                        fset = "all_features",
                        prefix = "", 
                        naming = "{method_pam}_k{k}"){
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    out <- 
      getClusterVarsPam(
        object = clust_obj, 
        ks = ks, 
        methods_pam = methods_pam, 
        prefix = prefix, 
        naming = naming
      )
    
    return(out)
    
  }
)


#' @rdname getPam
#' @export
setMethod(
  f = "getPam", 
  signature = "Cypro",
  definition = function(object, 
                        k, 
                        method_pam = "euclidean",
                        fset = "all_features",
                        stop_if_null = TRUE){
    
    clust_obj <- getClustering(object, fset = fset)
    
    out <- 
      getPam(
        object = clust_obj, 
        k = k, 
        method_pam = method_pam, 
        stop_if_null = stop_if_null
      )
    
    return(out)
    
  }
)

#' @rdname getPam
#' @export
setMethod(
  f = "getPam", 
  signature = "CyproTimeLapseMP",
  definition = function(object, 
                        phase,
                        k, 
                        method_pam = "euclidean",
                        fset = "all_features",
                        stop_if_null = TRUE){
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    out <- 
      getPam(
        object = clust_obj, 
        k = k, 
        method_pam = method_pam, 
        stop_if_null = stop_if_null
      )
    
    return(out)
    
  }
)


#' @rdname getSilWidthsDf
#' @export
setMethod(
  f = "getSilWidthsDf", 
  signature = "Cypro",
  definition = function(object, 
                        ks, 
                        method_pam = "euclidean",
                        fset = "all_features"){
    
    clust_obj <- getClustering(object, fset = fset)
    
    out <- 
      getSilWidthsDf(
        object = clust_obj, 
        ks = ks, 
        method_pam = method_pam
      )
    
    return(out)
    
  }
)

#' @rdname getSilWidthsDf
#' @export
setMethod(
  f = "getSilWidthsDf", 
  signature = "CyproTimeLapseMP",
  definition = function(object, 
                        phase,
                        ks, 
                        method_pam = "euclidean",
                        fset = "all_features"){
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    out <- 
      getSilWidthsDf(
        object = clust_obj, 
        ks = ks, 
        method_pam = method_pam
      )
    
    return(out)
    
  }
)


#' @rdname plotAvgSilWidths
#' @export
setMethod(
  f = "plotAvgSilWidths", 
  signature = "Cypro", 
  definition = function(object,
                        ks,
                        methods_pam = "euclidean",
                        fset = "all_features",
                        color = "steelblue",
                        display_cols = TRUE,
                        display_points = TRUE,
                        display_line = TRUE,
                        ncol = NULL,
                        nrow = NULL){
    
    clust_obj <- getClustering(object, fset = fset)
    
    plotAvgSilWidths(
      object = clust_obj,
      ks = ks, 
      methods_pam = methods_pam, 
      color = color, 
      display_cols = display_cols, 
      display_points = display_points, 
      display_line, 
      ncol = ncol, 
      nrow = nrow
    )
  }
)

#' @rdname plotAvgSilWidths
#' @export
setMethod(
  f = "plotAvgSilWidths", 
  signature = "CyproTimeLapseMP", 
  definition = function(object,
                        phase,
                        ks,
                        methods_pam = "euclidean",
                        fset = "all_features",
                        color = "steelblue",
                        display_cols = TRUE,
                        display_points = TRUE,
                        display_line = TRUE,
                        ncol = NULL,
                        nrow = NULL){
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    plotAvgSilWidths(
      object = clust_obj,
      ks = ks, 
      methods_pam = methods_pam, 
      color = color, 
      display_cols = display_cols, 
      display_points = display_points, 
      display_line, 
      ncol = ncol, 
      nrow = nrow
    )
  }
)

#' @rdname plotSilWidths
#' @export
setMethod(
  f = "plotSilWidths",
  signature = "Cypro",
  definition = function(object, 
                        ks,
                        method_pam = "euclidean",
                        fset = "all_features",
                        clrp = "milo",
                        ncol = NULL,
                        nrow = NULL,
                        verbose = TRUE){
    
    clust_obj <- getClustering(object, fset = fset)
    
    plotSilWidths(
      object = clust_obj, 
      ks = ks, 
      method_pam = method_pam, 
      clrp = clrp, 
      ncol = ncol, 
      nrow = nrow, 
      verbose = verbose
    )
    
  }
)

#' @rdname plotSilWidths
#' @export
setMethod(
  f = "plotSilWidths",
  signature = "CyproTimeLapseMP",
  definition = function(object,
                        phase,
                        ks,
                        method_pam = "euclidean",
                        fset = "all_features",
                        clrp = "milo",
                        ncol = NULL,
                        nrow = NULL,
                        verbose = TRUE){
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    plotSilWidths(
      object = clust_obj, 
      ks = ks, 
      method_pam = method_pam, 
      clrp = clrp, 
      ncol = ncol, 
      nrow = nrow, 
      verbose = verbose
    )
    
  }
)

