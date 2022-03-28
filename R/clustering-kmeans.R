


# c -----------------------------------------------------------------------

#' @rdname computeClusteringKmeans
#' @export
setMethod(
  f = "computeClusteringKmeans", 
  signature = "Cypro", 
  definition = function(object, 
                        ks, 
                        methods_kmeans = "Hartigan-Wong", 
                        fset = "all_features",
                        verbose = TRUE){
    
    
    clust_obj <- getClustering(object, fset = fset)
    
    clust_obj <- 
      computeClusteringKmeans(
        object = clust_obj, 
        ks = ks, 
        methods_kmeans = methods_kmeans, 
        verbose = verbose
      )
    
    object <- setAnalysisAspect(object, analysis_aspect = clust_obj, fset = fset)
    
    return(object)
    
  }
)

#' @rdname computeClusteringKmeans
#' @export
setMethod(
  f = "computeClusteringKmeans", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, 
                        phase,
                        ks, 
                        methods_kmeans = "Hartigan-Wong", 
                        fset = "all_features",
                        verbose = TRUE){
    
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    clust_obj <- 
      computeClusteringKmeans(
        object = clust_obj, 
        ks = ks, 
        methods_kmeans = methods_kmeans, 
        verbose = verbose
      )
    
    object <- setAnalysisAspect(object, analysis_aspect = clust_obj, fset = fset, phase = phase)
    
    return(object)
    
  }
)

#' @rdname getClusterVarsKmeans
#' @export
setMethod(
  f = "getClusterVarsKmeans", 
  signature = "Cypro", 
  definition = function(object, 
                        ks, 
                        fset = "all_features",
                        methods_kmeans = "Hartigan-Wong", 
                        prefix = "", 
                        naming = "{method_kmeans}_k{k}"){
    
    clust_obj <- getClustering(object, fset = fset)
    
    out <- 
      getClusterVarsKmeans(
        object = clust_obj, 
        ks = ks, 
        methods_kmeans = methods_kmeans, 
        prefix = prefix, 
        naming = naming
      )
    
    return(out)
    
  })
  
  #' @rdname getClusterVarsKmeans
  #' @export
  setMethod(
    f = "getClusterVarsKmeans", 
    signature = "CyproTimeLapseMP", 
    definition = function(object,
                          phase,
                          ks, 
                          fset = "all_features",
                          methods_kmeans = "Hartigan-Wong", 
                          prefix = "", 
                          naming = "{method_kmeans}_k{k}"){
      
      clust_obj <- getClustering(object, fset = fset, phase = phase)
      
      out <- 
        getClusterVarsKmeans(
          object = clust_obj, 
          ks = ks, 
          methods_kmeans = methods_kmeans, 
          prefix = prefix, 
          naming = naming
        )
      
      return(out)
      
    } 
  
)

#' @rdname getKmeans
#' @export
setMethod(
  f = "getKmeans", 
  signature = "Cypro", 
  definition = function(object, 
                        fset = "all_features",
                        method_kmeans = "Hartigan-Wong", 
                        stop_if_null = TRUE){
    
    clust_obj < getClustering(object, fset = fset)
    
    out <- 
      getKmeans(
        object = clust_obj, 
        method_kmeans = method_kmeans, 
        stop_if_null = stop_if_null
      )
    
    return(out)
    
  }
)

#' @rdname getKmeans
#' @export
setMethod(
  f = "getKmeans", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, 
                        phase,
                        fset = "all_features",
                        method_kmeans = "Hartigan-Wong", 
                        stop_if_null = TRUE){
    
    clust_obj < getClustering(object, fset = fset, phase = phase)
    
    out <- 
      getKmeans(
        object = clust_obj, 
        method_kmeans = method_kmeans, 
        stop_if_null = stop_if_null
      )
    
    return(out)
    
  }
)



#' @rdname plotScreeplot
#' @export
setMethod(
  f = "plotScreeplot", 
  signature = "Cypro", 
  definition = function(object, 
                        ks,
                        fset = "all_features",
                        methods_kmeans = "Hartigan-Wong",
                        color = "steelblue",
                        display_cols = TRUE,
                        display_line = TRUE,
                        display_points = TRUE){
    
    
    clust_obj <- getClustering(object, fset = fset)
    
    plotScreeplot(
      object = clust_obj, 
      methods_kmeans = methods_kmeans, 
      color = color, 
      display_cols = display_cols, 
      display_line = display_line, 
      display_points = display_points
    )
    
  }
)

#' @rdname plotScreeplot
#' @export
setMethod(
  f = "plotScreeplot", 
  signature = "Cypro", 
  definition = function(object, 
                        phase,
                        ks,
                        fset = "all_features",
                        methods_kmeans = "Hartigan-Wong",
                        color = "steelblue",
                        display_cols = TRUE,
                        display_line = TRUE,
                        display_points = TRUE){
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    plotScreeplot(
      object = clust_obj, 
      ks = ks,
      methods_kmeans = methods_kmeans, 
      color = color, 
      display_cols = display_cols, 
      display_line = display_line, 
      display_points = display_points
    )
    
  }
)
