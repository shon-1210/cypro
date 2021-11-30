#' @include imported-generics.R
NULL

# c -----------------------------------------------------------------------


#' @rdname agglomerateHierarchicalTrees
#' @export
setMethod(
  f = "agglomerateHierarchicalTrees", 
  signature = "Cypro", 
  definition = function(object, 
                        fset = "all_features",
                        methods_dist = "euclidean", 
                        methods_aggl = "ward.D", 
                        verbose = TRUE){
    
    clust_obj <- getClustering(object, fset = fset)
    
    clust_obj <- 
      agglomerateHierarchicalTrees(
        object = clust_obj, 
        methods_dist = methods_dist, 
        methods_aggl = methods_aggl, 
        verbose = verbose
        )
    
    object <- setAnalysisAspect(object, analysis_aspect = clust_obj, fset = fset)
    
    return(object)
    
  }
)

#' @rdname agglomerateHierarchicalTrees
#' @export
setMethod(
  f = "agglomerateHierarchicalTrees", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, 
                        phase, 
                        fset = "all_features",
                        methods_dist = "euclidean", 
                        methods_aggl = "ward.D", 
                        verbose = TRUE){
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    clust_obj <- 
      agglomerateHierarchicalTrees(
        object = clust_obj, 
        methods_dist = methods_dist, 
        methods_aggl = methods_aggl, 
        verbose = verbose
      )
    
    object <- setAnalysisAspect(object, analysis_aspect = clust_obj, fset = fset, phase = phase)
    
    return(object)
    
  }
)


# c -----------------------------------------------------------------------


#' @rdname computeClusteringHclust
#' @export
setMethod(
  f = "computeClusteringHclust",
  signature = "Cypro", 
  definition = function(object, 
                        fset = "all_features",
                        methods_dist = "euclidean",
                        methods_aggl = "ward.D", 
                        verbose = TRUE){
    
    clust_obj <- getClustering(object, fset = fset)
    
    clust_obj <- 
      computeClusteringHclust(
        object = clust_obj, 
        methods_dist = methods_dist, 
        methods_aggl = methods_aggl, 
        verbose = verbose
      )
    
    object <- setAnalysisAspect(object, analysis_aspect = clust_obj, fset = fset)
    
    return(object)
    
  }
)

#' @rdname computeClusteringHclust
#' @export
setMethod(
  f = "computeClusteringHclust",
  signature = "CyproTimeLapseMP", 
  definition = function(object,
                        phase,
                        fset = "all_features",
                        methods_dist = "euclidean",
                        methods_aggl = "ward.D", 
                        verbose = TRUE){
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    clust_obj <- 
      computeClusteringHclust(
        object = clust_obj, 
        methods_dist = methods_dist, 
        methods_aggl = methods_aggl, 
        verbose = verbose
      )
    
    object <- setAnalysisAspect(object, analysis_aspect = clust_obj, fset = fset, phase = phase)
    
    return(object)
    
  }
)

#' @rdname computeDistanceMatrices
#' @export
setMethod(
  f = "computeDistanceMatrices", 
  signature = "Cypro", 
  definition = function(object, 
                        fset = "all_features",
                        methods_dist = "euclidean", 
                        force = FALSE, 
                        verbose = TRUE){
    
    clust_obj <- getClustering(object, fset = fset)
    
    clust_obj <- 
      computeDistanceMatrices(
        object = clust_obj, 
        methods_dist = methods_dist, 
        force = force, 
        verbose = verbose
      )
    
    object <- setAnalysisAspect(object, analysis_aspect = clust_obj, fset = fset)
    
    return(object)
    
  }
)

#' @rdname computeDistanceMatrices
#' @export
setMethod(
  f = "computeDistanceMatrices", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, 
                        phase,
                        fset = "all_features",
                        methods_dist = "euclidean", 
                        force = FALSE, 
                        verbose = TRUE){
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    clust_obj <- 
      computeDistanceMatrices(
        object = clust_obj, 
        methdos_dist = methods_dist, 
        force = force, 
        verbose = verbose
      )
    
    object <- setAnalysisAspect(object, analysis_aspect = clust_obj, fset = fset, phase = phase)
    
    return(object)
    
  }
)



# g -----------------------------------------------------------------------


#' @rdname getClusterVarsHclust
#' @export
setMethod(
  f = "getClusterVarsHclust", 
  signature = "Cypro", 
  definition = function(object,
                        fset = "all_features",
                        ks = NULL, 
                        hs = NULL, 
                        methods_dist = "euclidean", 
                        methods_aggl = "ward.D", 
                        prefix = "", 
                        naming_k = "{method_dist}_{method_aggl}_k{k}", 
                        nmaing_h = "{method_dist}_{method_aggl}_h{h}"){
    
    clust_obj <- getClustering(object, fset = fset)
    
    out <- 
      getClusterVarsHclust(
        object = clust_obj, 
        ks = ks, 
        hs = hs, 
        methods_dist = methods_dist, 
        methods_aggl = methods_aggl, 
        prefix = prefix, 
        naming_k = naming_k, 
        naming_h = naming_h
      )
    
  }
)

#' @rdname getClusterVarsHclust
#' @export
setMethod(
  f = "getClusterVarsHclust", 
  signature = "CyproTimeLapseMP", 
  definition = function(object,
                        phase,
                        fset = "all_features",
                        ks = NULL, 
                        hs = NULL, 
                        methods_dist = "euclidean", 
                        methdos_aggl = "ward.D", 
                        prefix = "", 
                        naming_k = "{method_dist}_{method_aggl}_k{k}", 
                        nmaing_h = "{method_dist}_{method_aggl}_h{h}"){
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    out <- 
      getClusterVarsHclust(
        object = clust_obj, 
        ks = ks, 
        hs = hs, 
        methods_dist = methods_dist, 
        methods_aggl = methods_aggl, 
        prefix = prefix, 
        naming_k = naming_k, 
        naming_h = naming_h
      )
    
  }
)

#' @rdname getDendro
#' @export
setMethod(
  f = "getDendro", 
  signature = "Cypro", 
  definition = function(object, 
                        fset = "all_features",
                        method_dist = "euclidean",
                        method_aggl = "ward.D",
                        k = NULL, 
                        h = NULL, 
                        type = "rectangle"){
    
    clust_obj <- getClustering(object, fset = fset)
    
    out <- 
      getDendro(
        object = clust_obj,
        method_dist = method_dist, 
        method_aggl = method_aggl, 
        k = k, 
        h = h, 
        type = type
        )
    
    return(out)
    
  }
)

#' @rdname getDendro
#' @export
setMethod(
  f = "getDendro", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, 
                        phase, 
                        fset = "all_features",
                        method_dist = "euclidean",
                        method_aggl = "ward.D",
                        k = NULL, 
                        h = NULL, 
                        type = "rectangle"){
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    out <- 
      getDendro(
        object = clust_obj,
        method_dist = method_dist, 
        method_aggl = method_aggl, 
        k = k, 
        h = h, 
        type = type
      )
    
    return(out)
    
  }
)

#' @rdname getDendroSegmentDf
#' @export
setMethod(
  f = "getDendroSegmentDf", 
  signature = "Cypro", 
  definition = function(object,
                        fset = "all_features",
                        methods_dist = "euclidean", 
                        methods_aggl = "ward.D", 
                        k = NULL, 
                        h = NULL, 
                        type = "rectangle"){
    
    clust_obj <- getClustering(object, fset = fset)
    
    out <- 
      getDendroSegmentDf(
        object = clust_obj, 
        methods_dist = methods_dist, 
        methods_aggl = methods_aggl, 
        k = k, 
        h = h, 
        type = type
      )
    
    return(out)
    
  }
)

#' @rdname getDendroSegmentDf
#' @export
setMethod(
  f = "getDendroSegmentDf", 
  signature = "CyproTimeLapseMP", 
  definition = function(object,
                        phase,
                        fset = "all_features",
                        methods_dist = "euclidean", 
                        methods_aggl = "ward.D", 
                        k = NULL, 
                        h = NULL, 
                        type = "rectangle"){
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    out <- 
      getDendroSegmentDf(
        object = clust_obj, 
        methods_dist = methods_dist, 
        methods_aggl = methods_aggl, 
        k = k, 
        h = h, 
        type = type
      )
    
    return(out)
    
  }
)


#' @rdname getDistMtr
#' @export
setMethod(
  f = "getDistMtr",
  signature = "Cypro", 
  definition = function(object, 
                        fset = "all_features",
                        method_dist = "euclidean", 
                        stop_if_null = TRUE){
    
    clust_obj <- getClustering(object, fset = fset)
    
    out <- 
      getDistMtr(
        object = clust_obj, 
        method_dist = method_dist, 
        stop_if_null = stop_if_null
      )
    
    return(out)
    
  }
)

#' @rdname getDistMtr
#' @export
setMethod(
  f = "getDistMtr",
  signature = "CyproTimeLapseMP", 
  definition = function(object, 
                        fset = "all_features",
                        method_dist = "euclidean", 
                        stop_if_null = TRUE){
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    out <- 
      getDistMtr(
        object = clust_obj, 
        method_dist = method_dist, 
        stop_if_null = stop_if_null
      )
    
    return(out)
    
  }
)

#' @rdname getHclust
#' @export
setMethod(
  f = "getHclust", 
  signature = "Cypro", 
  definition = function(object, 
                        fset = "all_features",
                        method_dist = "euclidean", 
                        method_aggl = "ward.D", 
                        stop_if_null = TRUE){
    
    clust_obj <- getClustering(object)
    
    out <- 
      getHclust(
        object = clust_obj, 
        method_dist = method_dist, 
        method_aggl = method_aggl, 
        stop_if_null = stop_if_null
      )
    
    return(out)
    
  }
)

#' @rdname getHclust
#' @export
setMethod(
  f = "getHclust", 
  signature = "Cypro", 
  definition = function(object, 
                        phase,
                        fset = "all_features",
                        method_dist = "euclidean", 
                        method_aggl = "ward.D", 
                        stop_if_null = TRUE){
    
    clust_obj <- getClustering(object)
    
    out <- 
      getHclust(
        object = clust_obj, 
        method_dist = method_dist, 
        method_aggl = method_aggl, 
        stop_if_null = stop_if_null
      )
    
    return(out)
    
  }
)




# p -----------------------------------------------------------------------


#' @rdname plotDendrogram
#' @export
setMethod(
  f = "plotDendrogram", 
  signature = "Cypro", 
  definition = function(object, 
                        fset = "all_features",
                        methods_dist = "euclidean",
                        methods_aggl = "ward.D",
                        k = NULL,
                        h = NULL,
                        type = "rectangle",
                        direction = "bt",
                        branch_color = "black",
                        branch_size = 1,
                        display_labels = FALSE,
                        labels_angle = 90,
                        labels_hjust = 0,
                        labels_nudge = 0.01,
                        labels_size = 3,
                        labels_vjust = 0.5,
                        display_title = FALSE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        simple = FALSE,
                        facet_with = "grid",
                        nrow = NULL,
                        ncol = NULL,
                        ...){
    
    
    clust_obj <- getClustering(object, fset = fset)
    
    plotDendrogram(
      object = clust_obj, 
      methods_dist = methods_dist, 
      methods_aggl = methods_aggl, 
      k = k, 
      h = h, 
      type = type, 
      direction = direction, 
      branch_color = branch_color, 
      branch_size = branch_size, 
      display_labels = display_labels, 
      labels_angle = labels_angle, 
      labels_hjust = labels_hjust, 
      labels_nudge = labels_nudge, 
      labels_size = labels_size, 
      labels_vjust = labels_vjust, 
      display_title = display_title, 
      clrp = clrp, 
      clrp_adjust = clrp_adjust, 
      simple = simple, 
      facet_with = facet_with, 
      nrow = nrow, 
      ncol = ncol, 
      ...
    )
    
  }
)

#' @rdname plotDendrogram
#' @export
setMethod(
  f = "plotDendrogram", 
  signature = "Cypro", 
  definition = function(object, 
                        phase,
                        fset = "all_features",
                        methods_dist = "euclidean",
                        methods_aggl = "ward.D",
                        k = NULL,
                        h = NULL,
                        type = "rectangle",
                        direction = "bt",
                        branch_color = "black",
                        branch_size = 1,
                        display_labels = FALSE,
                        labels_angle = 90,
                        labels_hjust = 0,
                        labels_nudge = 0.01,
                        labels_size = 3,
                        labels_vjust = 0.5,
                        display_title = FALSE,
                        clrp = "milo",
                        clrp_adjust = NULL,
                        simple = FALSE,
                        facet_with = "grid",
                        nrow = NULL,
                        ncol = NULL,
                        ...){
    
    
    clust_obj <- getClustering(object, fset = fset, phase = phase)
    
    plotDendrogram(
      object = clust_obj, 
      methods_dist = methods_dist, 
      methods_aggl = methods_aggl, 
      k = k, 
      h = h, 
      type = type, 
      direction = direction, 
      branch_color = branch_color, 
      branch_size = branch_size, 
      display_labels = display_labels, 
      labels_angle = labels_angle, 
      labels_hjust = labels_hjust, 
      labels_nudge = labels_nudge, 
      labels_size = labels_size, 
      labels_vjust = labels_vjust, 
      display_title = display_title, 
      clrp = clrp, 
      clrp_adjust = clrp_adjust, 
      simple = simple, 
      facet_with = facet_with, 
      nrow = nrow, 
      ncol = ncol, 
      ...
    )
    
  }
)