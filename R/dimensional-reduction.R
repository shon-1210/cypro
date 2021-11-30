
#' @rdname computePCA
#' @export
setMethod(
  f = "computePCA", 
  signature = "Cypro", 
  definition = function(object, 
                        n_dims,
                        fset = "all_features", 
                        verbose = TRUE, 
                        ...){
    
    dimred_obj <- getDimRed(object, fset = fset)
    
    dimred_obj <- 
      computePCA(
        object = dimred_obj, 
        n_dims = n_dims, 
        verbose = verbose
      )
    
    object <- setAnalysisAspect(object, analysis_aspect = dimred_obj, fset = fset)
    
    return(object)
    
  }
)

#' @rdname computePCA
#' @export
setMethod(
  f = "computePCA", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, 
                        phase,
                        n_dims,
                        fset = "all_features", 
                        verbose = TRUE, 
                        ...){
    
    dimred_obj <- getDimRed(object, fset = fset, phase = phase)
    
    dimred_obj <- 
      computePCA(
        object = dimred_obj, 
        n_dims = n_dims, 
        verbose = verbose
      )
    
    object <- setAnalysisAspect(object, analysis_aspect = dimred_obj, fset = fset, phase = phase)
    
    return(object)
    
  }
)


#' @rdname computeTSNE
#' @export
setMethod(
  f = "computeTSNE", 
  signature = "Cypro", 
  definition = function(object, 
                        n_dims,
                        fset = "all_features",
                        ...){
    
    dimred_obj <- getDimRed(object, fset = fset)
    
    dimred_obj <- computeTSNE(object = dimred_obj, n_dims = n_dims, ...)
    
    object <- setAnalysisAspect(object, analysis_aspect = dimred_obj, fset = fset)
    
    return(object)
    
  }
)

#' @rdname computeTSNE
#' @export
setMethod(
  f = "computeTSNE", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, 
                        phase,
                        n_dims,
                        fset = "all_features",
                        ...){
    
    dimred_obj <- getDimRed(object, fset = fset, phase = phase)
    
    dimred_obj <- computeTSNE(object = dimred_obj, n_dims = n_dims, ...)
    
    object <- setAnalysisAspect(object, analysis_aspect = dimred_obj, fset = fset, phase = phase)
    
    return(object)
    
  }
)


#' @rdname computeUMAP
#' @export
setMethod(
  f = "computeUMAP", 
  signature = "Cypro", 
  definition = function(object, 
                        fset = "all_features",
                        verbose = TRUE,
                        ...){
    
    dimred_obj <- getDimRed(object, fset = fset)
    
    dimred_obj <- computeUMAP(object = dimred_obj, verbose = verbose, ...)
    
    object <- setAnalysisAspect(object, analysis_aspect = dimred_obj, fset = fset)
    
    return(object)
    
  }
)

#' @rdname computeUMAP
#' @export
setMethod(
  f = "computeUMAP", 
  signature = "CyproTimeLapseMP", 
  definition = function(object,
                        phase,
                        fset = "all_features", 
                        verbose = TRUE,
                        ...){
    
    dimred_obj <- getDimRed(object, fset = fset, phase = phase)
    
    dimred_obj <- computeUMAP(object = dimred_obj, verbose = verbose, ...)
    
    object <- setAnalysisAspect(object, analysis_aspect = dimred_obj, fset = fset, phase = phase)
    
    return(object)
    
  }
)


#' @rdname getEmbeddingDf
#' @export
setMethod(
  f = "getEmbeddingDf", 
  signature = "Cypro", 
  definition = function(object, 
                        fset = "all_features", 
                        method_dimred = "pca",
                        numeric = FALSE,
                        numeric_scaled = FALSE,
                        grouping = FALSE,
                        logical = FALSE,
                        complete = FALSE,
                        shift = FALSE){
    
    dimred_obj <- getDimRed(object, fset = fset)
    
    out <- 
      getEmbeddingDf(
        object = dimred_obj, 
        method_dimred = method_dimred, 
        numeric = numeric, 
        numeric_scaled = numeric_scaled, 
        grouping = grouping, 
        logical = logical, 
        complete = complete, 
        shift = shift
      )
    
    return(out)
    
  }
)

#' @rdname getEmbeddingDf
#' @export
setMethod(
  f = "getEmbeddingDf", 
  signature = "CyproTimeLapseMP", 
  definition = function(object,
                        phase,
                        fset = "all_features", 
                        method_dimred = "pca",
                        numeric = FALSE,
                        numeric_scaled = FALSE,
                        grouping = FALSE,
                        logical = FALSE,
                        complete = FALSE,
                        shift = FALSE){
    
    dimred_obj <- getDimRed(object, fset = fset, phase = phase)
    
    out <- 
      getEmbeddingDf(
        object = dimred_obj, 
        method_dimred = method_dimred, 
        numeric = numeric, 
        numeric_scaled = numeric_scaled, 
        grouping = grouping, 
        logical = logical, 
        complete = complete, 
        shift = shift
      )
    
    return(out)
    
  }
)


#' @rdname plotPCA
#' @export
setMethod(
  f = "plotPCA", 
  signature = "Cypro",
  definition = function(object, 
                        fset = "all_features", 
                        n_dims = 2,
                        alpha_by = NULL,
                        color_by = NULL,
                        shape_by = NULL,
                        size_by = NULL,
                        pt_alpha = 0.9,
                        pt_color = "black",
                        pt_fill = "black",
                        pt_shape = 19,
                        pt_size = 1,
                        color_aes = "color",
                        clrp = "milo",
                        clrp_adjust = NULL,
                        clrsp = "inferno",
                        ...){
    
    dimred_obj <- getDimRed(object, fset = fset)
    
    plotPCA(
      object = dimred_obj, 
      n_dims = n_dims, 
      alpha_by = alpha_by, 
      color_by = color_by, 
      shape_by = shape_by, 
      size_by = size_by, 
      pt_alpha = pt_alpha, 
      pt_color = pt_color, 
      pt_fill = pt_fill, 
      pt_shape = pt_shape, 
      pt_size = pt_size, 
      color_aes = color_aes, 
      clrp = clrp, 
      clrp_adjust = clrp_adjust, 
      clrsp = clrsp, 
      ...
    )
    
  }
)

#' @rdname plotPCA
#' @export
setMethod(
  f = "plotPCA", 
  signature = "CyproTimeLapseMP",
  definition = function(object,
                        phase,
                        fset = "all_features", 
                        n_dims = 2,
                        alpha_by = NULL,
                        color_by = NULL,
                        shape_by = NULL,
                        size_by = NULL,
                        pt_alpha = 0.9,
                        pt_color = "black",
                        pt_fill = "black",
                        pt_shape = 19,
                        pt_size = 1,
                        color_aes = "color",
                        clrp = "milo",
                        clrp_adjust = NULL,
                        clrsp = "inferno",
                        ...){
    
    dimred_obj <- getDimRed(object, fset = fset, phase = phase)
    
    plotPCA(
      object = dimred_obj, 
      n_dims = n_dims, 
      alpha_by = alpha_by, 
      color_by = color_by, 
      shape_by = shape_by, 
      size_by = size_by, 
      pt_alpha = pt_alpha, 
      pt_color = pt_color, 
      pt_fill = pt_fill, 
      pt_shape = pt_shape, 
      pt_size = pt_size, 
      color_aes = color_aes, 
      clrp = clrp, 
      clrp_adjust = clrp_adjust, 
      clrsp = clrsp, 
      ...
    )
    
  }
)

#' @rdname plotTSNE
#' @export
setMethod(
  f = "plotTSNE", 
  signature = "Cypro",
  definition = function(object, 
                        fset = "all_features", 
                        n_dims = 2,
                        alpha_by = NULL,
                        color_by = NULL,
                        shape_by = NULL,
                        size_by = NULL,
                        pt_alpha = 0.9,
                        pt_color = "black",
                        pt_fill = "black",
                        pt_shape = 19,
                        pt_size = 1,
                        color_aes = "color",
                        clrp = "milo",
                        clrp_adjust = NULL,
                        clrsp = "inferno",
                        ...){
    
    dimred_obj <- getDimRed(object, fset = fset)
    
    plotTSNE(
      object = dimred_obj, 
      n_dims = n_dims, 
      alpha_by = alpha_by, 
      color_by = color_by, 
      shape_by = shape_by, 
      size_by = size_by, 
      pt_alpha = pt_alpha, 
      pt_color = pt_color, 
      pt_fill = pt_fill, 
      pt_shape = pt_shape, 
      pt_size = pt_size, 
      color_aes = color_aes, 
      clrp = clrp, 
      clrp_adjust = clrp_adjust, 
      clrsp = clrsp, 
      ...
    )
    
  }
)

#' @rdname plotTSNE
#' @export
setMethod(
  f = "plotTSNE", 
  signature = "CyproTimeLapseMP",
  definition = function(object,
                        phase,
                        fset = "all_features", 
                        n_dims = 2,
                        alpha_by = NULL,
                        color_by = NULL,
                        shape_by = NULL,
                        size_by = NULL,
                        pt_alpha = 0.9,
                        pt_color = "black",
                        pt_fill = "black",
                        pt_shape = 19,
                        pt_size = 1,
                        color_aes = "color",
                        clrp = "milo",
                        clrp_adjust = NULL,
                        clrsp = "inferno",
                        ...){
    
    dimred_obj <- getDimRed(object, fset = fset, phase = phase)
    
    plotTSNE(
      object = dimred_obj, 
      n_dims = n_dims, 
      alpha_by = alpha_by, 
      color_by = color_by, 
      shape_by = shape_by, 
      size_by = size_by, 
      pt_alpha = pt_alpha, 
      pt_color = pt_color, 
      pt_fill = pt_fill, 
      pt_shape = pt_shape, 
      pt_size = pt_size, 
      color_aes = color_aes, 
      clrp = clrp, 
      clrp_adjust = clrp_adjust, 
      clrsp = clrsp, 
      ...
    )
    
  }
)

#' @rdname plotUMAP
#' @export
setMethod(
  f = "plotUMAP", 
  signature = "Cypro",
  definition = function(object, 
                        fset = "all_features", 
                        alpha_by = NULL,
                        color_by = NULL,
                        shape_by = NULL,
                        size_by = NULL,
                        pt_alpha = 0.9,
                        pt_color = "black",
                        pt_fill = "black",
                        pt_shape = 19,
                        pt_size = 1,
                        color_aes = "color",
                        clrp = "milo",
                        clrp_adjust = NULL,
                        clrsp = "inferno",
                        ...){
    
    dimred_obj <- getDimRed(object, fset = fset)
    
    plotUMAP(
      object = dimred_obj, 
      alpha_by = alpha_by, 
      color_by = color_by, 
      shape_by = shape_by, 
      size_by = size_by, 
      pt_alpha = pt_alpha, 
      pt_color = pt_color, 
      pt_fill = pt_fill, 
      pt_shape = pt_shape, 
      pt_size = pt_size, 
      color_aes = color_aes, 
      clrp = clrp, 
      clrp_adjust = clrp_adjust, 
      clrsp = clrsp, 
      ...
    )
    
  }
)

#' @rdname plotUMAP
#' @export
setMethod(
  f = "plotUMAP", 
  signature = "CyproTimeLapseMP",
  definition = function(object,
                        phase,
                        fset = "all_features", 
                        alpha_by = NULL,
                        color_by = NULL,
                        shape_by = NULL,
                        size_by = NULL,
                        pt_alpha = 0.9,
                        pt_color = "black",
                        pt_fill = "black",
                        pt_shape = 19,
                        pt_size = 1,
                        color_aes = "color",
                        clrp = "milo",
                        clrp_adjust = NULL,
                        clrsp = "inferno",
                        ...){
    
    dimred_obj <- getDimRed(object, fset = fset, phase = phase)
    
    plotUMAP(
      object = dimred_obj, 
      alpha_by = alpha_by, 
      color_by = color_by, 
      shape_by = shape_by, 
      size_by = size_by, 
      pt_alpha = pt_alpha, 
      pt_color = pt_color, 
      pt_fill = pt_fill, 
      pt_shape = pt_shape, 
      pt_size = pt_size, 
      color_aes = color_aes, 
      clrp = clrp, 
      clrp_adjust = clrp_adjust, 
      clrsp = clrsp, 
      ...
    )
    
  }
)