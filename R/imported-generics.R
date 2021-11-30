

# a -----------------------------------------------------------------------

#' @export
setGeneric(
  name = "agglomerateHierarchicalTrees",
  def = getGeneric(f = "agglomerateHierarchicalTrees", package = "confuns")
  )


# c -----------------------------------------------------------------------

#' @export
setGeneric(
  name = "computeClusteringHclust",
  def = getGeneric(f = "computeClusteringHclust", package = "confuns")
)

#' @export
setGeneric(
  name = "computeClusteringKmeans",
  def = getGeneric(f = "computeClusteringKmeans", package = "confuns")
)

#' @export
setGeneric(
  name = "computeClusteringPam",
  def = getGeneric(f = "computeClusteringPam", package = "confuns")
)

#' @export
setGeneric(
  name = "computeDistanceMatrices",
  def = getGeneric(f = "computeDistanceMatrices", package = "confuns")
)

#' @inherit confuns::computeCorrelation title description details params return
#' @export
setGeneric(
  name = "computeCorrelation",
  def = getGeneric(f = "computeCorrelation", package = "confuns")
)

#' @export
setGeneric(
  name = "computePCA",
  def = getGeneric(f = "computePCA", package = "confuns")
)

#' @export
setGeneric(
  name = "computeTSNE",
  def = getGeneric(f = "computeTSNE", package = "confuns")
)

#' @export
setGeneric(
  name = "computeUMAP",
  def = getGeneric(f = "computeUMAP", package = "confuns")
)


# g -----------------------------------------------------------------------

#' @export
setGeneric(
  name = "getAvgSilWidthsDf",
  def = getGeneric(f = "getAvgSilWidthsDf", package = "confuns")
)

#' @export
setGeneric(
  name = "getClusteringHclust",
  def = getGeneric(f = "getClusteringHclust", package = "confuns")
)

#' @export
setGeneric(
  name = "getClusteringKmeans",
  def = getGeneric(f = "getClusteringKmeans", package = "confuns")
)

#' @export
setGeneric(
  name = "getClusterVarsHclust",
  def = getGeneric(f = "getClusterVarsHclust", package = "confuns")
)

#' @export
setGeneric(
  name = "getClusterVarsKmeans",
  def = getGeneric(f = "getClusterVarsKmeans", package = "confuns")
)

#' @export
setGeneric(
  name = "getClusterVarsKmeans",
  def = getGeneric(f = "getClusterVarsKmeans", package = "confuns")
)

#' @export
setGeneric(
  name = "getClusterVarsPam",
  def = getGeneric(f = "getClusterVarsPam", package = "confuns")
)

#' @inherit confuns::getCorrDf title description params details return 
#' @export
setGeneric(
  name = "getCorrDf",
  def = getGeneric(f = "getCorrDf", package = "confuns")
)

#' @inherit confuns::getCorrMtr title description params details return
#' @export
setGeneric(
  name = "getCorrMtr",
  def = getGeneric(f = "getCorrMtr", package = "confuns")
)

#' @export
setGeneric(
  name = "getDendro",
  def = getGeneric(f = "getDendro", package = "confuns")
)

#' @export
setGeneric(
  name = "getDendroSegmentDf",
  def = getGeneric(f = "getDendroSegmentDf", package = "confuns")
)

#' @export
setGeneric(
  name = "getDf",
  def = getGeneric(f = "getDf", package = "confuns")
)

#' @export
setGeneric(
  name = "getDistMtr",
  def = getGeneric(f = "getDistMtr", package = "confuns")
)

#' @export
setGeneric(
  name = "getEmbeddingDf",
  def = getGeneric(f = "getEmbeddingDf", package = "confuns")
)

#' @export
setGeneric(
  name = "getHclust",
  def = getGeneric(f = "getHclust", package = "confuns")
)

#' @export
setGeneric(
  name = "getKeyDf",
  def = getGeneric(f = "getKeyDf", package = "confuns")
)

#' @export
setGeneric(
  name = "getKmeans",
  def = getGeneric(f = "getKmeans", package = "confuns")
)

#' @export
setGeneric(
  name = "getMtr",
  def = getGeneric(f = "getMtr", package = "confuns")
)

#' @export
setGeneric(
  name = "getPam",
  def = getGeneric(f = "getPam", package = "confuns")
)

#' @inherit confuns::getRcorr title description params details return 
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getRcorr",
  def = getGeneric(f = "getRcorr", package = "confuns")
)

#' @export
setGeneric(
  name = "getResults",
  def = getGeneric(f = "getResults", package = "confuns")
)

#' @title Obtain scaled data
#' 
#' @description Extracts scaled data as a data.frame. In case of screening experiments 
#' it's the scaled data from slot @@features. In case of time lapse 
#' experiments its the scaled data from slot @@stat_features.
#' 
#' @inherit argument_dummy params
#' 
#' @return A data.frame.
#' @export
setGeneric(
  name = "getScaledDf",
  def = getGeneric(f = "getScaledDf", package = "confuns")
)

#' @inherit confuns::getScaledMtr title return
#' @description Extracts scaled data as a matrix. In case of screening experiments 
#' it's the scaled data from slot @@features. In case of time lapse 
#' experiments its the scaled data from slot @@stat_features.
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getScaledMtr",
  def = getGeneric(f = "getScaledMtr", package = "confuns")
)

#' @export
setGeneric(
  name = "getSilWidthsDf",
  def = getGeneric(f = "getSilWidthsDf", package = "confuns")
)


# p -----------------------------------------------------------------------


#' @export
setGeneric(
  name = "plotAvgSilWidths",
  def = getGeneric(f = "plotAvgSilWidths", package = "confuns")
)

#' @inherit confuns::plotCorrplot title description params details return
#' @export
setGeneric(
  name = "plotCorrplot",
  def = getGeneric(f = "plotCorrplot", package = "confuns")
)

#' @export
setGeneric(
  name = "plotDendrogram",
  def = getGeneric(f = "plotDendrogram", package = "confuns")
)

#' @export
setGeneric(
  name = "plotPCA",
  def = getGeneric(f = "plotPCA", package = "confuns")
)

#' @export
setGeneric(
  name = "plotScatterplot",
  def = getGeneric(f = "plotScatterplot", package = "confuns")
)

#' @export
setGeneric(
  name = "plotScreeplot",
  def = getGeneric(f = "plotScreeplot", package = "confuns")
)

#' @export
setGeneric(
  name = "plotSilWidths",
  def = getGeneric(f = "plotSilWidths", package = "confuns")
)

#' @export
setGeneric(
  name = "plotTSNE",
  def = getGeneric(f = "plotTSNE", package = "confuns")
)

#' @export
setGeneric(
  name = "plotUMAP",
  def = getGeneric(f = "plotUMAP", package = "confuns")
)


# s -----------------------------------------------------------------------


#' @title Scale data
#' 
#' @description Scales numeric data of the \code{Cypro} object. In case 
#' of screening experiments the numeric data from slot @@features is taken. 
#' In case of time lapse experiments the data form slot @@features stats is taken.
#' 
#' @inherit argument_dummy params
#' 
#' @return The input object.
#'   
#' @export
setGeneric(
  name = "scaleData",
  def = getGeneric(f = "scaleData", package = "confuns")
)

#' @export
setGeneric(
  name = "setData",
  def = getGeneric(f = "setData", package = "confuns")
)

