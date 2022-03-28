

# a -----------------------------------------------------------------------

#' @inherit confuns::addClusterVarsHclust title description details params return
#' @export
setGeneric(
  name = "addClusterVarsHclust",
  def = getGeneric(f = "addClusterVarsHclust", package = "confuns")
  )

#' @inherit confuns::addClusterVarsKmeans title description details params return
#' @export
setGeneric(
  name = "addClusterVarsKmeans", 
  def = getGeneric(f = "addClusterVarsKmeans", package = "confuns")
)

#' @inherit confuns::addClusterVarsPam title description details params return
#' @export
setGeneric(
  name = "addClusterVarsPam", 
  def = getGeneric(f = "addClusterVarsPam", package = "confuns")
)


#' @inherit confuns::agglomerateHierarchicalTrees title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "agglomerateHierarchicalTrees",
  def = getGeneric(f = "agglomerateHierarchicalTrees", package = "confuns")
  )


# c -----------------------------------------------------------------------

#' @inherit confuns::computeClusteringHclust title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "computeClusteringHclust",
  def = getGeneric(f = "computeClusteringHclust", package = "confuns")
)

#' @inherit confuns::computeClusteringKmeans title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "computeClusteringKmeans",
  def = getGeneric(f = "computeClusteringKmeans", package = "confuns")
)

#' @inherit confuns::computeClusteringPam title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "computeClusteringPam",
  def = getGeneric(f = "computeClusteringPam", package = "confuns")
)

#' @inherit confuns::computeDistanceMatrices title description details params return
#' @inherit argument_dummy params
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

#' @inherit confuns::computePCA title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "computePCA",
  def = getGeneric(f = "computePCA", package = "confuns")
)

#' @inherit confuns::computeTSNE title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "computeTSNE",
  def = getGeneric(f = "computeTSNE", package = "confuns")
)

#' @inherit confuns::computeUMAP title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "computeUMAP",
  def = getGeneric(f = "computeUMAP", package = "confuns")
)



# d -----------------------------------------------------------------------

#' @inherit confuns::detectOutliers title description params
#' @export
setGeneric(name = "detectOutliers", def = getGeneric(f = "detectOutliers", package = "confuns"))


# g -----------------------------------------------------------------------

#' @inherit confuns::getAvgSilWidthsDf title description params details return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getAvgSilWidthsDf",
  def = getGeneric(f = "getAvgSilWidthsDf", package = "confuns")
)

#' @inherit confuns::getClustering title description params details return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getClustering",
  def = getGeneric(f = "getClustering", package = "confuns")
)

#' @inherit confuns::getClusteringHclust title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getClusteringHclust",
  def = getGeneric(f = "getClusteringHclust", package = "confuns")
)

#' @inherit confuns::getClusteringKmeans title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getClusteringKmeans",
  def = getGeneric(f = "getClusteringKmeans", package = "confuns")
)

#' @inherit confuns::getClusterVarsHclust title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getClusterVarsHclust",
  def = getGeneric(f = "getClusterVarsHclust", package = "confuns")
)

#' @inherit confuns::getClusterVarsKmeans title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getClusterVarsKmeans",
  def = getGeneric(f = "getClusterVarsKmeans", package = "confuns")
)

#' @inherit confuns::getClusterVarsKmeans title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getClusterVarsKmeans",
  def = getGeneric(f = "getClusterVarsKmeans", package = "confuns")
)

#' @inherit confuns::getClusterVarsPam title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getClusterVarsPam",
  def = getGeneric(f = "getClusterVarsPam", package = "confuns")
)

#' @inherit confuns::getCorrelation title description params details return 
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getCorrelation",
  def = getGeneric(f = "getCorrelation", package = "confuns")
)

#' @inherit confuns::getCorrDf title description params details return 
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getCorrDf",
  def = getGeneric(f = "getCorrDf", package = "confuns")
)

#' @inherit confuns::getCorrMtr title description params details return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getCorrMtr",
  def = getGeneric(f = "getCorrMtr", package = "confuns")
)

#' @inherit confuns::getDendro title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getDendro",
  def = getGeneric(f = "getDendro", package = "confuns")
)

#' @inherit confuns::getDendroSegmentDf title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getDendroSegmentDf",
  def = getGeneric(f = "getDendroSegmentDf", package = "confuns")
)

#' @inherit confuns::getDf title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getDf",
  def = getGeneric(f = "getDf", package = "confuns")
)

#' @inherit confuns::getDimRed title description params return
#' @inherit argument_dummy params 
#' @export
setGeneric(
  name = "getDimRed",
  def = getGeneric(f = "getDimRed", package = "confuns")
)

#' @inherit confuns::getDistMtr title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getDistMtr",
  def = getGeneric(f = "getDistMtr", package = "confuns")
)

#' @inherit confuns::getEmbeddingDf title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getEmbeddingDf",
  def = getGeneric(f = "getEmbeddingDf", package = "confuns")
)

#' @inherit confuns::getHclust title description params details return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getHclust",
  def = getGeneric(f = "getHclust", package = "confuns")
)

#' @inherit confuns::getKeyDf title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getKeyDf",
  def = getGeneric(f = "getKeyDf", package = "confuns")
)

#' @inherit confuns::getKmeans title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getKmeans",
  def = getGeneric(f = "getKmeans", package = "confuns")
)

#' @inherit confuns::getMtr title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getMtr",
  def = getGeneric(f = "getMtr", package = "confuns")
)


#' @title Obtain outlier cell IDs
#' 
#' @description Extracts the cell IDs that were identified as outliers. See 
#' details for more.
#' 
#' @inherit argument_dummy params
#' @inherit detectOutliers params
#' 
#' @return Character vector or named list of character vectors.
#' 
#' @details The class of the return value depends on the input for argument 
#' \code{across}. If \code{across} = NULL, a character vector of cell IDs
#' is returned. If \code{across} is a character, a list of character vectors 
#' is returned. Each character vector in the list represents the results for 
#' one of the group the grouping variable denoted in \code{across} contains. 
#' 
#' Depending on the input for argument \code{method} additional arguments 
#' can be specified to refine the output. 
#' 
#' \itemize{
#'  \item{\code{method} = \emph{'IQR'}: }{Input for argument \code{features} determines
#'  the numeric features for which outlier detection has been conducted. If NULL, all 
#'  are considered. If character, only cell IDs that were identified as outliers within 
#'  the specified features/variables are included in the return value.}
#'  }
#'  
#' @seealso \code{getOutlierResults()}
#' 
#' @export
setGeneric(name = "getOutlierIDs", def = getGeneric(f = "getOutlierIDs", package = "confuns"))

#' @inherit confuns::getOutlierResults title description details return 
#' @inherit argument_dummy params
#' @export
setGeneric(name = "getOutlierResults", def = getGeneric(f = "getOutlierResults", package = "confuns"))

#' @inherit confuns::getPam title description details params return
#' @inherit argument_dummy params
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

#' @inherit confuns::getResults title description details params return
#' @inherit argument_dummy params
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

#' @inherit confuns::getSilWidthsDf title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "getSilWidthsDf",
  def = getGeneric(f = "getSilWidthsDf", package = "confuns")
)



# i -----------------------------------------------------------------------



# p -----------------------------------------------------------------------

#' @inherit confuns::plotAvgSilWidths title description details params return
#' @inherit argument_dummy params
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

#' @inherit confuns::plotDendrogram title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "plotDendrogram",
  def = getGeneric(f = "plotDendrogram", package = "confuns")
)

#' @inherit confuns::plotPCA title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "plotPCA",
  def = getGeneric(f = "plotPCA", package = "confuns")
)

#' @inherit confuns::plotScatterplot title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "plotScatterplot",
  def = getGeneric(f = "plotScatterplot", package = "confuns")
)

#' @inherit confuns::plotScreeplot title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "plotScreeplot",
  def = getGeneric(f = "plotScreeplot", package = "confuns")
)

#' @inherit confuns::plotSilWidths title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "plotSilWidths",
  def = getGeneric(f = "plotSilWidths", package = "confuns")
)

#' @inherit confuns::plotTSNE title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "plotTSNE",
  def = getGeneric(f = "plotTSNE", package = "confuns")
)

#' @inherit confuns::plotUMAP title description details params return
#' @inherit argument_dummy params
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

#' @inherit confuns::setData title description details params return
#' @inherit argument_dummy params
#' @export
setGeneric(
  name = "setData",
  def = getGeneric(f = "setData", package = "confuns")
)

