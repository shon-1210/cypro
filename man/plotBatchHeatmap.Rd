% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batch-effect-detection.R
\name{plotBatchHeatmap}
\alias{plotBatchHeatmap}
\title{Visualize possible batch effects}
\usage{
plotBatchHeatmap(object, clrp = "milo", verbose = NULL, correlated = FALSE)
}
\arguments{
\item{object}{A valid cypro object.}

\item{clrp}{Character value. Specifies the color palette to be used to represent
groups of discrete variables. Run \code{validColorPalettes()} to obtain valid
input options.}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}

\item{correlated}{Logical value. If set to TRUE the correlation results
of the distance matrix are used as input for the heatmap.}
}
\value{
A heatmap as output from \code{pheatmap::pheatmap()}.
}
\description{
Visualizes the distances of the median well profiles cmoputed by
\code{detectBatchEffects()} in a not clustered heatmap.
}
