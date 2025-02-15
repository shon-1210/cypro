% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print-summary.R
\name{printSummary}
\alias{printSummary}
\alias{printAnalysisSummary}
\title{Print object summary}
\usage{
printSummary(object)

printAnalysisSummary(object, slots = c("dim_red", "clustering"))
}
\arguments{
\item{object}{A valid cypro object.}
}
\value{
A printed message via \code{writeLines()}.
}
\description{
Prints an overview of the objects content into the console.
}
