% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-family.R
\name{getOutlierResults}
\alias{getOutlierResults}
\alias{getOutlierIds}
\title{Obtain outlier detection results}
\usage{
getOutlierResults(
  object,
  method_outlier = NULL,
  check = TRUE,
  phase = NULL,
  verbose = NULL
)

getOutlierIds(
  object,
  method_outlier = NULL,
  check = FALSE,
  flatten = TRUE,
  phase = NULL
)
}
\arguments{
\item{object}{A valid cypro object.}

\item{method_outlier}{Character vector. Specifies the method/algorithm of interest.

Use \code{validOutlierDetectionMethods()} to obtain all valid input options.}

\item{phase}{Character or numeric. If character, the ordinal value referring to the phase of interest (e.g. \emph{'first'}, \emph{'second'} etc.).
referring to the phase of interest or \emph{'all'}. If numeric, the number referring to the phase.

If set to NULL takes the phase denoted as default with \code{adjustDefault()}.

Ignored if the experiment design contains only one phase.}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}
}
\value{
\code{getOutlierResults()} returns a list in which each slot contains
the results for a specific method. \code{getOutlierIds()} returns a character
vector of cell ids containing all cell ids that have been detected as outliers
by at least one method.
}
\description{
These functions can be used to extract the results of the outlier
detection algorithms.
}
