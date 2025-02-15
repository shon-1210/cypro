% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/subset-object.R
\name{subsetByCellId}
\alias{subsetByCellId}
\title{Create data subset by cell ids}
\usage{
subsetByCellId(
  object,
  new_name,
  cell_ids,
  reasoning = NULL,
  suffix = FALSE,
  suffix_sep = "_",
  verbose = NULL,
  ...
)
}
\arguments{
\item{object}{A valid cypro object.}

\item{new_name}{Character value. Denotes the name of the output object. If set to NULL the name of the
input object is taken and suffixed with \emph{'_subset'}.}

\item{cell_ids}{Character vector. Denotes the cells to keep unambiguously with their cell ids.}

\item{reasoning}{Character value. Allows two write a short description of how the cell ids according
to which the object is filtered were selected. This description is included in the output of \code{printSubsetHistory()}.
Ignored if set to NULL.}

\item{suffix}{Logical value. If set to TRUE the character value provided with arugment \code{new_name} is
used to suffix the original name as well as the current default directory.}

\item{suffix_sep}{Character value. Denotes the string to insert between the object name / storage directory
and the input value for \code{new_name} if \code{suffix} is set to TRUE.}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}
}
\value{
A cypro object that contains the data for the subsetted cells.
}
\description{
Subset functions allow to conveniently split your data by certain characteristics such
as cell lines, conditions, cluster etc. or for specific cell ids. This might be useful if you want apply some machine learning
algorithms such as clustering and correlation on only a subset of cells. See details for more information.
}
\details{
Creating subsets of your data affects analysis results such as clustering and correlation which
is why these results are reset in the subsetted object and must be computed again. To prevent inadvertent overwriting
the default directory is reset as well. Make sure to set a new one via \code{setDefaultDirectory()}.

The mechanism with which you create the subset is stored in the output object. Use \code{printSubsetHistory()}
to reconstruct the way from the original object to the current one.
}
