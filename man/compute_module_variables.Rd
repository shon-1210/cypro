% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute-family.R
\name{compute_module_variables}
\alias{compute_module_variables}
\alias{summarize_module_variables}
\title{Compute module based variables}
\usage{
compute_module_variables(track_df, object, verbose, phase)

summarize_module_variables(stat_df, track_df, object, verbose)
}
\arguments{
\item{track_df}{The already completed track data.frame complete_tracks() is
currently working with.}

\item{object}{A valid cypro object.}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}

\item{phase}{Character or numeric. If character, the ordinal value referring to the phase of interest (e.g. \emph{'first'}, \emph{'second'} etc.).
referring to the phase of interest or \emph{'all'}. If numeric, the number referring to the phase.

If set to NULL takes the phase denoted as default with \code{adjustDefault()}.

Ignored if the experiment design contains only one phase.}

\item{stat_df}{The already summarized stats data.frame complete_stats() is
currently working with.}
}
\value{

}
\description{
To be used within complete_tracks() and complete_stats() again
within processData(). Checks which modules are used and which variables
needs to be computed for the tracks data and summarized for the stats
data. They iteratively join the computed variables to the respective
data.frame. As the for loops rely on seq_along() these functions
return the input data.frame if nothing needs to be computed/summarized.
}
