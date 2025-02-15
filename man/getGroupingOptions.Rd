% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{getGroupingOptions}
\alias{getGroupingOptions}
\alias{getAcrossOptions}
\title{Obtain names of variables that group the cells}
\usage{
getGroupingOptions(object, phase = NULL)

getAcrossOptions(object, phase = NULL)
}
\arguments{
\item{object}{A valid cypro object.}

\item{phase}{Character or numeric. If character, the ordinal value referring to the phase of interest (e.g. \emph{'first'}, \emph{'second'} etc.).
referring to the phase of interest or \emph{'all'}. If numeric, the number referring to the phase.

If set to NULL takes the phase denoted as default with \code{adjustDefault()}.

Ignored if the experiment design contains only one phase.}
}
\value{
An informative list.
}
\description{
This function returns the names of the variables that
group cell ids and can therefore be used as input for the \code{across}
argument.
}
