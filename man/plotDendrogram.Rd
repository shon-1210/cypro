% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering-hierarchical.R
\name{plotDendrogram}
\alias{plotDendrogram}
\title{Visualize hierarchical clustering}
\usage{
plotDendrogram(
  object,
  variable_set,
  phase = NULL,
  method_dist = NULL,
  method_aggl = NULL,
  k = NULL,
  h = NULL,
  branch_size = 1,
  display_legend = TRUE,
  display_title = TRUE,
  clrp = "milo",
  clrp_adjust = NULL,
  ncol = NULL,
  nrow = NULL,
  verbose = NULL,
  ...
)
}
\arguments{
\item{object}{A valid cypro object.}

\item{variable_set}{Character value. Denotes the variable set of interest. Use \code{getVariableSetNames()}
to obtain all names of currently stored variable sets in your object.}

\item{phase}{Character or numeric. If character, the ordinal value referring to the phase of interest (e.g. \emph{'first'}, \emph{'second'} etc.).
referring to the phase of interest or \emph{'all'}. If numeric, the number referring to the phase.

If set to NULL takes the phase denoted as default with \code{adjustDefault()}.

Ignored if the experiment design contains only one phase.}

\item{method_dist}{Character vector (or value see details for more.) Denotes the distance method(s)
of interest (e.g. \emph{'euclidean'} or \emph{'manhattan'}).

Use \code{validDistanceMethods()} to obtain all valid input options.}

\item{method_aggl}{Character vector (or value see details for more.) Denotes the agglomeration
method(s) of interest according to which the existing distance matrices are agglomerated to hierarchical
trees.

Use \code{validAgglomerationMethods()} to obtain all valid input options.}

\item{k}{Numeric value or NULL. If numeric the branches are colored according to their cluster assignment
whereby the number of clusters is equal to \code{k}.}

\item{h}{Numeric value or NULL. If numeric the branches are colored according to their cluster assignment
whereby the number of clusters is equal to the results of cutting the hierarchical tree at height \code{h}..}

\item{branch_size}{Numeric value. Denotes the thickness of the branches.}

\item{display_legend}{Logical value. If set to TRUE a legend is displayed.}

\item{display_title}{Logical value. If set to TRUE an informative title is displayed.}

\item{clrp}{Character value. Specifies the color palette to be used to represent
groups of discrete variables. Run \code{validColorPalettes()} to obtain valid
input options.}

\item{clrp_adjust}{Named character vector or NULL. If character, it adjusts the
color palette that is used to represent the groups. Names of the input vector must refer
to the group and the respective named element denotes the color with which to
represent the group.}

\item{ncol}{Given to \code{ggplot2::facet_wrap()}. Affects the way the subplots
are displayed.}

\item{nrow}{Given to \code{ggplot2::facet_wrap()}. Affects the way the subplots
are displayed.}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}

\item{...}{Additional arguments given to \code{ggdendro::ggdendrogram()}.}
}
\value{


A ggplot.
}
\description{
Plots the hierarchical tress agglomerated by \code{agglomerateHierarchicalCluster()}.
}
\details{
Iterates over all valid combinations of the \code{method_*} arguments and creates a dendrogram
if the respective clustering results are found. \code{nrow} and \code{ncol} can be used to
align the plots.
}
