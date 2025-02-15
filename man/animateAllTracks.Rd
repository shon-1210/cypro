% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/animate-family.R
\name{animateAllTracks}
\alias{animateAllTracks}
\title{Animate all tracks}
\usage{
animateAllTracks(
  object,
  across = "cell_line",
  across_subset = NULL,
  frame_subset = NULL,
  phase = "all",
  n_cells = 100,
  color_by = across,
  linetype = "solid",
  linesize = 0.75,
  add_on_list = list(),
  verbose = NULL,
  ...
)
}
\arguments{
\item{object}{A valid cypro object.}

\item{across}{Character value, character vector or NULL (see details for more).
Specifies the grouping variable of interest.

Use \code{getGroupingVariableNames()} to obtain all variable names that group the
cells of your experiment in a certain manner.}

\item{across_subset}{Character vector, list of character vectors or NULL (see details for
more). Specifies the particular groups of interest the grouping variable specified in argument
\code{across} contains.

If set to NULL all of them are chosen. You can prefix groups you are NOT interested in
with a \emph{'-'}. (Saves writing if there are more groups you are interested in
than groups you are not interested in.)

Use \code{getGroupNames()} to obtain all valid input options.}

\item{phase}{Character or numeric. If character, the ordinal value referring to the phase of interest (e.g. \emph{'first'}, \emph{'second'} etc.).
referring to the phase of interest or \emph{'all'}. If numeric, the number referring to the phase.

If set to NULL takes the phase denoted as default with \code{adjustDefault()}.

Ignored if the experiment design contains only one phase.}

\item{n_cells}{Numeric calue. Determines the number of cells that are randomly chosen from
every group to be displayed. Useful to keep plots insightful and aesthetically pleasing.}

\item{linetype}{Character value. Valid options are \emph{'solid', 'twodash', 'longdash', 'dotted'}
and \emph{'dotdash'}.}

\item{linesize}{Numeric value. Denotes the size of the lines drawn.}

\item{add_on_list}{A list of ggplot2-add-ons that are supposed to be integrated in
the visualization process.}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}

\item{...}{Additional arguments given to \code{gganimte::animate()}.}
}
\value{
An animated GIF.
}
\description{
Animates the output of function \code{plotAllTracks()}.
}
