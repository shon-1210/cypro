% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-module-migration.R
\name{plotVelocityLineplot}
\alias{plotVelocityLineplot}
\title{Plot cell activity over time}
\usage{
plotVelocityLineplot(
  object,
  across = "condition",
  across_subset = NULL,
  phase = NULL,
  threshold = NULL,
  linesize = 1,
  smooth = TRUE,
  smooth_span = 0.25,
  smooth_se = FALSE,
  clrp = "milo",
  verbose = TRUE,
  ...,
  in_shiny = FALSE
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

\item{threshold}{Numeric value or NULL. If set to NULL (the default) the
threshold to consider a cell 'active' is equal to \code{base::mean(speed) + base::sd(speed)}}

\item{linesize}{Numeric value. Denotes the size of the lines drawn.}

\item{smooth}{Logical. If set to TRUE the values are smoothed.}

\item{smooth_span}{NUmeric value. Denotes the smoothing span used.}

\item{smooth_se}{Logical. If set to TRUE the standard error will be displayed.}

\item{clrp}{Character value. Specifies the color palette to be used to represent
groups of discrete variables. Run \code{validColorPalettes()} to obtain valid
input options.}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}
}
\description{
Visualizes the percentage of active cells over time.
}
