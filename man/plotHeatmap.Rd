% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-miscellaneous.R
\name{plotHeatmap}
\alias{plotHeatmap}
\title{Plot numeric variables in a heatmap}
\usage{
plotHeatmap(
  object,
  variable_names,
  across = "cell_id",
  across_subset = NULL,
  relevel = NULL,
  summarize_with = "mean",
  drop_na = TRUE,
  phase = NULL,
  verbose = TRUE,
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

\item{relevel}{Logical value. If set to TRUE the input order of \code{across_subset}
determines the order in which the groups of interest are displayed. Groups that
are not included are dropped which affects the choice of color.}

\item{summarize_with}{Character value. Denotes the function with which the
numeric variables are summarized across groups if \code{across} is not specified
as \emph{'cell_id'} but \emph{e.g. 'condition'}. One of \emph{'mean', 'median', 'max'} \emph{'min'}.}

\item{phase}{Character or numeric. If character, the ordinal value referring to the phase of interest (e.g. \emph{'first'}, \emph{'second'} etc.).
referring to the phase of interest or \emph{'all'}. If numeric, the number referring to the phase.

If set to NULL takes the phase denoted as default with \code{adjustDefault()}.

Ignored if the experiment design contains only one phase.}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}

\item{...}{Addtional arguments given to function \code{pheatmap::pheatmap()}.}
}
\value{
A heatmap.
}
\description{
This function uses a heatmpa to visualize numeric variables.
By specifying the \code{across}-argument in combination with the \code{summarize_with}-
argument the heatmap can visualize not only cellular profiles but summarized
profiles of any grouping variable such as \emph{cell_line} or \emph{condition}.
}
\details{
Input for argument \code{across} can be \emph{'cell_id'} to focus
on cells. In this case the summarizing is skipped.

Before visualization all values are rescaled to values from 0 to 1 within their
variable for proper color coding.
}
