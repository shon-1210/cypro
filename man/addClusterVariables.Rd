% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add-family.R
\name{addClusterVariables}
\alias{addClusterVariables}
\alias{addMetaVariables}
\title{Add discrete/categorical variables that group the cells}
\usage{
addClusterVariables(
  object,
  input_df,
  variable_names,
  phase = NULL,
  overwrite = FALSE,
  by = "cell_id",
  verbose = TRUE
)

addMetaVariables(
  object,
  input_df,
  variable_names,
  phase = NULL,
  overwrite = FALSE,
  with_well_plate = FALSE,
  by = "cell_id",
  verbose = TRUE
)
}
\arguments{
\item{object}{A valid cypro object.}

\item{input_df}{A data.frame that contains the variables denoted in argument \code{variable_names}
as well as a character variable named according to input of argument \code{by} that is used to match
and join both data.frames.}

\item{variable_names}{Character vector. The name of the variables that are to be joined.}

\item{phase}{Character or numeric. If character, the ordinal value referring to the phase of interest (e.g. \emph{'first'}, \emph{'second'} etc.).
referring to the phase of interest or \emph{'all'}. If numeric, the number referring to the phase.

If set to NULL takes the phase denoted as default with \code{adjustDefault()}.

Ignored if the experiment design contains only one phase.}

\item{overwrite}{Logical value. Must be set to TRUE in case of overlapping
variable names.

(Note that overwriting stat variables leads to all analysis progress of affected
variable sets being discarded.)}

\item{by}{Character value. Denotes the variable by which the new informational
variables specified in argument \code{variable_names} are supposed to be joined.
In case of \code{addClusterVariables()} and \code{addMetaVariables()} this does not
have to be \emph{'cell_id'}.

E.g. if you want to add additional grouping options that refer to the conditions such
as \emph{'mechanism_of_action'} again grouping the conditions you can specify
\code{by} = \emph{'condition'}.}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}
}
\value{
An updated cypro object that contains the data added.
}
\description{
Allows to join new discrete/categorical variables that can be referred
to via the \code{across}-argument of many functions.
}
