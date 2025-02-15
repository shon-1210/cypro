% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny-functions.R
\name{evaluate_file_availability_shiny}
\alias{evaluate_file_availability_shiny}
\alias{evaluate_file_content_shiny}
\title{Evaluate the availability of files}
\usage{
evaluate_file_availability_shiny(
  wp_list,
  recursive = TRUE,
  keep_filetype = "csv$"
)

evaluate_file_content_shiny(
  var_name_well_plate = "none",
  var_name_well = "Metadata_Well",
  var_name_roi = "Metadata_ROI",
  wp_list,
  df,
  dir,
  ...
)
}
\arguments{
\item{wp_list}{A list that contains a well-plate data.frame and a directory leading to the folder in which
the respective files are stored.}

\item{recursive}{Logical value. Given to argument \code{recursive} of \code{base::list.files()}.}

\item{keep_filetype}{Character value. Determines the filetype to be ignored if for one well-image
a .csv and a .xls file is found. Either \emph{'xls$'} or \emph{'csv$'}.}
}
\value{
The same list with additional slots containing a the well-plate data.frame joined
with the evaluation variables as well as a vector containing the well-image files that are missing,
a vector with file directories that are ambiguous and a vector with the valid directories from which
to load the data.
}
\description{
This functions takes a list as input that contains a well-plate data.frame (slot
must be named \emph{wp_df}) and a directory. The directory leads to the folder in which to look for
.csv and .xls files named according to the well-image they belong. The availability of files is compared
to what is expected based on the well-plate data.frame (wells, number of images per well etc.)
}
