% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier-detection.R
\name{detect_outliers_iqr}
\alias{detect_outliers_iqr}
\alias{detect_outliers_mahalanobis}
\title{Outlier detection functions}
\usage{
detect_outliers_iqr(
  object,
  variable_names = NULL,
  across = NULL,
  phase = NULL,
  verbose = NULL
)

detect_outliers_mahalanobis(
  object,
  variable_names = NULL,
  threshold_pval = 0.001,
  verbose = NULL,
  across = NULL,
  phase = NULL
)
}
\description{
Outlier detection functions
}
