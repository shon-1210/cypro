

# confuns helpers ---------------------------------------------------------

#' @title Valid colorpanels & -spectra
#' @export
allColorpalettes <- confuns::all_color_palettes

#' @rdname allColorpalettes
#' @export
allColorspectra <- confuns::all_color_spectra

# not exported 
scollapse <- confuns::scollapse


# tidyselect helpers ------------------------------------------------------


#' @title Imported selection helpers from package 'tidyselect'
#' 
#' @inherit tidyselect::starts_with description params details return examples
#' @export
starts_with <- tidyselect::starts_with

#' @rdname starts_with
#' @export
ends_with <- tidyselect::ends_with

#' @rdname starts_with
#' @export
contains <- tidyselect::contains

#' @rdname starts_with
#' @export
matches <- tidyselect::matches


#' @importFrom magrittr %>%
#'
NULL
