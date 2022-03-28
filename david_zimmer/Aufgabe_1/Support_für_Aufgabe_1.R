
# Die Funktionen, die ich geschrieben habe, um die variablen distance from origin
# und distance from last point zu berechnen

compute_distance <- function(starting_pos, final_pos){

  # direction vector
  drvc <- final_pos - starting_pos

  # compute effective distance traveled ( = value of direction vector)
  base::sqrt(drvc[1]^2 + drvc[2]^2)

}

#' @title Compute the distances from origin
#'
#' @description The first values of both input-vectors are taken as
#' the origin position. For each subsequent position the respective
#' distance to the position of origin is computed.
#'
#' To be used in \code{dplyr::mutate()} to recalculate the distances
#' of origin if the experiment includes time displaced treatment.
#'
#' @param x_coords Numeric vector. Refers to x-coordinates of object.
#' @param y_coords Numeric vector. Refers to y-coordinates of object.
#'
#' @return A numeric vector that corresponds to the respective distances from
#' the first position.
#'

compute_distances_from_origin <- function(x_coords, y_coords, object){

  n_coords <- base::length(x_coords)

  origin <- c(x_coords[1], y_coords[1])

  subsequent_positions <-
    purrr::map2(
      .x = x_coords[2:n_coords],
      .y = y_coords[2:n_coords],
      .f = ~ c(.x, .y)
    )

  distances_from_origin <-
    purrr::map_dbl(
      .x = subsequent_positions,
      .f = ~ compute_distance(starting_pos = origin, final_pos = .x)
    ) %>%
    base::round(digits = 2) %>%
    purrr::prepend(values = 0)

  return(distances_from_origin)

}

#' @rdname compute_distances_from_origin
compute_distances_from_last_point <- function(x_coords, y_coords){

  n_coords <- base::length(x_coords)

  origins <-
    purrr::map2(
      .x = x_coords[1:(n_coords-1)],
      .y = y_coords[1:(n_coords-1)],
      .f = ~ c(.x, .y)
    )

  subsequent_positions <-
    purrr::map2(
      .x = x_coords[2:n_coords],
      .y = y_coords[2:n_coords],
      .f = ~ c(.x, .y)
    )

  distances_from_last_point <-
    purrr::map2(
      .x = origins,
      .y = subsequent_positions,
      .f = ~ compute_distance(starting_pos = .x, final_pos = .y)
    ) %>%
    purrr::flatten_dbl() %>%
    purrr::prepend(values = 0)

  return(distances_from_last_point)

}
