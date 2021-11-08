

# d -----------------------------------------------------------------------

#' @title Compute the distance between to points
#'
#' @param starting_pos,final_pos Numeric vector of length two. Denotes the two positions 
#' between which the distance is calculated 
#'
#' @return A numeric value. 
#'

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



# m -----------------------------------------------------------------------

#' @title Calculate migration efficiency
#'
#' @param x_coords Numeric vector. Refers to x-coordinates of object. 
#' @param y_coords Numeric vector. Refers to y-coordinates of object.
#' @param total_dist Numeric vector/value. Refers to the total distance 
#' the object of interested has traveled. 
#'
#' @return A numeric value assessing how efficient the object of interested has
#' migrated. 

compute_migration_efficiency <- function(x_coords, y_coords, total_dist){
  
  # compute direction vector between first and final position
  starting_pos <- c(x_coords[1], y_coords[1])
  final_pos <- c(x_coords[length(x_coords)], y_coords[length(y_coords)])
  drvc <- final_pos - starting_pos
  
  # compute effective distance travelled ( = value of direction vector)
  effective_distance <- base::sqrt(drvc[1]^2 + drvc[2]^2)
  
  # make sure that length == 1 if calculated inside dplyr::mutate()
  total_dist <- base::unique(total_dist)
  
  # divide effective distance by actual distance
  migration_efficiency <- total_dist/(total_dist/effective_distance)
  
  return(migration_efficiency)
  
}


compute_module_variables_hlpr <- function(object, df, verbose = TRUE){
  
  used_modules <- 
    object@modules[getActiveModuleNames(object)] %>% 
    purrr::keep(.p = containsComputableVariables)
  
  for(m in base::seq_along(used_modules)){
    
    module <- used_modules[[m]]
    
    if(containsComputableVariables(module)){
      
      vars_to_compute <-
        purrr::keep(
          .x = module@variables_computable,
          .p = ~ base::is.na(.x@name_in_example)
        )
      
      n_vtc <- base::length(vars_to_compute)
      ref <- adapt_reference(vars_to_compute, "variable")
      
      give_feedback(
        msg = glue::glue("Computing {n_vtc} {ref} for module {module@name_in_app}."),
        verbose = verbose
      )
      
      for(v in base::seq_along(vars_to_compute)){
        
        vtc <- vars_to_compute[[v]]
        
        give_feedback(
          msg = glue::glue("Computing variable '{vtc@name_in_app}' ({vtc@name_in_cypro})."), 
          verbose = verbose
        )
        
        # all variable compute functions take the feature df 
        # as the first and the cypro object as the last argument
        df <- vtc@compute_with(df, object)
        
      }
      
    }
    
  }
  
  return(df)
  
}


# n -----------------------------------------------------------------------

#' @title Compute n missing values
#'
#' @description Counts the number of missing values a cell id has for all 
#' track variables. 
compute_n_missing_values <- function(object, phase = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase)
  
  msg <- 
    glue::glue(
      "Counting NAs by cell id in track data{ref_phase}.",
      ref_phase = hlpr_glue_phase(object, phase, FALSE, "of")
      )
  
  confuns::give_feedback(msg = msg, verbose = verbose)
  
  variable_names <- getTrackVariableNames(object)
  
  if(multiplePhases(object)){
    
    df <- purrr::map_df(object@cdata$tracks[phase], .f = ~ .x)
    
  } else {
    
    df <- object@cdata$tracks
    
  }
  
  # summarise how many missing values every cell id has across all variables
  na_df <- 
    dplyr::select(df, dplyr::any_of(x = c("cell_id", "frame_num")), dplyr::all_of(variable_names)) %>% 
    dplyr::group_by(cell_id) %>% 
    dplyr::summarise_all(.funs = function(var){ base::is.na(var) %>% base::sum()})
  
  if(isTimeLapseExp(object)){
    
    na_df <- dplyr::select(na_df, -frame_num)
    
  }
  
  return(na_df)
  
}



# v -----------------------------------------------------------------------

# functions to compute variables within compute_module_variables()
# must take tracks_df, object and ... as arguments. 
# must return the tracks_df with the new variable as output

compute_var_afo <- function(tracks_df, object, ...){
  
  tracks_df$afo <- 0
  
  return(tracks_df)
  
}

compute_var_aflp <- function(tracks_df, object, ...){
  
  tracks_df$aflp <- 0
  
  return(tracks_df)
  
}

compute_var_dflp <- function(tracks_df, object, ...){
  
  tracks_df <- 
    dplyr::group_by(tracks_df, cell_id) %>% 
    dplyr::mutate(dflp = compute_distances_from_last_point(x_coords, y_coords))
  
  return(tracks_df)
  
}

compute_var_dfo <- function(tracks_df, object, phase, ...){
  
  tracks_df <- 
    dplyr::group_by(tracks_df, cell_id) %>% 
    dplyr::mutate(
      dfo = compute_distances_from_origin(
        x_coords = x_coords,
        y_coords = y_coords, 
        object = object
      )
    )
  
  return(tracks_df)
  
}

compute_var_speed <- function(tracks_df, object, ...){
  
  exp_design <- getExperimentDesign(object)
  
  itvl <- exp_design@interval
  
  tracks_df <-
    dplyr::group_by(tracks_df, cell_id) %>% 
    dplyr::mutate(speed = dflp / {{itvl}})
  
  return(tracks_df)
  
}







# A -----------------------------------------------------------------------


# M -----------------------------------------------------------------------

#' @title Compute module variables 
#' 
#' @description Computes the computable variables of all active analysis modules
#' in the \code{Cypro} object and adds them to \code{Cypro} objects data.
#'
#' @inherit argument_dummy params
#'
#' @return The input object. 
#' @export
#'
setGeneric(name = "computeModuleVariables", def = function(object, ...){
  
  standardGeneric(f = "computeModuleVariables")
  
})

#' @rdname computeModuleVariables
#' @export
setMethod(
  f = "computeModuleVariables",
  signature = "CyproScreening",
  definition = function(object, verbose = TRUE){
    
    give_feedback(
      msg = "Checking for analysis module related variables that need to be computed.", 
      verbose = verbose
    )
  
    feature_df <- getFeatureDf(object)
    
    feature_df <- compute_module_variables_hlpr(object = object, df = feature_df, verbose = verbose)
    
    object <- setFeatureDf(object = object, df = feature_df)
    
    return(object)
  
})

#' @rdname computeModuleVariables
#' @export
setMethod(
  f = "computeModuleVariables",
  signature = "CyproTimeLapse",
  definition = function(object, verbose = TRUE){
    
    give_feedback(
      msg = "Checking for analysis module related variables that need to be computed.", 
      verbose = verbose
    )
    
    tracks_df <- getTracksDf(object)
    
    tracks_df <- compute_module_variables_hlpr(object = object, df = tracks_df, verbose = verbose)
    
    object <- setTracksDf(object = object, df = tracks_df)
    
    return(object)
  
})

#' @rdname computeModuleVariables
#' @export
setMethod(
  f = "computeModuleVariables",
  signature = "CyproTimeLapseMP",
  definition = function(object, verbose = TRUE){
    
    give_feedback(
      msg = "Checking for analysis module related variables that need to be computed.", 
      verbose = verbose
    )
    
    phases <- getPhases(object)
    
    for(p in phases){
      
      give_feedback(
        msg = glue::glue("Working on phase {p}."), 
        verbose = verbose
      )
      
      tracks_df <- getTracksDf(object, phase = p)
      
      tracks_df <- compute_module_variables_hlpr(object = object, df = tracks_df, verbose = verbose)
      
      if(p != 1){
        
        first_frame <- base::min(tracks_df$frame_num)
        
        # remove first frame as it actually belongs to
        # previous frame
        tracks_df <- dplyr::filter(tracks_df, frame_num != {{first_frame}})
        
      }
      
      object <- setTracksDf(object = object, df = tracks_df, phase = p)
      
    }
    
    return(object)
    
  })










