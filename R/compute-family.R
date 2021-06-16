
#' @title Summarize tracking data
#'
#' @inherit check_track_df params
#'
#' @return A summarized data.frame. 

compute_cell_stats <- function(df, phase, verbose, object){
  
  msg <- 
    glue::glue(
      "Computing cell statistics and summary{ref_phase}.",
      ref_phase = hlpr_glue_phase(object, phase, FALSE)
      )
  
  confuns::give_feedback(msg = msg, verbose = verbose)
  
  df <- dplyr::group_by(df, cell_id)
  
  if(isUsable(object, module = "migration")){
    
    # compute migration efficiency 
    mgr_eff_df <- 
      dplyr::summarise(.data = df, 
        total_dist = base::sum(dflp, na.rm = TRUE),
        mgr_eff = compute_migration_efficiency(x_coords, y_coords, total_dist)
      )
    
  } else {
    
    # empty data.frame with only cell id variable 
    mgr_eff_df <- dplyr::select(df, cell_id) %>% dplyr::distinct()
    
  }
  
  stat_df <- 
    dplyr::select(df, -x_coords, -y_coords, -frame_num, -frame_time, -frame_itvl) %>% 
    dplyr::summarise(
      dplyr::across(
        .cols = where(fn = base::is.numeric),
        .fns = stat_funs,
        .names = "{.fn}_{.col}",
        na.rm = TRUE
        ) 
      ) %>% 
    dplyr::left_join(y = mgr_eff_df, by = "cell_id") %>% 
    dplyr::ungroup() %>% 
    # keep only numeric variables
    # grouping variables are extracted and 
    # stored previous to this function call
    dplyr::select(cell_id, where(fn = base::is.numeric)) 
  
  base::return(stat_df)
  
}

#' @rdname compute_cell_stats  
compute_stat <- function(track_df){ # deprecated
  
  dplyr::group_by(.data = track_df,
    cell_id, condition, cell_line, cl_condition,
    well, well_plate_name, well_plate_index, well_image
    ) %>% 
    dplyr::summarise(
      total_dist = base::sum(dflp, na.rm = TRUE),
      max_dist_fo = base::max(dfo, na.rm = TRUE),
      avg_dist_fo = base::mean(dfo, na.rm = TRUE),
      max_dist_flp = base::max(dflp, na.rm = TRUE),
      avg_dist_flp = base::mean(dflp, na.rm = TRUE),
      max_speed = base::max(speed, na.rm = TRUE),
      avg_speed = base::mean(speed, na.rm = TRUE),
      mgr_eff = compute_migration_efficiency(x_coords, y_coords, total_dist)
    ) %>% 
    dplyr::ungroup()
  
}


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

compute_distances_from_origin <- function(x_coords, y_coords){
  
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
  
  base::return(distances_from_origin)
  
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
  
  base::return(distances_from_last_point)
  
}


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
    dplyr::select(df, cell_id, frame_num, dplyr::all_of(variable_names)) %>% 
    dplyr::group_by(cell_id) %>% 
    dplyr::summarise_all(.funs = function(var){ base::is.na(var) %>% base::sum()}) %>% 
    dplyr::select(-frame_num)
  
  return(na_df)
  
}




# module variables --------------------------------------------------------

# to call within complete_stats()
compute_module_variables <- function(track_df, object, verbose){
  
  used_modules <- get_used_module_names(object)
  
  track_df <- dplyr::group_by(track_df, cell_id)
  
  for(i in base::seq_along(used_modules)){
    
    used_module <- used_modules[i]
    
    module_info <- 
      stringr::str_c("module", used_module, sep = "_") %>% 
      base::parse(text = .) %>% 
      base::eval()
    
    computable_vars <-
      get_computable_variable_names(object, module = used_module)
    
    vars_to_compute <- 
      get_variable_names_to_be_computed(object, module = used_module)
    
    if(base::length(vars_to_compute) >= 1){
      
      confuns::give_feedback(
        msg = glue::glue("Module: '{module_info$pretty_name}'"),
        verbose = verbose
      )
      
      for(var_to_compute in module_info$computation_order){
        
        if(var_to_compute %in% vars_to_compute){
          
          variable_info <- module_info$variables[[var_to_compute]]
          
          var_name_in_app <- variable_info$name_in_app
          
          confuns::give_feedback(
            msg = glue::glue("Variable: '{var_name_in_app}' ('{var_to_compute}')"), 
            verbose = verbose
          )
          
          fn <- variable_info$compute_with
          
          args <- list(track_df = track_df, object = object)
          
          args <- c(args, variable_info$compute_with_args)
          
          track_df <- rlang::invoke(.fn = fn, .args = args)
          
        }
        
      }
      
      replace_val <- module_info$replace_na_first_frame
      
      if(!base::is.null(replace_val)){
        
        for(cvar in computable_vars){
          
          track_df <- 
            dplyr::mutate(
              .data = track_df, 
              {{cvar}} := dplyr::case_when(
                frame_num == 1 & base::is.na(!!rlang::sym(cvar)) ~ {{replace_val}}, 
                TRUE ~ !!rlang::sym(cvar)
              )
            )
          
        }
        
      }

      
    }
    
  }
  
  return(track_df)
  
}







# 'computable'

compute_var_afo <- function(track_df, object){
  
  return(track_df)
  
}

compute_var_aflp <- function(track_df, object){
  
  return(track_df)
  
}


compute_var_dflp <- function(track_df, object){
  
  track_df <- 
    dplyr::group_by(track_df, cell_id) %>% 
    dplyr::mutate(dflp = compute_distances_from_last_point(x_coords, y_coords))
  
  return(track_df)
  
}

compute_var_dfo <- function(track_df, object){
  
  track_df <- 
    dplyr::group_by(track_df, cell_id) %>% 
    dplyr::mutate(dfo = compute_distances_from_origin(x_coords, y_coords))
  
  return(track_df)
  
}

compute_var_speed <- function(track_df, object){
  
  track_df <-
    dplyr::group_by(track_df, cell_id) %>% 
    dplyr::mutate(speed = dflp / object@set_up$itvl)
  
  return(track_df)
  
}


# 'summarizable'
compute_var_mgr_eff <- function(track_df, stat_df, ...){
  
  smrd_df <- 
    dplyr::left_join(
      x = track_df, 
      y = stat_df[,c("cell_id", "total_dist")],
      by = "cell_id"
    ) %>% 
    dplyr::group_by(cell_id) %>% 
    dplyr::summarize(
      mgr_eff = compute_migration_efficiency(x_coords, y_coords, total_dist)
    )
  
  return(smrd_df)
  
}

compute_var_total_dist <- function(track_df, ...){
  
  smrd_df <-
    dplyr::group_by(track_df, cell_id) %>% 
    dplyr::summarize(total_dist = base::sum(dflp))
  
  return(smrd_df)
  
}





