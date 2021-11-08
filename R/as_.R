

# documentation dummy

#' Dummy
#'
#' @param df A data.frame of a class for which a method has been defined.
#'
#' @return The input data.frame with adjusted attributes and - if necessary - 
#' adjusted variables.
#' 
#' @seealso \code{as_cypro_df()}, \code{as_cluster_df()}, \code{as_stats_df()}, \code{as_tracks_df()}
#' 
as_df_dummy <- function(df){}


# c -----------------------------------------------------------------------




#' @title Coerce to \code{cluster_df} data.frame
#' 
#' @description Coerces input data.frame to \code{cluster_df} data.frame.
#'
#' @inherit as_df_dummy params return seealso
#'
#' @details Numeric variables of type \emph{double} are silently dropped.
#' Apart from variable \emph{cell_id} all non-\emph{double} variables
#' are coerced to factors. 
#'

#' 
#' @export
as_cluster_df <- function(df){
  
  UseMethod(generic = "as_cluster_df", object = df)
  
}

#' @rdname as_cluster_df
#' @export 
as_cluster_df.cypro_df <- function(df){
  
  out <- 
    dplyr::select_if(df, .predicate = ~ !base::is.double(.x)) %>% 
    dplyr::mutate(dplyr::across(.cols = -dplyr::all_of("cell_id"), .fns = function(var){
      
      if(!base::is.factor(var)){
        
        var <- base::as.factor(var)
        
      }
      
      return(var)
      
    })) %>% 
    add_classes(classes = "cluster_df")
  
  return(out)
  
}


#' @title Coerce to \code{cypro_df} data.frame
#' 
#' @description Coerces input data.frame to \code{cypro_df} data.frame.
#'
#' @inherit as_df_dummy params return seealso
#' 
#' @details Variable \emph{cell_id} is coerced to class \code{character}.
#'
#' @export
#'
as_cypro_df <- function(df){
  
  UseMethod(generic = "as_cypro_df", object = df)
  
}

#' @rdname as_cypro_df
#' @export
as_cypro_df.data.frame <- function(df){
  
  check_data_frame(
    df = df, 
    var.class = list(cell_id = "any")
  )
  
  out <- 
    dplyr::mutate(df, cell_id = base::as.character(cell_id)) %>% 
    add_classes(classes = "cypro_df")
  
  return(out)
  
}



# l -----------------------------------------------------------------------


#' @title Coerce to \code{layout_df} data.frame
#' 
#' @description Coerces input data.frame to layout data.frames.
#'
#' @inherit argument_dummy params
#' @param df A data.frame.
#'
#' @return The input data.frame with adjusted attributes.
#' @export
#'

as_layout_df <- function(df, well_plate_type = NULL){
  
  UseMethod(generic = "as_layout_df", object = df)
  
}

#' @rdname as_layout_df
#' @export
as_layout_df_mp <- function(df, well_plate_type = NULL, n_phases = NULL){
  
  UseMethod(generic = "as_layout_df_mp")
  
}

#' @rdname as_layout_df
#' @export
as_layout_df.data.frame <- function(df, well_plate_type = NULL){
  
  is_value(well_plate_type, mode = "character")
  
  if(base::is.null(well_plate_type)){
    
    well_plate_type <- base::attr(df, which = "well_plate_type")
    
    if(base::is.null(well_plate_type)){
      
      stop("Please specify argument 'well_plate_type'.")
      
    }
    
  }
  
  check_one_of(
    input = well_plate_type, 
    against = well_plate_info$type
  )
  
  classes_df <- 
    base::class(df) %>% 
    vselect(-any_of("layout_df"))
  
  base::class(df) <- c("layout_df", classes_df)
  
  base::attr(x = df, which = "well_plate_type") <- well_plate_type
  
  df <- addRoiInfoVarNames(df, vars = c("well_roi", "roi"))
  
  return(df)
  
}

#' @rdname as_layout_df
#' @export
as_layout_df_mp.layout_df <- function(df, well_plate_type = NULL, n_phases = NULL){
  
  is_value(well_plate_type, mode = "character", skip.allow = TRUE, skip.val = NULL)
  is_value(n_phases, mode = "numeric")
  
  classes_df <- 
    base::class(df) %>% 
    vselect(-any_of("layout_df_mp"))
  
  base::class(df) <- c("layout_df_mp", classes_df)
  
  if(!base::is.character(well_plate_type)){
    
    wpt_set <- base::attr(x = df, which = "well_plate_type")
    
    if(!base::is.character(wpt_set)){
      
      stop("Please specify argument 'well_plate_type'.")
      
    }
    
  } else {
    
    check_one_of(
      input = well_plate_type, 
      against = well_plate_info$type
    )
    
    base::attr(x = df, which = "well_plate_type") <- well_plate_type
    
  }
  
  if(!base::is.numeric(n_phases)){
    
    n_phases <- base::attr(x = df, which = "n_phases")
    
    if(base::is.null(n_phases)){
      
      stop("Please specify argument 'n_phases'.")
      
    } else {
      
      # already set 
      
    }
    
  } else {
    
    base::attr(df, which = "n_phases") <- n_phases
    
  }
  
  return(df)
  
}



# s -----------------------------------------------------------------------


#' @title Coerce to \code{stats_df}.
#' 
#' @description Coerces input data.frame to \code{stats_df} data.frame.
#'
#' @inherit as_df_dummy params return seealso
#'
#' @export
#'
as_stats_df <- function(df){
  
  UseMethod(generic = "as_stats_df", object = df)
  
}

#' @rdname as_stats_df
#' @export
as_stats_df.cypro_df <- function(df, completed = FALSE){
  
  add_classes(x = df, classes = c("stats_df", "feature_df")) %>% 
    magrittr::set_attr(which = "completed", value = completed)
  
}


# t -----------------------------------------------------------------------



#' @title Coerce to \code{tracks_df}.
#' 
#' @description Coerces input data.frame to \code{tracks_df} data.frame.
#'
#' @inherit as_df_dummy params return seealso
#'
#' @export
#'
as_tracks_df <- function(df){
  
  UseMethod(generic = "as_tracks_df", object = df)
  
}

#' @rdname as_tracks_df
#' @export
as_tracks_df.cypro_df <- function(df, completed = FALSE){
  
  out <- 
    dplyr::mutate(
      .data = df,
      frame_added = base::as.logical(frame_added),
      frame_num = base::as.integer(frame_num)
      ) %>% 
    add_classes(classes = c("tracks_df", "feature_df")) %>% 
    magrittr::set_attr(which = "completed", value = completed)
  
  return(out)
  
}


# w -----------------------------------------------------------------------

#' @title Coerce to \code{well_plate_df} data.frame
#' 
#' @description Coerces input data.frame to \code{well_plate_df} data.frame.
#'
#' @inherit as_cluster_df params
#'
#' @return The input data.frame with adjusted attributes and - if necessary - 
#' adjusted variables classes.
#' 
#' @export
#'

as_well_plate_df <- function(df){
  
  UseMethod(generic = "as_well_plate_df", object = df)
  
}

#' @rdname as_well_plate_df
#' @export
as_well_plate_df.cypro_df <- function(df){
  
  out <-
    dplyr::mutate(df, dplyr::across(.cols = dplyr::all_of(c("well_plate_name", "well", "well_roi")), .fns = base::as.factor)) %>% 
    dplyr::mutate(roi = base::as.integer(roi)) %>% 
    add_classes(classes = "well_plate_df")
  
  return(out)
  
}

#' @rdname as_well_plate_df
#' @export
asWellPlateDf <- function(df){
  
  UseMethod(generic = "asWellPlateDf", object = df)
  
}

#' @rdname as_well_plate_df
#' @export
asWellPlateDf.cypro_df <- as_well_plate_df.cypro_df







# C -----------------------------------------------------------------------

#' @rdname as_cypro_df
#' @export
asCyproDf <- function(df){
  
  UseMethod(generic = "asCyproDf", object = df)
}

#' @rdname as_cypro_df
#' @export
asCyproDf.data.frame <- as_cypro_df.data.frame



# L -----------------------------------------------------------------------

#' @rdname as_layout_df
#' @export
asLayoutDf <- function(df, well_plate_type){
  
  UseMethod(generic = "asLayoutDf", object = df)
  
}

#' @rdname as_layout_df
#' @export
asLayoutDf.data.frame <- as_layout_df.data.frame

#' @rdname as_layout_df
#' @export
asLayoutDfMP <- as_layout_df_mp 

#' @rdname as_layout_df
#' @export
asLayoutDfMP.layout_df <- as_layout_df_mp.layout_df



# S -----------------------------------------------------------------------

#' @rdname as_stats_df
#' @export
asStatsDf <- function(df){
  
  UseMethod(generic = "asStatsDf", object = df)
  
}

#' @rdname as_stats_df
#' @export
asStatsDf.cypro_df <- as_stats_df.cypro_df


# T -----------------------------------------------------------------------

#' @rdname as_tracks_df
#' @export
asTracksDf <- function(df){
  
  UseMethod(generic = "asTracksDf", object = df)
}

#' @rdname as_tracks_df
#' @export
asTracksDf.cypro_df <- as_tracks_df.cypro_df

