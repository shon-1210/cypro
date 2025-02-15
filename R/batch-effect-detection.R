





#' @title Detect batch effects
#' 
#' @description This function does the necessary computation to detect 
#' batch effects in your data. See details for more.  
#'
#' @inherit argument_dummy params
#' 
#' @details Batch effects result from undesired technical variation and can lead to 
#' misinterpretation and false conclusion. This does two things: 
#' First it creates summarized median profiles for each well by grouping the 
#' cell data by well plate name and well via \code{dplyr::group_by()} and 
#' then summarizes all stat variables via \code{dplyr::summarize_all()}. 
#' 
#' It then computes the distances between all wells. If wells of well plates 
#' that were set up in one batch cluster together it is likely that batch effects
#' impact your data. 
#' 
#' Hint: This logic only applies to experiment designs in which conditions are 
#' randomly distributed over well plates as a wells condition highly impacts
#' its median profile. 
#'
#' @inherit updated_object return
#' 
#' @seealso plotBatchHeatmap(), getBatchEffectDf()
#' 
#' @export
#'
detectBatchEffects <- function(object, verbose = TRUE){
  
  check_object(object)
  
  assign_default(object)

  # currently default (change to more options in future?)
  depth = "well"
  summarize_with = "median"
  phase = 1
    
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  confuns::check_one_of(input = depth, against = c("well"))
  
  # sufficient well plates? 
  well_plate_names <- getWellPlateNames(object)
  
  if(base::length(well_plate_names) == 1){
    
    base::stop("To detect batch effects across well plates at least two well plates must be stored in the cypro object.")
    
  }
  
  # summarize fun 
  check_summarize_with(summarize_with = summarize_with)
  
  stat_fun <- stat_funs[[summarize_with]]
    
  
  # compute well profiles 
  
  confuns::give_feedback(msg = "Computing median profiles by well.", verbose = verbose)
  
  well_profile_df <- 
    getStatsDf(object, phase = phase, with_well_plate = TRUE) %>% 
    dplyr::select(well_plate_name, !!rlang::sym(depth), where(base::is.numeric)) %>% 
    dplyr::group_by(well_plate_name, !!rlang::sym(depth)) %>% 
    dplyr::summarise_all(.funs = stat_fun, na.rm = TRUE) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(identifier = stringr::str_c(well_plate_name, !!rlang::sym(depth), sep = "_"))
  
  group_df <- dplyr::select(well_profile_df, -where(base::is.numeric))
  
  num_df <-
    tibble::column_to_rownames(well_profile_df, var = "identifier") %>% 
    dplyr::select(where(base::is.numeric))
  
  # compute distances 
  confuns::give_feedback(msg = "Computing euclidean distance across all wells.", verbose = verbose)
  
  dist_mtr <- stats::dist(x = num_df)
  
  object@qcheck$batch_effects <- 
    list(
      depth = depth, 
      profiles = well_profile_df, 
      dist_mtr = dist_mtr
    )
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}



#' @title Visualize possible batch effects
#' 
#' @description Visualizes the distances of the median well profiles cmoputed by 
#' \code{detectBatchEffects()} in a not clustered heatmap. 
#'
#' @inherit argument_dummy params  
#' @param correlated Logical value. If set to TRUE the correlation results 
#' of the distance matrix are used as input for the heatmap. 
#'
#' @return A heatmap as output from \code{pheatmap::pheatmap()}.
#' @export

plotBatchHeatmap <- function(object, clrp = "milo", verbose = NULL, correlated = FALSE){
  
  check_object(object)
  
  assign_default(object)
  
  dist_mtr <- object@qcheck$batch_effects$dist_mtr
  
  if(!"dist" %in% base::class(dist_mtr)){
    
    base::stop("Could not find required data. Please run 'detectBatchEffects()` first.")
    
  }
  
  if(base::isTRUE(correlated)){
    
    input_mtr <- 
      base::as.matrix(dist_mtr) %>% 
      stats::cor()
    
  } else {
    
    input_mtr <- base::as.matrix(dist_mtr)
    
  }
  
  annotation <- 
    object@qcheck$batch_effects$profiles %>% 
    dplyr::select(well_plate_name, identifier) %>% 
    tibble::column_to_rownames(var = "identifier")
  
  well_plates <- base::levels(annotation$well_plate_name)
  
  colors <- confuns::color_vector(clrp = clrp, names = well_plates)
  
  annotation_colors <- list(well_plate_name = colors)
  
  pheatmap::pheatmap(
    mat = input_mtr, 
    cluster_rows = FALSE, 
    cluster_cols = FALSE,
    annotation_col = annotation, 
    annotation_row = annotation,
    annotation_names_col = FALSE, 
    annotation_names_row = FALSE,
    annotation_colors = annotation_colors,
    show_rownames = FALSE, 
    show_colnames = FALSE
  )
  
}

















