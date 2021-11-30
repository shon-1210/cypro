#' @include S4-classes.R
NULL
#' @include S4-method-skeletons.R
NULL


# Analysis extraction ------------------------------------------------------

#' @title Obtain cypros clustering objects
#'
#' @inherit argument_dummy params 
#'
#' @return An S4 object of \emph{'hclust_conv'}, \emph{'kmeans_conv'} or \emph{'pam_conv'}.
#' @export
#'
getHclustConv <- function(object, feature_set, phase = NULL, with_data = TRUE){
  
  check_object(object)
  assign_default(object)
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase, max_phase = 1)
    
    cluster_object <- object@analysis$clustering$hclust[[feature_set]][[phase]]
    
  } else {
    
    cluster_object <- object@analysis$clustering$hclust[[feature_set]]
    
  }
  
  check_availability(
    evaluate = !base::is.null(cluster_object) & base::class(cluster_object) == "hclust_conv",
    phase = phase, 
    ref_input = glue::glue("hierarchical clustering object with variable set '{feature_set}'"), 
    ref_fun = "initiateHierarchicalClustering()"
  )
  
  cluster_object <- 
    hlpr_add_data_to_cluster_object(
      object = object,
      cluster_object,
      with_data = with_data,
      phase = phase
      )
  
  base::return(cluster_object)
  
}

#' @rdname getHclustConv
#' @export
getHclustObject <- getHclustConv

#' @rdname getHclustConv
#' @export
getKmeansConv <- function(object, feature_set, phase = NULL, with_data = TRUE){
  
  check_object(object)
  assign_default(object)
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    cluster_object <- object@analysis$clustering$kmeans[[feature_set]][[phase]]
    
  } else {
    
    cluster_object <- object@analysis$clustering$kmeans[[feature_set]]
    
  }
  
  check_availability(
    evaluate = !base::is.null(cluster_object) & base::class(cluster_object) == "kmeans_conv",
    phase = phase, 
    ref_input = glue::glue("kmeans clustering object with variable set '{feature_set}'"), 
    ref_fun = "initiateKmeansClustering()"
  )
  
  cluster_object <- 
    hlpr_add_data_to_cluster_object(
      object = object,
      cluster_object,
      with_data = with_data,
      phase = phase
      )
  
  base::return(cluster_object)
  
}

#' @rdname getHclustConv
#' @export
getKmeansObject <- getKmeansConv

#' @rdname getHclustConv
#' @export
getPamConv <- function(object, feature_set, phase = NULL, with_data = TRUE){
  
  check_object(object)
  assign_default(object)
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    cluster_object <- object@analysis$clustering$pam[[feature_set]][[phase]]
    
  } else {
    
    cluster_object <- object@analysis$clustering$pam[[feature_set]]
    
  }
  
  check_availability(
    evaluate = !base::is.null(cluster_object) & base::class(cluster_object) == "pam_conv",
    phase = phase, 
    ref_input = glue::glue("PAM clustering object with variable set '{feature_set}'"), 
    ref_fun = "initiatePamClustering()"
  )
  
  cluster_object <- 
    hlpr_add_data_to_cluster_object(
      object = object,
      cluster_object,
      with_data = with_data,
      phase = phase
      )
  
  base::return(cluster_object)
  
}

#' @rdname getHclustConv
#' @export
getPamObject <- getPamConv




# -----



# Batch effects -----------------------------------------------------------

#' @title Obtain batch effect computation results
#' 
#' @description Returns the distances across well plate wells computed 
#' by \code{detectBatchEffects()} either as a data.frame or as 
#' a distance matrix. 
#'
#' @inherit argument_dummy params
#' @param reduce Logical value. If set to TRUE (the default) the data.frame
#' is reduced to the unique combinations of well plate well, else each combination 
#' appears to times in the output data.frame.
#'
#' @return A data.frame or an object of class \emph{dist}.
#' @export
#'
getBatchEffectDf <- function(object, reduce = TRUE, verbose = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  dist_mtr <- object@qcheck$batch_effects$dist_mtr
  
  if(!"dist" %in% base::class(dist_mtr)){
    
    base::stop("Could not find required data. Please run 'detectBatchEffects()` first.")
    
  }
  
  batch_eff_df <- 
    object@qcheck$batch_effects$dist_mtr %>% 
    base::as.matrix() %>% 
    reshape2::melt(value.name = "distances")
  
  if(base::isTRUE(reduce)){
    
    well_plate_wells <- base::levels(batch_eff_df$Var1)
    
    empty_df <- 
      utils::combn(x = well_plate_wells, m = 2) %>% 
      base::t() %>% 
      base::as.data.frame() %>% 
      magrittr::set_colnames(c("Var1", "Var2")) %>% 
      tibble::as_tibble()
    
    output_df <- 
      dplyr::left_join(x = empty_df, y = batch_eff_df, by = c("Var1", "Var2"))
    
  } else {
    
    output_df <- batch_eff_df
    
  }
  
  base::return(output_df)
  
}

#' @rdname getBatchEffectDf
#' @export
getBatchEffectDist <- function(object, verbose = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  dist_mtr <- object@qcheck$batch_effects$dist_mtr
  
  if(!"dist" %in% base::class(dist_mtr)){
    
    base::stop("Could not find required data. Please run 'detectBatchEffects()` first.")
    
  }
  
  base::return(dist_mtr)
  
}


#' @title Obtain possible outlier wells 
#' 
#' @description Returns a data.frame of well plate wells that indicates
#' how often a wells distance value to other wells is assumed to 
#' be an outlier by \code{grDevices::boxplot.stats()}.
#'
#' @inherit argument_dummy params
#' @param threshold Numeric value. The minimum percentage of wells to which 
#' a wells distance must be assumed to be an outlier. Defaults to 0.75.
#'
#' @return A data.frame of well plate wells. 
#' @export
#'
getOutlierWells <- function(object, threshold = 0.75, verbose = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  confuns::is_value(threshold, mode = "numeric")
  
  if(!stringr::str_detect(threshold, pattern = "^0\\.|^\\.")){
    
    base::stop("Input for argument 'threshold' must be specified as a decimal number. (e.g.: 0.75).")
    
  }
  
  batch_df <- getBatchEffectDf(object, verbose = verbose, reduce = TRUE)
  
  box_stats <- grDevices::boxplot.stats(x = batch_df$distances)
  
  outliers <- box_stats$out
  
  outlier_df <-
    dplyr::filter(batch_df, distances %in% {{outliers}})
  
  outlier_df$Var1 -> var1
  
  outlier_df$Var2 -> var2
  
  n_well_plate_wells <- 
    getWellPlateDf(object) %>% 
    dplyr::select(well_plate_name, well) %>% 
    dplyr::distinct() %>% 
    base::nrow()
  
  outlier_wells_df <- 
    base::table(c(var1, var2)) %>% 
    base::as.data.frame() %>% 
    dplyr::arrange(dplyr::desc(Freq)) %>% 
    dplyr::transmute(
      well_plate_name = stringr::str_remove(Var1, pattern = stringr::str_c("_", well_regex, "$")), 
      well = stringr::str_extract(Var1, pattern = stringr::str_c(well_regex, "$")), 
      freq = Freq, 
      perc = base::round(freq / n_well_plate_wells, digits = 2)
    ) %>% 
    dplyr::filter(perc >= {{threshold}})
  
  if(base::nrow(outlier_wells_df) == 0){
    
    base::stop(
      glue::glue("No well plate wells remainingn if threshold set to {ref} percent of cases.", 
                 ref = stringr::str_remove(threshold, pattern = "^0\\.|^\\."))
      )
    
  }
  
  base::return(outlier_wells_df)
  
}

# -----


# Missing values ----------------------------------------------------------

#' @title Obtain missing value counts
#' 
#' @description This function returns a data.frame giving insight into 
#' the number of missing values every cell has across all variables. 
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame.
#' @export
#'

setGeneric(name = "getMissingValuesDf", def = function(object, ...){
  
  standardGeneric(f = "getMissingValuesDf")
  
})

#' @rdname getMissingValuesDf
#' @export
setMethod(f = "getMissingValuesDf", signature = "Cypro", definition = get_missing_values_df)

#' @rdname getMissingValuesDf
#' @export
setMethod(f = "getMissingValuesDf", signature = "CyproTimeLapseMP", definition = get_missing_values_df_mp)
# -----









