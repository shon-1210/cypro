#' @include S4-documentation.R
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


#' @title Obtain cypros correlation objects
#'
#' @inherit argument_dummy params
#'
#' @return An S4 object of class \emph{'corr_conv'}
#' @export
#'
getCorrConv <- function(object, feature_set, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    corr_object <- object@analysis$correlation[[feature_set]][[phase]]
    
  } else {
    
    corr_object <- object@analysis$correlation[[feature_set]]
    
  }
  
  check_availability(
    evaluate = !base::is.null(corr_object) & base::class(corr_object) == "corr_conv",
    phase = phase, 
    ref_input = "correlation object", 
    ref_fun = "initiateCorrelation()"
  )
  
  corr_object@meta <- 
    dplyr::left_join(
      x = corr_object@meta, 
      y = getGroupingDf(object, phase = phase, verbose = FALSE), 
      by = c("key" = "cell_id"))
  
  corr_object@data <- 
    getStatsDf(object = object, phase = phase) %>% 
    tibble::column_to_rownames(var = "cell_id") %>% 
    dplyr::select(dplyr::all_of(corr_object@variables_num)) %>% 
    base::as.matrix()
  
  corr_object@variables_discrete <-
    getGroupingVariableNames(object, phase = phase, verbose = FALSE)
  
  base::return(corr_object)
  
}

#' @rdname getCorrConv
#' @export
getCorrObject <- getCorrConv


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

# Cell Ids ----------------------------------------------------------------

#' @title Get cell ids
#' 
#' @description Using the ... options the data can be 
#' subsetted in the style of \code{dplyr::filter()}. 
#'
#' @inherit argument_dummy params 
#' @inherit dplyr::filter params
#'
#' @return Character vector of cell ids.
#' 
#' @details Cell IDs are extracted from the feature data.frame in case 
#' of \code{CyproScreening} objects and from the stats data.frame in 
#' case of \code{CyproTimeLapse(MP)} objects.
#' 
#' @export
#'

setGeneric(name = "getCellIds", def = function(object, ...){
  
  standardGeneric(f = "getCellIds")
  
})

#' @rdname getCellIds
#' @export
setMethod(f = "getCellIds", signature = "CyproScreening", definition = function(object, ...){
  
  filtering <- rlang::enquos(...)
  
  cell_ids <- 
    getFeatureDf(object = object, with_grouping = TRUE) %>% 
    dplyr::filter(!!!filtering) %>% 
    dplyr::pull(cell_id) %>% 
    base::unique()
  
  return(cell_ids)
  
})

#' @rdname getCellIds
#' @export
setMethod(f = "getCellIds", signature = "CyproTimeLapse", definition = function(object, ...){
  
  filtering <- rlang::enquos(...)
  
  cell_ids <- 
    getStatsDf(object = object, with_grouping = TRUE) %>% 
    dplyr::filter(!!!filtering) %>% 
    dplyr::pull(cell_id) %>% 
    base::unique()
  
  return(cell_ids)
  
})

#' @rdname getCellIds
#' @export
setMethod(f = "getCellIds", signature = "CyproTimeLapseMP", definition = function(object, phase = NULL, ...){
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  filtering <- rlang::enquos(...)
  
  cell_ids <- 
    getStatsDf(object = object, phase = phase, with_grouping = TRUE) %>% 
    dplyr::filter(!!!filtering) %>% 
    dplyr::pull(cell_id) %>% 
    base::unique()
  
  return(cell_ids)
  
})

# -----
# Data extraction ---------------------------------------------------------

#' @title Obtain cluster data (by cell id)
#' 
#' @description This function lets you extract a data.frame that contains variables 
#' with which cells are clustered. 
#'
#' @inherit argument_dummy params
#' 
#' @return A data.frame.
#' @export
#'

setGeneric(name = "getClusterDf", def = function(object, ...){
  
  standardGeneric(f = "getClusterDf")
  
})


#' @rdname getClusterDf
#' @export
setMethod(f = "getClusterDf", signature = "Cypro", definition = function(object,
                                                                         verbose = NULL,
                                                                         ...){
  
  assign_default(object = object)
  
  
  cluster_df <- 
    getCdata(object) %>% 
    getClusterDf()
  
  if(base::all(base::colnames(cluster_df) == "cell_id")){
    
    confuns::give_feedback(
      msg = "No cluster variables have been added yet.",
      verbose = verbose, 
      with.time = FALSE
    )
    
  }
  
  return(cluster_df)
  
})

#' @rdname getClusterDf
#' @export
setMethod(f = "getClusterDf", signature = "CyproTimeLapseMP", definition = function(object,
                                                                                    phase = NULL,
                                                                                    verbose = NULL, 
                                                                                    ...){
  
  assign_default(object = object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cluster_df <- 
    getCdata(object) %>% 
    getClusterDf(phase = phase)
  
  if(base::all(base::colnames(cluster_df) == "cell_id")){
    
    confuns::give_feedback(
      msg = glue::glue("No cluster variables have been added yet for {phase} phase."),
      verbose = verbose, 
      with.time = FALSE
    )
    
  }
  
  return(cluster_df)
  
})

#' @title Obtain numeric data (by cell id)
#' 
#' @description This function lets you extract a data.frame that contains all variables 
#' with which cells can be grouped. 
#'
#' @inherit argument_dummy params
#' 
#' @seealso \code{getTracksDf()} and \code{getStatsDf()} to obtain numeric data
#' of \code{Cypro} objects from time lapse experiments.
#' 
#' @return A data.frame.
#' @export
#'

setGeneric(name = "getFeatureDf", def = function(object, ...){
  
  standardGeneric(f = "getFeatureDf")
  
})

#' @rdname getFeatureDf
#' @export
setMethod(
  f = "getFeatureDf",
  signature = "CyproScreening", 
  definition = function(object,
                        drop_na = FALSE,
                        with_grouping = NULL,
                        with_cluster = NULL,
                        with_meta = NULL,
                        with_well_plate = NULL,
                        verbose = NULL,
                        ...){
    
    assign_default(object)
    
    if(base::isFALSE(with_grouping)){
      
      with_cluster <- FALSE
      with_meta <- FALSE
      with_well_plate <- FALSE
      
    }
    
    df <- 
      getCdata(object) %>% 
      getFeatureDf()
    
    # add cluster
    if(base::isTRUE(with_cluster) | base::isTRUE(with_grouping)){
      
      cluster_df <- getClusterDf(object, verbose = FALSE)  
      
      df <- dplyr::left_join(x = df, y = cluster_df, by = "cell_id")
      
    }
    
    # add meta
    if(base::isTRUE(with_meta) | base::isTRUE(with_grouping)){
      
      meta_df <- getMetaDf(object)
      
      df <- dplyr::left_join(x = df, y = meta_df, by = "cell_id")
      
    }
    
    # add well plate info
    if(base::isTRUE(with_well_plate) | base::isTRUE(with_grouping)){
      
      wp_df <- getWellPlateDf(object)
      
      df <- dplyr::left_join(x = df, y = wp_df, by = "cell_id")
      
    }
    
    if(base::isTRUE(drop_na)){
      
      df <- tidyr::drop_na(df)
      
    }
    
    return(df)  
    
  }
  )

#' @rdname getFeatureDf
#' @export
setMethod(
  f = "getFeatureDf", 
  signature = "CyproTimeLapse", 
  definition = function(object, 
                        slot = "stats", 
                        drop_na = FALSE,
                        with_grouping = NULL, 
                        with_cluster = NULL, 
                        with_meta = NULL, 
                        with_well_plate = NULL, 
                        verbose = NULL
  ){
    
    check_object(object)
    assign_default(object)
    
    confuns::is_value(x = slot, mode = "character")
    
    confuns::check_one_of(
      input = slot, 
      against = c("stats", "tracks")
    )
    
    if(slot == "tracks"){
      
      df <- 
        getTracksDf(
          object = object, 
          with_grouping = with_grouping, 
          with_cluster = with_cluster, 
          with_meta = with_meta, 
          with_well_plate = with_well_plate, 
          verbose = verbose, 
          drop_na = drop_na
        )
      
    } else if(slott == "stats"){
      
      df <- 
        getStatsDf(
          object = object, 
          with_grouping = with_grouping, 
          with_cluster = with_cluster, 
          with_meta = with_meta, 
          with_well_plate = with_well_plate, 
          verbose = verbose, 
          drop_na = drop_na
        )
      
    }
    
    return(df)
    
  }
)

#' @rdname getFeatureDf
#' @export
setMethod(
  f = "getFeatureDf", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, 
                        slot = "stats", 
                        drop_na = FALSE,
                        with_grouping = NULL, 
                        with_cluster = NULL, 
                        with_meta = NULL, 
                        with_well_plate = NULL, 
                        phase = NULL, 
                        verbose = NULL
  ){
    
    check_object(object)
    assign_default(object)
    
    confuns::is_value(x = slot, mode = "character")
    
    confuns::check_one_of(
      input = slot, 
      against = c("stats", "tracks")
    )
    
    if(slot == "tracks"){
      
      df <- 
        getTracksDf(
          object = object, 
          with_grouping = with_grouping, 
          with_cluster = with_cluster, 
          with_meta = with_meta, 
          with_well_plate = with_well_plate, 
          phase = phase,
          verbose = verbose, 
          drop_na = drop_na
        )
      
    } else if(slott == "stats"){
      
      df <- 
        getStatsDf(
          object = object, 
          with_grouping = with_grouping, 
          with_cluster = with_cluster, 
          with_meta = with_meta, 
          with_well_plate = with_well_plate, 
          phase = phase, 
          verbose = verbose, 
          drop_na = drop_na
        )
      
    }
    
    return(df)
    
  }
)



#' @title Obtain grouping data (by cell id)
#' 
#' @description This function lets you extract a data.frame that contains all variables 
#' with which cells can be grouped. 
#'
#' @inherit argument_dummy params
#' 
#' @return A data.frame.
#' @export
#'

setGeneric(name = "getGroupingDf", def = function(object, ...){
  
  standardGeneric(f = "getGroupingDf")
  
})

#' @rdname getGroupingDf
#' @export
setMethod(
  f = "getGroupingDf",
  signature = "Cypro",
  definition = function(object,
                        verbose = NULL,
                        ...){
  
  group_df <- 
    dplyr::left_join(
      x = getWellPlateDf(object), 
      y = getMetaDf(object), 
      by = "cell_id"
    ) %>% 
    dplyr::left_join(
      x = ., 
      y = getClusterDf(object, verbose = verbose), 
      by = "cell_id"
    )
  
  return(group_df)
  
}
)

#' @rdname getGroupingDf
#' @export
setMethod(
  f = "getGroupingDf",
  signature = "CyproTimeLapseMP",
  definition = function(object,
                        phase = NULL,
                        verbose = NULL,
                        ...){
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  df <- 
    dplyr::left_join(
      x = getWellPlateDf(object), 
      y = getMetaDf(object, phase = phase), 
      by = "cell_id"
    ) %>% 
    dplyr::left_join(
      x = ., 
      y = getClusterDf(object, phase = phase, verbose = verbose)
    )
  
  return(df)
  
}
)



#' @title Obtain meta data (by cell id)
#' 
#' @description This function lets you extract a data.frame that contain variables 
#' with which cells are grouped. 
#'
#' @inherit argument_dummy params
#' 
#' @return A data.frame.
#' @export

setGeneric(name = "getMetaDf", def = function(object, ...){
  
  standardGeneric(f = "getMetaDf")
  
})

#' @rdname getMetaDf
#' @export
setMethod(f = "getMetaDf", signature = "Cypro", definition = function(object){
  
  check_object(object)
  
  meta_df <- 
    getCdata(object) %>% 
    getMetaDf()
  
  return(meta_df)
  
})

#' @rdname getMetaDf
#' @export
setMethod(f = "getMetaDf", signature = "CyproTimeLapseMP", definition = function(object, phase = NULL){
  
  assign_default(object = object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  meta_df <- 
    getCdata(object) %>% 
    getMetaDf(phase = phase)
  
  return(meta_df)
  
})


#' @title Obtain stat data.frame 
#' 
#' @inherit argument_dummy params
#' @param with_cluster,with_meta,with_well_plate Logical values. Denoting 
#' if the respective grouping information should be joined to the stats data.frame
#' or not.
#'
#' @return A data.frame with all numeric variables summarizing the measurements of 
#' the track data.frame. 
#' 
#' @export
#'

setGeneric(name = "getStatsDf", def = function(object, ...){
  
  standardGeneric(f = "getStatsDf")
  
})

#' @rdname getStatsDf
#' @export
setMethod(f = "getStatsDf", signature = "CyproTimeLapse", definition = get_stats_df)

#' @rdname getStatsDf
#' @export
setMethod(f = "getStatsDf", signature = "CyproTimeLapseMP", definition = get_stats_df_mp)


#' @title Obtain track data.frame. 
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame in which each observation refers to a cell at a given frame.
#' 
#' @export
#'

setGeneric(name = "getTracksDf", def = function(object, ...){
  
  standardGeneric(f = "getTracksDf")
  
})

#' @rdname getTracksDf
#' @export
setMethod(f = "getTracksDf", signature = "CyproTimeLapse", definition = get_tracks_df)

#' @rdname getTracksDf
#' @export
setMethod(f = "getTracksDf", signature = "CyproTimeLapseMP", definition = get_tracks_df_mp)


#' @title Obtain well plate data (by cell id)
#' 
#' @description This function lets you extract a data.frame that contain variables 
#' with which cells are grouped. 
#'
#' @inherit argument_dummy params
#' 
#' @return A data.frame.
#' @export

setGeneric(name = "getWellPlateDf", def = function(object){
  
  standardGeneric(f = "getWellPlateDf")
  
})

#' @rdname getWellPlateDf
#' @export
setMethod(f = "getWellPlateDf", signature = "Cypro", definition = function(object){
  
  check_object(object)
  
  wp_df <- object@cdata@well_plate
  
  return(wp_df)
  
})

#' @title Obtain well plate set up 
#' 
#' @description Access function to the experiment set up in a tidy-data fashion. 
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame in which each observation represents the well of a well plate
#' @export
#'

setGeneric(name = "getSetUpDf", def = function(object, ...){
  
  standardGeneric(f = "getSetUpDf")
  
})

#' @rdname getSetUpDf
#' @export
setMethod(f = "getSetUpDf", signature = "Cypro", function(object, well_plate, ...){
  
  confuns::check_one_of(
    input = well_plate, 
    against = getWellPlateNames(object)
  )
  
  exp_design <- getExperimentDesign(object)
  
  set_up_df <- exp_design@well_plates[[well_plate]][[set_up]]
  
  return(set_up_df)
  
})

#' @title Obtain defined set of data variables
#' 
#' @description Convenient access to defined sets of variables or names 
#' mentioned sets. Useful to obtain vectors for recurrent arguments like 
#' \code{variables}.
#'
#' @inherit argument_dummy params
#'
#' @return A character vector. 
#' @export
#'

setGeneric(name = "getFeatureSet", def = function(object, feature_set){
  
  standardGeneric(f = "getFeatureSet")
  
})

#' @rdname getFeatureSet 
#' @export 
setMethod(f = "getFeatureSet", signature = "Cypro", definition = function(object){
  
  if(base::length(base::names(object@feature_sets)) == 0){
    
    stop("No variable sets have been defined yet.")
    
  }
  
  confuns::check_one_of(
    input = feature_set, 
    against = base::names(object@feature_sets), 
    fdb.opt = 2, 
    ref.opt.2 = "defined variable sets"
  )
  
  fset <- object@feature_sets[[feature_set]]
  
  return(fset)
  
})

#' @title Obtain defined sets of data variables 
#' 
#' @inherit getFeatureSet description
#' @inherit argument_dummy params
#' 
#' @return A list of character vectors.
#' 
#' @export

setGeneric(name = "getFeatureSets", def = function(object){
  
  standardGeneric(f = "getFeatureSets")
  
})

#' @rdname getFeatureSet 
#' @export 
setMethod(f = "getFeatureSets", signature = "Cypro", definition = function(object){
  
  return(object@feature_sets)
  
})

#' @title Obtain names of defined sets of data variables 
#' 
#' @inherit argument_dummy params
#' 
#' @return A character vector.
#' 
#' @export

setGeneric(name = "getFeatureSetNames", def = function(object){
  
  standardGeneric(f = "getFeatureSetNames")
  
})

#' @rdname getFeatureSet 
#' @export 
setMethod(f = "getFeatureSetNames", signature = "Cypro", definition = function(object){
  
  return(base::names(object@feature_sets))
  
})


# -----









# Miscellaneous -----------------------------------------------------------

#' @title Obtain frame time sequence
#' 
#' @description Convenient access to the time line as a numeric vector.
#'
#' @inherit argument_dummy params 
#'
#' @return A numeric vector.
#' @export
#'

setGeneric(name = "getFrameTimeSeq", def = function(object, ...){
  
  standardGeneric(f = "getFrameTimeSeq")
  
})

#' @rdname getFrameTimeSeq
#' @export
setMethod(f = "getFrameTimeSeq", signature = "CyproTimeLapse", definition = get_frame_time_seq)

#' @rdname getFrameTimeSeq
#' @export
setMethod(f = "getFrameTimeSeq", signature = "CyproTimeLapseMP", definition = get_frame_time_seq_mp)


#' @title Obtain the object's default instructions
#' 
#' @description Convenient access to what is currently defined as the default. 
#'
#' @inherit argument_dummy params
#' @param return Character value. Either \emph{'list'} (default) or \emph{'S4'}.
#'
#' @return A list or an S4 object.
#' @export
#'

setGeneric(name = "getDefaultInstructions", def = function(object, ...){
  
  standardGeneric(f = "getDefaultInstructions")
  
})

#' @rdname getDefaultInstructions
#' @export

setMethod(f = "getDefaultInstructions", signature = "Cypro", definition = function(object, return = "list"){
  
  check_object(object)
  
  return(object@default)
  
})

#' @title Obtain vector of phase names
#' 
#' @inherit argument_dummy params
#' 
#' @return Character vector.
#' @export

setGeneric(name = "getPhases", def = function(object){
  
  standardGeneric(f = "getPhases")
  
})

#' @rdname getPhases
#' @export
setMethod(f = "getPhases", signature = "CyproTimeLapseMP", definition = function(object){
  
  object@design@phases %>% base::names()
  
})

#' @title Obtain list of subset information 
#' 
#' @description Whenever a \code{Cypro} object is subsetted by one of it's 
#' \code{subsetBy*()} methods information about the subsetting is stored in 
#' the resulting object. This functions uses as an extractor for these 
#' information. 
#'
#' @param nth Numeric value. Denotes the ordinal number of the subsetting 
#' you are interested in.
#'
#' @return A list. 
#' @export
#'
setGeneric(name = "getSubsetList", def = function(object, nth){
  
  standardGeneric("getSubsetList")
  
})

#' @rdname getSubsetList
#' @export
setMethod(f = "getSubsetList", signature = "Cypro", definition = function(object, nth = 1){
  
  check_object(object)
  
  nth <- english::ordinal(x = nth)
  
  subset_list <- object@information$subset[[nth]]
  
  if(!base::is.list(subset_list)){
    
    msg <- glue::glue("Could not find info for a {nth} subsetting.")
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  return(subset_list)
  
})



#' @title Obtain well plate indices
#'
#' @inherit argument_dummy params 
#' @param return Character value. Either \emph{'tibble'} or \emph{'vector'}.
#'
#' @return Depends on input for argument \code{return}.
#' @export
#'

setGeneric(name = "getWellPlateIndices", def = function(object, ...){
  
  standardGeneric(f = "getWellPlateIndices")
  
})

#' @rdname getWellPlateIndices
#' @export
setMethod(f = "getWellPlateIndices", signature = "Cypro", definition = function(object, return = "tibble"){
  
  wps <- object@design@well_plates
  
  wp_names <- base::names(wps)
  
  wp_indices <- 
    purrr::map_int(.x = wps, .f = ~ methods::slot(.x, "index"))
  
  
  if(return == "tibble"){
    
    res <- 
      base::data.frame(
        well_plate_name = wp_names, 
        well_plate_index = wp_indices
      ) %>% 
      tibble::as_tibble()
    
  } else if(return == "vector"){
    
    res <- 
      purrr::set_names(wp_indices, wp_names)
    
  }
  
  return(res)
    
})

#
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
# Names -------------------------------------------------------------------

# Exported ---


#' @title Obtain group names a grouping variable contains
#' 
#' @description This function returns the names of the groups in which a specific grouping
#' variable groups the cells. Useful to obtain input options for arguments like \code{across_subset}. 
#'
#' @inherit argument_dummy params
#' @param grouping_variable Character value. Denotes the discrete variable - the grouping of cells - 
#' of interest. Use \code{getGroupingVariableNames()} to obtain all valid input options. 
#'
#' @return Character vector of group names. 
#' 
#' @export

setGeneric(name = "getGroupNames", def = function(object, ...){
  
  standardGeneric(f = "getGroupNames")
  
})

#' @rdname getGroupNames
#' @export
setMethod(f = "getGroupNames", signature = "Cypro", definition = get_group_names)

#' @rdname getGroupNames
#' @export
setMethod(f = "getGroupNames", signature = "CyproTimeLapseMP", definition = get_group_names_mp)



#' @title Obtain cluster variable names of cell data
#' 
#' @description Convenient access to the names of the data variables of your cell data. Useful to 
#' obtain vectors of variable names as input for recurring arguments like \code{across} or
#' \code{grouping_variable}.
#'
#' @inherit argument_dummy params
#' @param named Logical value. If set to TRUE the grouping variables are named 
#' according to their grouping type (cluster, meta or well_plate).
#' @param ... Additional selection helpers from the \code{tidyselect} package that match 
#' variable names according to a given pattern. 
#' 
#' @return A character vector. 
#' 
#' @seealso starts_with(), ends_with(), contains(), matches()
#' 
#' @export

setGeneric(name = "getClusterVariableNames", def = function(object, ...){
  
  standardGeneric(f = "getClusterVariableNames")
  
})

#' @rdname getClusterVariableNames
#' @export
setMethod(
  f = "getClusterVariableNames",
  signature = "Cypro",
  definition = function(object, ..., named = FALSE, verbose = NULL){
    
    check_object(object)
    
    assign_default(object)
    
    group_df <- 
      getClusterDf(object, verbose = TRUE) %>% 
      dplyr::select(-cell_id)
    
    res <- 
      get_grouping_variable_names(
        group_df = group_df, 
        named = named, 
        verbose = verbose, 
      ) %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  }
)

#' @rdname getClusterVariableNames
#' @export
setMethod(
  f = "getClusterVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, ..., named = FALSE, phase = NULL, verbose = NULL){
    
    check_object(object)
    
    assign_default(object)
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    group_df <- 
      getClusterDf(object, verbose = TRUE, phase = phase) %>% 
      dplyr::select(-cell_id)
    
    res <- 
      get_grouping_variable_names(
        group_df = group_df, 
        named = named, 
        verbose = verbose, 
        ...
      ) %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  }
)

#' @title Obtain grouping variable names of cell data
#' 
#' @inherit getClusterVariableNames description params
#' 
#' @return A character vector. 
#' 
#' @seealso starts_with(), ends_with(), contains(), matches()
#' 
#' @export

setGeneric(name = "getGroupingVariableNames", def = function(object, ...){
  
  standardGeneric(f = "getGroupingVariableNames")
  
})

#' @rdname getGroupingVariableNames
#' @export
setMethod(
  f = "getGroupingVariableNames",
  signature = "Cypro",
  definition = function(object, ..., named = FALSE, verbose = NULL){
    
    check_object(object)
    
    assign_default(object)
    
    group_df <- 
      getGroupingDf(object, verbose = verbose) %>% 
      dplyr::select(-cell_id)
    
    res <- 
      get_grouping_variable_names(
        group_df = group_df, 
        named = named, 
        verbose = verbose, 
      ) %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  }
)


#' @rdname getGroupingVariableNames
#' @export
setMethod(
  f = "getGroupingVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, ..., named = FALSE, phase = NULL, verbose = NULL){
    
    check_object(object)
    
    assign_default(object)
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    group_df <- 
      getGroupingDf(object, verbose = verbose, phase = phase) %>% 
      dplyr::select(-cell_id)
    
    res <- 
      get_grouping_variable_names(
        group_df = group_df, 
        named = named, 
        verbose = verbose, 
        ...
      ) %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  }
)

#' @title Obtain meta variable names of cell data
#' 
#' @inherit getClusterVariableNames description params
#' 
#' @return A character vector. 
#' 
#' @seealso starts_with(), ends_with(), contains(), matches()
#' 
#' @export

setGeneric(name = "getMetaVariableNames", def = function(object, ...){
  
  standardGeneric(f = "getMetaVariableNames")
  
})

#' @rdname getMetaVariableNames
#' @export
setMethod(
  f = "getMetaVariableNames",
  signature = "Cypro",
  definition = function(object, ..., named = FALSE, verbose = NULL){
    
    check_object(object)
    
    assign_default(object)
    
    res <- 
      getMetaDf(object) %>% 
      dplyr::select(-cell_id) %>% 
      base::colnames() %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  }
)


#' @rdname getMetaVariableNames
#' @export
setMethod(
  f = "getMetaVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, ..., named = FALSE, phase = NULL, verbose = NULL){
    
    check_object(object)
    
    assign_default(object)
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    res <- 
      getMetaDf(object, verbose = TRUE, phase = phase) %>% 
      dplyr::select(-cell_id) %>% 
      base::colnames() %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  }
)

#' @title Obtain well plate variable names of cell data
#' 
#' @inherit getClusterVariableNames description params
#' 
#' @return A character vector. 
#' 
#' @seealso starts_with(), ends_with(), contains(), matches()
#' 
#' @export

setGeneric(name = "getWellPlateVariableNames", def = function(object, ...){
  
  standardGeneric(f = "getWellPlateVariableNames")
  
})

#' @rdname getWellPlateVariableNames
#' @export
setMethod(f = "getWellPlateVariableNames", signature = "Cypro", definition = function(object, ...){
  
  check_object(object)
  
  wp_names <- 
    getWellPlateDf(object) %>% 
    dplyr::select(-cell_id) %>% 
    base::colnames() %>% 
    confuns::vselect(input = ., ...) 
  
  return(wp_names)
  
}
)


#' @title Obtain numeric variables of cell stat data 
#'
#' @description Convenient access to the names of the numeric data variables of your 
#' cell data. Useful to obtain vectors of variable names as input for recurring
#' arguments like \code{variables}.
#'
#' @inherit argument_dummy params
#' @param ... Additional selection helpers from the \code{tidyselect} package that match 
#' variable names according to a given pattern. 
#' 
#' @return A character vector. 
#' 
#' @seealso starts_with(), ends_with(), contains(), matches()
#' 
#' @export
#'

setGeneric(name = "getStatVariableNames", def = function(object, ...){
  
  standardGeneric(f = "getStatVariableNames")
  
})

#' @rdname getStatVariableNames
#' @export
setMethod(
  f = "getStatVariableNames",
  signature = "CyproTimeLapse",
  definition = function(object, ...){
    
    check_object(object)
    
    res <- 
      getStatsDf(object, with_grouping = FALSE) %>% 
      dplyr::select(-cell_id) %>% 
      base::colnames() %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  })

#' @rdname getStatVariableNames
#' @export
setMethod(
  f = "getStatVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, phase = NULL, ...){
    
    check_object(object)
    
    assign_default(object)
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    res <- 
      getStatsDf(object, phase = phase, with_grouping = FALSE) %>% 
      dplyr::select(-cell_id) %>% 
      base::colnames() %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
    
  }
)


#' @title Obtain numeric variables of cell track data 
#'
#' @inherit getStatVariableNames description params
#' 
#' @return A character vector. 
#' 
#' @seealso starts_with(), ends_with(), contains(), matches()
#' 
#' @export
#' 

setGeneric(name = "getTrackVariableNames", def = function(object, ...){
  
  standardGeneric(f = "getTrackVariableNames")
  
})

#' @rdname getTrackVariableNames
#' @export
setMethod(
  f = "getTrackVariableNames",
  signature = "CyproTimeLapse",
  definition = function(object, ...){
    
    check_object(object)
    
    res <- 
      getTracksDf(object, with_grouping = FALSE) %>% 
      dplyr::select(-cell_id) %>% 
      base::colnames() %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  })

#' @rdname getTrackVariableNames
#' @export
setMethod(
  f = "getTrackVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, phase = NULL, ...){
    
    check_object(object)
    
    assign_default(object)
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    res <- 
      getTracksDf(object, phase = phase, with_grouping = FALSE) %>% 
      dplyr::select(-cell_id) %>% 
      base::colnames() %>% 
      confuns::vselect(input = ., ...)
    
    return(res)
    
  }
)

#' @title Obtain well plate names
#'
#' @inherit argument_dummy params
#'
#' @return A character vector. 
#' @export

setGeneric(name = "getWellPlateNames", def = function(object){
  
  standardGeneric(f = "getWellPlateNames")
  
})

#' @rdname getWellPlateNames
#' @export 
setMethod(f = "getWellPlateNames", signature = "Cypro", def = function(object){
  
  return(base::names(object@well_plates))  
  
})

#' @title Obtain cell line and condition names 
#' 
#' @description Quick wrapper around the functionality of getGroupingVariableNames().
#'
#' @inherit check_object params 
#'
#' @details Useful helper function when it comes to specify conditions and cell lines 
#' of interest via the \code{across_subset}-argument.
#' 
#' @return Character vector.
#' @export
#'

setGeneric(name = "getCellLines", def = function(object){
  
  standardGeneric(f = "getCellLines")
  
})

#' @rdname getCellLines
#' @export
setMethod(f = "getCellLines", signature = "Cypro", function(object){
  
  check_object(object)
  
  cell_lines <- 
    getCdata(object) %>% 
    getMetaDf() %>% 
    pull(cell_line) %>% 
    base::levels()
  
  return(cell_lines)
  
})

#' @rdname getCellLines
#' @export
#' 

setGeneric(name = "getConditions", def = function(object, ...){
  
  standardGeneric(f = "getConditions")
  
})

#' @rdname getCellLines
#' @export
setMethod(f = "getConditions", signature = "Cypro", definition = function(object){
  
  check_object(object)
  
  getMetaDf(object) %>% 
    dplyr::pull(condition) %>% 
    base::levels()
  
})

#' @rdname getCellLines
#' @export
setMethod(f = "getConditions", signature = "CyproTimeLapseMP", definition = function(object, phase = NULL){
  
  check_object(object)
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  getMetaDf(object, phase = phase) %>% 
    dplyr::pull(condition) %>% 
    base::levels()
  
})

#' @title Obtain storage directory
#' 
#' @inherit argument_dummy params
#' 
#' @return Character value. 
#' 
#' @export

setGeneric(name = "getStorageDirectory", def = function(object){
  
  standardGeneric(f = "getStorageDirectory")
  
})

#' @rdname getStorageDirectory
#' @export
setMethod(f = "getStorageDirectory", signature = "Cypro", definition = function(object){
  
  check_object(object, set_up_req = "experiment_design")
  
  dir <- object@information$storage_directory
  
  if(base::is.null(dir) | dir == "not defined yet"){
    
    base::stop("Storage directory has not beend defined yet.")
    
  } else if(!stringr::str_detect(dir, pattern = "\\.{1}RDS")){
    
    base::stop("Storage directory must end with '.RDS'.")
    
  } else {
    
    base::return(dir)
    
  }
  
})

# -----
# Outlier detection -------------------------------------------------------

#' @title Obtain outlier detection results 
#' 
#' @description These functions can be used to extract the results of the outlier 
#' detection algorithms. 
#'
#' @inherit argument_dummy params
#' 
#' @return \code{getOutlierResults()} returns a list in which each slot contains 
#' the results for a specific method. \code{getOutlierIds()} returns a character 
#' vector of cell ids containing all cell ids that have been detected as outliers
#' by at least one method.
#' @export
#'
getOutlierResults <- function(object,
                              method_outlier = NULL,
                              check = TRUE,
                              phase = NULL,
                              verbose = NULL){
  
  check_object(object)
  assign_default(object)

  phase <- check_phase(object, phase = phase, max_phases = 1)
    
  if(base::isTRUE(check)){
    
    if(!existOutlierResults(object)){
      
      base::stop("Did not find any outlier detection results.")
      
    }
    
  }
  
  if(multiplePhases(object)){
    
    outlier_list <- 
      purrr::map(
        .x = object@qcheck$outlier_detection, 
        .f = ~ .x[[phase]]
      ) %>% 
      purrr::discard(.p = base::is.null)
    
    if(base::length(outlier_list) == 0){
      
      msg <- 
        glue::glue("Did not find any outlier detection results for {phase} phase.")
      
      confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
      
    }
    
    confuns::give_feedback(
      msg = glue::glue("Returning outlier detection results for {phase} phase."), 
      verbose = verbose, 
      with.time = FALSE
    )
    
  } else {
    
    outlier_list <- 
      purrr::discard(.x = object@qcheck$outlier_detection, .p = base::is.null)
    
  }
  
  # subset by method
  if(base::is.character(method_outlier)){
    
    confuns::check_vector(
      input = method_outlier,
      against = base::names(outlier_list), 
      ref.input = "input for argument 'method_outlier'", 
      ref.against = "methods with which outliers have been detected", 
      fdb.fn = "stop"
      )
    
    outlier_list <- outlier_list[method_outlier]
    
    if(base::length(method_outlier) == 1){
      
      outlier_list <- outlier_list[[1]]
      
    }
    
  }
  
  base::return(outlier_list)
  
}

#' @rdname getOutlierResults
#' @export
getOutlierIds <- function(object,
                          method_outlier = NULL,
                          check = FALSE,
                          flatten = TRUE, 
                          phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phase = 1)
  
  existOutlierResults(object, phase = phase, method_outlier = method_outlier, verbose = TRUE)
  
  outlier_ids <- list()
  
  if("iqr" %in% method_outlier){
    
    outlier_list <-
      getOutlierResults(
        object = object,
        check = check,
        verbose = FALSE,
        method_outlier = "iqr", 
        phase = phase
        )
    
    outlier_ids$iqr <-  
      purrr::flatten(outlier_list$ids) %>% # flatten groups
      purrr::flatten_chr() %>% # flatten stat vars
      base::unique()
    
  }
  
  if("mahalanobis" %in% method_outlier){
    
    outlier_list <- 
      getOutlierResults(
        object = object,
        check = check,
        verbose = FALSE,
        method_outlier = "mahalanobis", 
        phase = phase
      )
  
    outlier_ids$mahalanobis <- 
      purrr::flatten_chr(outlier_list$ids)
      
  }
  

  if(base::isTRUE(flatten)){
    
    outlier_ids <- 
      purrr::flatten_chr(.x = outlier_ids) %>% # flatten outlier methods
      base::unique()
    
  }
  
  base::return(outlier_ids)
  
}


# -----





# NOT EXPORTED ------------------------------------------------------------

getNumericVariableNames <- function(object){
  
  warning("getNumericVariableNames() is deprecated.")
  
  getStatsDf(object = object) %>% 
    dplyr::select_if(.predicate = base::is.numeric) %>% 
    base::colnames()
  
}

getFrameSeq <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  getTracks(object, phase = phase) %>% 
    dplyr::pull(var = "frame_num") %>% 
    base::unique()
  
}

getFrameLevels <- function(object, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  getTracksDf(object, phase = phase) %>% 
    dplyr::arrange(frame_num) %>% 
    dplyr::pull(frame_itvl) %>% 
    base::unique()
  
}

getInterval <- function(object){
  
  object@set_up$itvl
  
}

getIntervalUnit <- function(object){
  
  object@set_up$itvl_u
  
}

getStatsOrTracksDf <- function(object, phase){
  
  warning("rewrite to getStatsDf()")
  
  if(!isTimeLapseExp(object)){
    
    df <- getTracksDf(object, phase = phase)
    
  } else {
    
    df <- getStatsDf(object, phase = phase)
    
  }
  
  return(df)
  
}

