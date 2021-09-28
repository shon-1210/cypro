

#' @title Detect outlier cells 
#' 
#' @description There are several methods according to which outlier cells can be detected or 
#' defined. Which one to use strongly depends on the researchers question at hand. See details
#' for more information on which method does what exactly.
#'
#' @inherit argument_dummy
#' 
#' @details This function only detects outliers. It does not remove them from the object. Use \code{removeOutliers()}
#' for that matter. To get more insights in the outlier detection results before removing them 
#' use \code{getOutlierResults()}.
#' 
#' Method: Interquartile Range (iqr)
#' 
#' Every stat variable denoted in argument \code{variable_names} is screened via 
#' R's built in function \code{grDevices::boxplot.stats()}. Every ID that is 
#' recognized as an outlier is stored in a list slot named after the stat variable
#' under which it was detected as an outlier. 
#'
#' Method: mahalanobis
#' 
#' Multivariate outlier detection method. Mahalanobis distance is computed including 
#' all stat variables denoted in argument \code{variable_names} via \code{stats::mahalanobis()}.
#' P-values are calculated via \code{stats::pchisq()} and cells are filtered according 
#' to the threshold set with argument \code{threshold_pval}. Cells with a mahalanobis distance
#' p-value lower or equal than the threshold are defined as outliers. 
#' 
#'
#' @inherit updated_object return
#' 
#' @seealso removeOutliers()
#' 
#' @export

detectOutliers <- function(object,
                           method_outlier = "iqr",
                           threshold_pval = 0.001,
                           variable_names = NULL,
                           across = NULL, 
                           phase = NULL,
                           verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phase = 1)
  
  if("iqr" %in% method_outlier){
    
    object <- 
      detect_outliers_iqr(
        object = object,
        variable_names = variable_names,
        verbose = verbose, 
        phase = phase,
        across = across
        )
    
  }
  
  if("mahalanobis" %in% method_outlier){
    
    object <- 
      detect_outliers_mahalanobis(
        object = object,
        variable_names = variable_names,
        verbose = verbose,
        threshold_pval = threshold_pval, 
        phase = phase, 
        across = across
        )
    
  }
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}


#' @title Remove outliers from object
#' 
#' @description Removes the data from the cell ids detected by \code{detectOutliers()} from 
#' the object. As this changes the data set all clustering-, correlation- and dimensional 
#' reduction results are reset. 
#'
#' @inherit argument_dummy params 
#' @inherit updated_object return
#' 
#' @details Cell are removed via a reversed application of \code{subsetByCellId()}.
#' You can therefore keep track of conducted outlier removal by using function \code{printSubsetHistory()}
#' which prints summary text on how, when and why you subsetted the cypro object. 
#' 
#' @seealso detectOutliers()
#' @export

removeOutliers <- function(object, method_outlier, new_name = "outlier_removed", phase = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  confuns::is_value(x = method_outlier, mode = "character")
  
  if(multiplePhases(object)){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
  } else {
    
    phase <- NULL
    
  }
  
  outlier_results <-
    getOutlierResults(object, method_outlier = method_outlier, phase = phase, verbose = FALSE)
  
  outlier_ids <-
    getOutlierIds(object, method_outlier = method_outlier, phase = phase, check = TRUE)
  
  cell_ids <- getStatsDf(object, drop_na = FALSE) %>% dplyr::pull(cell_id)
  
  keep_ids <- cell_ids[!cell_ids %in% outlier_ids]
  
  n_outliers <- base::length(outlier_ids)
  
  if(n_outliers == 0){
    
    confuns::give_feedback(msg = "No outliers were detected by specified method. Returning original input object.")
    
  } else {
    
    msg <- 
      glue::glue(
        "Removing {n_outliers} {ref_outliers}.",
        ref_outliers = confuns::adapt_reference(outlier_ids, "outlier", "outliers")
        )
    
    confuns::give_feedback(msg = msg, verbose = verbose)
    
    ref_across <- 
      base::ifelse(
        test = base::is.null(outlier_results$across),
        yes = "None", 
        no = outlier_results$across
      )
    
    ref_variables <- 
      outlier_results$variable_names %>% 
      confuns::scollapse(width = 50)
    
    if(multiplePhases(object)){
      
      ref_phase <- stringr::str_c("\nBased on ", phase, " phase.") 
      
    } else {
      
      ref_phase <- " "
      
    }
    
    if(method_outlier == "mahalanobis"){
      
      ref_pval <- stringr::str_c("\nThreshold p-value:", outlier_results$threshold_pval) 
      
    } else {
      
      ref_pval <- " "
      
    }
    
    reasoning <- 
      glue::glue(
        "Outlier removal. ",
        ref_phase,
        "\nMethod: '{method_outlier}'" ,
        ref_pval,
        "\nAcross: '{ref_across}' " ,
        "\nVariables: '{ref_variables}'",
        "\nNumber of outliers: {n_outliers}"
        ) %>%
      base::as.character()
    
    
    object <-
      subsetByCellId(
        object = object,
        new_name = new_name,
        cell_ids = keep_ids,
        reasoning = reasoning,
        verbose = verbose, 
        phase = phase
        )
    
    object@analysis <- list()
    object@qcheck$outlier_detection <- NULL
    
    # set information
    if(!base::is.null(object@information$outliers_removed)){
      
      object@information$outliers_removed <-
        c(object@information$outliers_removed, list(outlier_ids = outlier_ids, method = method_outlier, across = ref_across))
      
    } else {
      
      object@information$outliers_removed[[1]] <- list(outlier_ids = outlier_ids, method = method_outlier, across = ref_across)
      
    }
    
    base::names(object@information$outliers_removed) <- 
      base::seq_along(object@information$outliers_removed) %>% english::ordinal()
    
  }
  
  base::return(object)
  
}


#' @title Outlier detection functions
detect_outliers_iqr <- function(object, variable_names = NULL, across = NULL, phase = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object = object, phase = phase, max_phases = 1)

  if(base::is.null(variable_names)){
    
    variable_names <- getStatVariableNames(object)
    
  }
  
  if(base::is.character(across)){
    
    confuns::check_one_of(
      input = across, 
      against = getGroupingVariableNames(object, verbose = FALSE)
    )
    
  }
  
  confuns::give_feedback(msg = "Running outlier detection with method = 'iqr'")
  
  stat_df <-
    getStatsDf(object, phase = phase, with_grouping = TRUE) %>% 
    dplyr::select(cell_id, dplyr::all_of(variable_names), dplyr::any_of(x = across))
  
  numeric_vars <-
    dplyr::select_if(stat_df, .predicate = base::is.numeric) %>% 
    base::colnames()
  
  if(base::is.character(across)){
    
    groups <-
      getGroupNames(object = object, grouping_variable = across, phase = phase)
    
  } else {
    
    groups <- "All"
    
  }
  
  # if across is not specified function is applied only one time
  # and stat_df is not filtered
  
  outlier_list <- 
    purrr::map(.x = groups, .f = function(group){
      
      if(base::is.character(across)){
        
        stat_df <- dplyr::filter(stat_df, !!rlang::sym(across) == {{group}})
        
      }
      
      all_outlier_ids <- 
        purrr::map(.x = numeric_vars, .f = function(num_var){
          
          variable <- base::as.numeric(stat_df[[num_var]])
          
          iqr_res <- grDevices::boxplot.stats(variable)
          
          outlier_values <- iqr_res$out
          
          outlier_positions <- base::which(variable %in% outlier_values)
          
          outlier_ids <- dplyr::pull(stat_df[outlier_positions,], var = "cell_id")
          
          base::return(outlier_ids)
          
        }) %>%
        purrr::set_names(nm = numeric_vars)
      
      return(all_outlier_ids)
      
    }) %>% 
    purrr::set_names(nm = groups)
  
  outlier_results <- list()
  outlier_results$ids <- outlier_list
  outlier_results$across <- across
  outlier_results$variable_names <- variable_names
  
  if(!multiplePhases(object)){
    
    object@qcheck$outlier_detection$iqr <- outlier_results
    
  } else {
    
    object@qcheck$outlier_detection$iqr[[phase]] <- outlier_results
    
  }
  
  outlier_ids <- getOutlierIds(object, phase = phase, method_outlier = "iqr")
  
  msg <- 
    glue::glue(
      "Found {n} {ref_outliers}.",
      n = base::length(outlier_ids),
      ref_outliers = confuns::adapt_reference(outlier_ids, "outlier", "outliers")
      )
  
  confuns::give_feedback(msg = msg, verbose = verbose)
  
  base::return(object)
  
}


#' @rdname detect_outliers_iqr
detect_outliers_mahalanobis <- function(object,
                                        variable_names = NULL,
                                        threshold_pval = 0.001,
                                        verbose = NULL,
                                        across = NULL, 
                                        phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object = object, phase = phase, max_phases = 1)
  
  if(base::is.null(variable_names)){
    
    variable_names <- getStatVariableNames(object)
    
  }
  
  if(base::is.character(across)){
    
    confuns::check_one_of(
      input = across, 
      against = getGroupingVariableNames(object, phase = phase, verbose = FALSE)
    )
    
    groups <- 
      getGroupNames(object, grouping_variable = across, phase = phase)
    
    stat_df <- 
      getStatsDf(object, phase = phase, with_grouping = TRUE, verbose = FALSE) %>% 
      dplyr::select(cell_id, !!rlang::sym(across), dplyr::all_of(variable_names))
    
  } else {
    
    groups <- "All"
    
    stat_df <- 
      getStatsDf(object, phase = phase, with_grouping = FALSE, verbose = FALSE) %>% 
      dplyr::select(cell_id, dplyr::all_of(variable_names))
    
  }
  
  outlier_list <- 
    # iterating over phase (in non time lapse experiments phase is ignored anyway)
    purrr::map(.x = groups, .f = function(group){
      
      if(base::is.character(across)){
        
        stat_df <- 
          dplyr::filter(stat_df, !!rlang::sym(across) == {{group}}) %>% 
          dplyr::select(-!!rlang::sym(across))
        
      }
      
      stat_df <- tibble::column_to_rownames(stat_df, var = "cell_id")
      
      stat_df$mahal <- 
        stats::mahalanobis(
          x = stat_df,
          center = base::colMeans(stat_df),
          cov = stats::cov(stat_df)
        )
      
      stat_df$pval <- 
        stats::pchisq(
          q = stat_df$mahal,
          df = base::length(variable_names)-1,
          lower.tail = FALSE
        )
      
      outlier_ids <- 
        tibble::rownames_to_column(stat_df, var = "cell_id") %>% 
        dplyr::filter(pval <= {{threshold_pval}}) %>% 
        dplyr::pull(cell_id)
      
      base::return(outlier_ids)
      
    }) %>% 
    purrr::set_names(groups)
  
  outlier_results <- list()
  outlier_results$across <- across
  outlier_results$ids <- outlier_list
  outlier_results$threshold_pval <- threshold_pval
  outlier_results$variable_names <- variable_names
  
  if(!multiplePhases(object)){
    
    object@qcheck$outlier_detection$mahalanobis <- outlier_results
    
  } else {
    
    object@qcheck$outlier_detection$mahalanobis[[phase]] <- outlier_results
    
  }
  
  ids <- getOutlierIds(object, method_outlier = "mahalanobis", phase = phase)
  
  msg <- 
    glue::glue(
      "Found {n} {ref_outliers}.",
      n = base::length(ids), 
      ref_outliers = confuns::adapt_reference(ids, "outlier", "outliers")
    )
  
  confuns::give_feedback(msg = msg, verbose = verbose)
  
  base::return(object)
  
}




