

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
                           verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  if("iqr" %in% method_outlier){
    
    object <- detect_outliers_iqr(object, variable_names = variable_names, verbose = verbose)
    
  }
  
  if("mahalanobis" %in% method_outlier){
    
    object <- detect_outliers_mahalanobis(object, variable_names = variable_names, 
                                          verbose = verbose, threshold_pval = threshold_pval)
    
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

removeOutliers <- function(object, method_outlier = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  outlier_methods <-
    getOutlierResults(object, method_outlier = method_outlier) %>% 
    base::names()
  
  outlier_ids <- getOutlierIds(object, method_outlier = method_outlier, check = TRUE)
  
  cell_ids <- getStatsDf(object) %>% dplyr::pull(cell_id)
  
  keep_ids <- cell_ids[!cell_ids %in% outlier_ids]
  
  n_outliers <- base::length(outlier_ids)
  
  if(n_outliers == 0){
    
    confuns::give_feedback(msg = "No outliers were detected by all specified methods. Returning original input object.")
    
  } else {
    
    msg <- glue::glue("Removing {n_outliers} {ref_outliers}.", 
                      ref_outliers = confuns::adapt_reference(outlier_ids, "outlier", "outliers"))
    
    confuns::give_feedback(msg = msg, verbose = verbose)
    
    object <-
      subsetByCellId(
        object = object,
        cell_ids = keep_ids,
        reasoning = glue::glue("Outlier removal. Methods: '{scollapse(method_outlier)}'"),
        verbose = verbose)
    
    object@analysis <- list()
    
    # set information
    if(!base::is.null(object@information$outliers_removed)){
      
      object@information$outliers_removed <-
        c(object@information$outliers_removed, list(outlier_ids = outlier_ids, methods = outlier_methods))
      
    } else {
      
      object@information$outliers_removed <- list(outlier_ids = outlier_ids, methods = outlier_methods)
      
    }
    
    base::names(object@information$outliers_removed) <- 
      base::seq_along(object@information$outliers_removed) %>% english::ordinal()
    
  }
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}


#' @title Outlier detection functions
detect_outliers_iqr <- function(object, variable_names = NULL, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(base::is.null(variable_names)){
    
    variable_names <- getStatVariableNames(object)
    
  }
  
  confuns::give_feedback(msg = "Running outlier detection with method = 'iqr'")
  
  outlier_results <- 
    purrr::map(.x = getPhases(object), # iterate over phases: in case of non time lapse experiment phase is ignored anyway
               .f = function(phase){
                 
                 stat_df <-
                   getStatsDf(object, phase = phase, with_grouping = FALSE) %>% 
                   dplyr::select(cell_id, dplyr::all_of(variable_names))
                 
                 numeric_vars <-
                   dplyr::select_if(stat_df, .predicate = base::is.numeric) %>% 
                   base::colnames()
                 
                 outlier_list <- 
                   purrr::map(.x = numeric_vars, .f = function(num_var){
                     
                     variable <- base::as.numeric(stat_df[[num_var]])
                     
                     iqr_res <- grDevices::boxplot.stats(variable)
                     
                     outlier_values <- iqr_res$out
                     
                     outlier_positions <- base::which(variable %in% outlier_values)
                     
                     outlier_ids <- dplyr::pull(stat_df[outlier_positions,], var = "cell_id")
                     
                     base::return(outlier_ids)
                     
                   }) %>% 
                   purrr::set_names(nm = numeric_vars)
                 
               }) %>% 
    purrr::set_names(getPhases(object))
  
  if(!multiplePhases(object)){
    
    outlier_results <- outlier_results[[1]]
    
  }
  
  object@qcheck$outlier_detection$iqr <- outlier_results
  
  msg <- glue::glue("Found {n} {ref_outliers}.",
                    n = base::length(getOutlierIds(object, method_outlier = "iqr")), 
                    ref_outliers = confuns::adapt_reference(getOutlierIds(object), "outlier", "outliers"))
  
  confuns::give_feedback(msg = msg, verbose = verbose)
  
  base::return(object)
  
}


#' @rdname detect_outliers_iqr
detect_outliers_mahalanobis <- function(object, variable_names = NULL, threshold_pval = 0.001, verbose = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(base::is.null(variable_names)){
    
    variable_names <- getStatVariableNames(object)
    
  }
  
  outlier_results <- 
    # iterating over phase (in non time lapse experiments phase is ignored anyway)
    purrr::map(.x = getPhases(object), .f = function(phase){
                 
                 stat_df <-
                   getStatsDf(object, phase = phase, with_grouping = FALSE) %>% 
                   dplyr::select(cell_id, dplyr::all_of(variable_names)) %>% 
                   tibble::column_to_rownames(var = "cell_id")
                 
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
    purrr::set_names(getPhases(object))
  
  if(!multiplePhases(object)){
    
    outlier_results <- outlier_results[[1]]
    
  }
  
  object@qcheck$outlier_detection$mahalanobis <- 
    list(
      outlier_ids = outlier_results, 
      threshold_pval = threshold_pval, 
      variable_names = variable_names
    )
  
  msg <- 
    glue::glue(
      "Found {n} {ref_outliers}.",
      n = base::length(getOutlierIds(object, method_outlier = "mahalanobis")), 
      ref_outliers = confuns::adapt_reference(getOutlierIds(object), "outlier", "outliers")
    )
  
  confuns::give_feedback(msg = msg, verbose = verbose)
  
  base::return(object)
  
}




