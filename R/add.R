
#' @include imported-generics.R
NULL

# Documentations dummies --------------------------------------------------

#' dummy 
#' @return An updated cypro object that contains the data added.
add_family <- function(){}







# a -----------------------------------------------------------------------



# c -----------------------------------------------------------------------

add_classes <- function(x, classes){
  
  old_classes <- base::class(x)
  
  all_classes <- c(classes, old_classes) %>% base::unique()
  
  base::class(x) <- all_classes 
  
  return(x)
  
}


# f -----------------------------------------------------------------------

add_fset_suffix <- function(cluster_df, fset){
  
  tibble::column_to_rownames(cluster_df, var = "cell_id") %>% 
    dplyr::rename_with(.fn = ~ stringr::str_c(.x, "_(", fset, ")", sep = "")) %>% 
    tibble::rownames_to_column(var = "cell_id")
  
}

# h -----------------------------------------------------------------------

#' @title Add helping text 
#'
add_helper <- function(shiny_tag, content, title = "What do I have to do here?", type = "inline", size = "s", ...){
  
  
  res <- 
    shinyhelper::helper(shiny_tag = shiny_tag, 
                        content = content, 
                        title = title, 
                        size = size,
                        type = type, 
                        ...)
  
  base::return(res)
  
}


# r -----------------------------------------------------------------------




# A -----------------------------------------------------------------------


#' @title Add analysis aspects
#' 
#' @description Adds new analysis aspects for a new feature set. 
#' 
#' @inherit argument_dummy params
#' 
#' @return The input object.
setGeneric(name = "addAnalysisAspects", def = function(object, ...){
  
  standardGeneric(f = "addAnalysisAspects")
  
})

#' @rdname addAnalysisAspects
#' @export
setMethod(
  f = "addAnalysisAspects", 
  signature = "Cypro", 
  definition = function(object, name, variables_numeric){
    
    object@analysis$clustering[[name]] <- 
      methods::new(Class = "Clustering", variables_numeric = variables_numeric, key_name = "cell_id")
    
    object@analysis$correlation[[name]] <- 
      methods::new(Class = "Correlation", variables_numeric = variables_numeric, key_name = "cell_id")
    
    object@analysis$dimred[[name]] <- 
      methods::new(Class = "DimRed", variables_numeric = variables_numeric, key_name = "cell_id")
    
    return(object)
    
  }
)

#' @rdname addAnalysisAspects
#' @export
setMethod(
  f = "addAnalysisAspects", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, name, variables_numeric){
    
    for(p in getPhaseNames(object)){
      
      object@analysis$clustering[[name]][[p]] <- 
        methods::new(Class = "Clustering", variables_numeric = variables_numeric, key_name = "cell_id")
      
      object@analysis$correlation[[name]][[p]] <- 
        methods::new(Class = "Correlation", variables_numeric = variables_numeric, key_name = "cell_id")
      
      object@analysis$dimred[[name]][[p]] <- 
        methods::new(Class = "DimRed", variables_numeric = variables_numeric, key_name = "cell_id")
      
    }
    
    return(object)
    
  }
)


# C -----------------------------------------------------------------------

#' @title Add cluster variables
#' 
#' @description Add cluster variables to cluster data.frame of the \code{Cypro}
#' object.
#' 
#' @param input_df Data.frame that contains the cluster variables that are supposed
#' to be added.
#' @param variable_names Character vector. The names of the variables that are 
#' supposed to be added.
#' @param key Character value. The key variable by which the variables are added.
#' Defaults to \code{cell_id}.
#' @inherit argument_dummy params

setGeneric(name = "addClusterVariables", def = function(object, ...){
  
  standardGeneric(f = "addClusterVariables")
  
})

#' @rdname addClusterVariables
#' @export
setMethod(
  f = "addClusterVariables", 
  signature = "Cypro", 
  definition = function(object, 
                        input_df, 
                        variable_names, 
                        key = "cell_id", 
                        force = FALSE,
                        verbose = TRUE){
    
    check_none_of(
      input = variable_names, 
      against = getClusterVariableNames(object), 
      ref.against = "cluster variables names", 
      force = force
    )
      
    check_data_frame(
      df = input_df, 
      var.class = purrr::map(variable_names, .f = function(x){return("factor")}) %>% purrr::set_names(nm = variable_names)
    )
    
    object@cdata@cluster <-
      dplyr::left_join(
        x = getClusterDf(object, verbose = FALSE) %>% dplyr::select(-any_of(variable_names)), 
        y = input_df[,c(key, variable_names)], 
        by = key
      )
    
    give_feedback(
      msg = glue::glue("Added all specified cluster variables."), 
      verbose = verbose
    )
    
    return(object)
    
  }
)

#' @rdname addClusterVariables
#' @export
setMethod(
  f = "addClusterVariables", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, 
                        phase,
                        input_df, 
                        variable_names, 
                        key = "cell_id", 
                        verbose = TRUE){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    check_none_of(
      input = variable_names, 
      against = getClusterVariableNames(object, phase = phase), 
      ref.against = "cluster variables names", 
      force = force
    )
    
    check_data_frame(
      df = input_df, 
      var.class = purrr::map(variable_names, .f = "factor") %>% purrr::set_names(nm = variable_names)
    )
    
    object@cdata$cluster[[phase]] <-
      dplyr::left_join(
        x = getClusterDf(object, phase = phase) %>% dplyr::select(-any_of(variable_names)), 
        y = input_df[,c(key, variable_names)], 
        by = key
      )
    
    give_feedback(
      msg = glue::glue("Added all specified cluster variables."), 
      verbose = verbose
    )
    
    return(object)
    
  }
)



#' @rdname addClusterVarsHclust
#' @export
setMethod(
  f = "addClusterVarsHclust", 
  signature = "Cypro", 
  definition = function(object, 
                        fset = "all_features", 
                        ks = NULL, 
                        hs = NULL, 
                        methods_dist = "euclidean", 
                        method_aggl = "ward.D", 
                        prefix = "", 
                        naming_k = "{method_dist}_{method_aggl}_k{k}", 
                        naming_h = "{method_dist}_{method_aggl}_h{h}",
                        verbose = TRUE){
    
    cluster_df <- 
      getClusterVarsHclust(
        object = object, 
        fset = fset, 
        ks = ks, 
        hs = hs, 
        methods_dist = methods_dist, 
        methods_aggl = methods_aggl, 
        prefix = prefix, 
        naming_k = naming_k, 
        naming_h = naming_h
      ) %>% 
      add_fset_suffix(fset = fset)
    
    cluster_names <- 
      dplyr::select(cluster_df, -cell_id) %>% 
      base::colnames()
    
    object <- 
      addClusterVariables(
        object = object, 
        input_df = cluster_df, 
        variable_names = cluster_names, 
        verbose = verbose
      )
    
    return(object)
    
  }
)

#' @rdname addClusterVarsHclust
#' @export
setMethod(
  f = "addClusterVarsHclust", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, 
                        phase,
                        fset = "all_features", 
                        ks = NULL, 
                        hs = NULL, 
                        methods_dist = "euclidean", 
                        method_aggl = "ward.D", 
                        prefix = "", 
                        naming_k = "{method_dist}_{method_aggl}_k{k}", 
                        naming_h = "{method_dist}_{method_aggl}_h{h}",
                        verbose = TRUE){
    
    cluster_df <- 
      getClusterVarsHclust(
        object = object, 
        phase = phase,
        fset = fset, 
        ks = ks, 
        hs = hs, 
        methods_dist = methods_dist, 
        methods_aggl = methods_aggl, 
        prefix = prefix, 
        naming_k = naming_k, 
        naming_h = naming_h
      ) %>% 
      add_fset_suffix(fset = fset)
    
    cluster_names <- 
      dplyr::select(cluster_df, -cell_id) %>% 
      base::colnames()
    
    object <- 
      addClusterVariables(
        object = object, 
        phase = phase,
        input_df = cluster_df, 
        variable_names = cluster_names, 
        verbose = verbose
      )
    
    return(object)
    
  }
)

#' @rdname addClusterVarsKmeans
#' @export
setMethod(
  f = "addClusterVarsKmeans",
  signature = "Cypro",
  definition = function(object, 
                        ks, 
                        fset = "all_features",
                        methods_kmeans = "Hartigan-Wong",
                        prefix = "",
                        naming = "{method_kmeans}_k{k}", 
                        force = FALSE, 
                        verbose = TRUE){
    
    cluster_df <- 
      getClusterVarsKmeans(
        object = object, 
        fset = fset,
        ks = ks, 
        methods_kmeans = methods_kmeans, 
        prefix = prefix, 
        naming = naming
      ) %>% 
      add_fset_suffix(fset = fset)
    
    cluster_names <- 
      dplyr::select(cluster_df, -cell_id) %>% 
      base::colnames()
    
    object <- 
      addClusterVariables(
        object = object, 
        input_df = cluster_df, 
        variable_names = cluster_names, 
        force = force,
        verbose = verbose
      )
    
    return(object)
    
  }
)

#' @rdname addClusterVarsKmeans
#' @export
setMethod(
  f = "addClusterVarsKmeans",
  signature = "CyproTimeLapseMP",
  definition = function(object,
                        phase,
                        ks, 
                        fset = "all_features",
                        methods_kmeans = "Hartigan-Wong",
                        prefix = "",
                        naming = "{method_kmeans}_k{k}", 
                        force = FALSE, 
                        verbose = TRUE){
    
    cluster_df <- 
      getClusterVarsKmeans(
        object = object,
        phase = phase,
        fset = fset,
        ks = ks, 
        methods_kmeans = methods_kmeans, 
        prefix = prefix, 
        naming = naming
      ) %>% 
      add_fset_suffix(fset = fset)
    
    cluster_names <- 
      dplyr::select(cluster_df, -cell_id) %>% 
      base::colnames()
    
    object <- 
      addClusterVariables(
        object = object, 
        phase = phase,
        input_df = cluster_df, 
        variable_names = cluster_names, 
        force = force,
        verbose = verbose
      )
    
    return(object)
    
  }
)


#' @rdname addClusterVarsPam
#' @export
setMethod(
  f = "addClusterVarsPam",
  signature = "Cypro",
  definition = function(object, 
                        ks, 
                        fset = "all_features",
                        methods_pam = "euclidean",
                        prefix = "",
                        naming = "{method_pam}_k{k}", 
                        force = FALSE, 
                        verbose = TRUE){
    
    cluster_df <- 
      getClusterVarsPam(
        object = object, 
        fset = fset, 
        ks = ks, 
        methods_pam = methods_pam,
        prefix = prefix, 
        naming = naming
      ) %>% 
      add_fset_suffix(fset = fset)
    
    cluster_names <- 
      dplyr::select(cluster_df, -cell_id) %>% 
      base::colnames()
    
    object <- 
      addClusterVariables(
        object = object, 
        input_df = cluster_df, 
        variable_names = cluster_names, 
        force = force, 
        verbose = verbose
      )
    
    return(object)
    
  }
)

#' @rdname addClusterVarsPam
#' @export
setMethod(
  f = "addClusterVarsPam",
  signature = "CyproTimeLapseMP",
  definition = function(object, 
                        phase,
                        ks, 
                        fset = "all_features",
                        methods_pam = "euclidean",
                        prefix = "",
                        naming = "{method_pam}_k{k}", 
                        force = FALSE, 
                        verbose = TRUE){
    
    cluster_df <- 
      getClusterVarsPam(
        object = object, 
        phase = phase,
        fset = fset, 
        ks = ks, 
        methods_pam = methods_pam,
        prefix = prefix, 
        naming = naming
      ) %>% 
      add_fset_suffix(fset = fset)
    
    cluster_names <- 
      dplyr::select(cluster_df, -cell_id) %>% 
      base::colnames()
    
    object <- 
      addClusterVariables(
        object = object, 
        phase = phase,
        input_df = cluster_df, 
        variable_names = cluster_names, 
        force = force, 
        verbose = verbose
      )
    
    return(object)
    
  }
)

# F -----------------------------------------------------------------------

#' @title Add feature set
#' 
#' @description With this function defined sets of cell features are set. See
#' details for more. 
#' 
#' @inherit argument_dummy params
#' 
#' @details The features specified of argument \code{features} or \code{stat_features}
#' are stored as a new feature set. Based on that clustering, correlation, etc. 
#' can be conducted. 
#' 
#' @section Concept:
#' The concept of \emph{feature sets} is used in \code{cypro} to facilitate
#' the analysis of data sets that contain an abundance of features as is often obtained
#' by high content screening. It allows to conveniently analyze the data from several points of
#' view as machine learning results (clustering, correlation, dimensional reduction, etc.)
#' are stored by feature set. E.g. you can cluster cells according to feature set \emph{area_shape}
#' that you might have defined as a set of features that only measure shape related properties 
#' of the cell and do the same for a feature set \emph{migration}. 
#'
#' @seealso \code{getFeatureSet()}, \code{getFeautureSets()}, \code{getFeatureSetNames()} 

setGeneric(name = "addFeatureSet", def = function(object, ...){
  
  standardGeneric(f = "addFeatureSet")
  
})

#' @rdname addFeatureSet
#' @export
setMethod(
  f = "addFeatureSet", 
  signature = "CyproScreening", 
  definition = function(object, name, features, force = FALSE, verbose = TRUE){
    
    is_value(x = name, mode = "character")
    
    is_vec(x = features, mode = "character", min.length = 2)
    
    check_none_of(
      input = name, 
      against = getFeatureSetNames(object), 
      force = force, 
      ref.against = "defined feature sets"
    )
    
    check_one_of(
      input = features, 
      against = getFeatureNames(object)
    )
    
    object@feature_sets[[name]] <- features
    
    object <- addAnalysisAspects(object, name = name, variables_numeric = features)
    
    n_features <- base::length(features)
    
    give_feedback(
      msg = glue::glue("Setting new feature set named '{name}' with {n_features} features."), 
      verbose = verbose
    )
    
    return(object)
    
  }
)

#' @rdname addFeatureSet
#' @export
setMethod(
  f = "addFeatureSet", 
  signature = "CyproTimeLapse", 
  definition = function(object, name, stat_features, force = FALSE, verbose = TRUE){
    
    is_value(x = name, mode = "character")
    
    is_vec(x = stat_features, mode = "character", min.length = 2)
    
    check_none_of(
      input = name, 
      against = getFeatureSetNames(object), 
      force = force
    )
    
    check_one_of(
      input = stat_features, 
      against = getFeatureNames(object)
    )
    
    object@feature_sets[[name]] <- stat_features
    
    object <- addAnalysisAspects(object, name = name, variables_numeric = features)
    
    n_features <- base::length(stat_features)
    
    give_feedback(
      msg = glue::glue("Setting new feature set named '{name}' with {n_features} features."), 
      verbose = verbose
    )
    
    return(object)
    
  }
)

#' @rdname addFeatureSet
#' @export
setMethod(
  f = "addFeatureSet", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, name, stat_features, force = FALSE, verbose = TRUE){
    
    is_value(x = name, mode = "character")
    
    is_vec(x = stat_features, mode = "character", min.length = 2)
    
    check_none_of(
      input = name, 
      against = getFeatureSetNames(object), 
      force = force
    )
    
    check_one_of(
      input = stat_features, 
      against = getStatVariableNames(object)
    )
    
    object@feature_sets[[name]] <- stat_features
    
    object <- addAnalysisAspects(object, name = name, variables_numeric = features)
    
    n_features <- base::length(stat_features)
    
    give_feedback(
      msg = glue::glue("Setting new feature set named '{name}' with {n_featurs} featueres."), 
      verbose = verbose
    )
    
    return(object)
    
  }
)


# M -----------------------------------------------------------------------

#' @title Add meta variables
#' 
#' @description Add meta variables to meta data.frame of the \code{Cypro}
#' object.
#' 
#' @param input_df Data.frame that contains the meta variables that are supposed
#' to be added.
#' @param variable_names Character vector. The names of the variables that are 
#' supposed to be added.
#' @param key Character value. The key variable by which the variables are added.
#' Defaults to \code{cell_id}.
#' @inherit argument_dummy params

setGeneric(name = "addMetaVariables", def = function(object, ...){
  
  standardGeneric(f = "addMetaVariables")
  
})

#' @rdname addMetaVariables
#' @export
setMethod(
  f = "addMetaVariables", 
  signature = "Cypro", 
  definition = function(object, 
                        input_df, 
                        variable_names, 
                        key = "cell_id", 
                        verbose = TRUE){
    
    check_none_of(
      input = variable_names, 
      against = getMetaVariableNames(object), 
      ref.against = "meta variables names", 
      force = force
    )
    
    check_data_frame(
      df = input_df, 
      var.class = purrr::map(variable_names, .f = "factor") %>% purrr::set_names(nm = variable_names)
    )
    
    object@cdata$meta <-
      dplyr::left_join(
        x = getMetaDf(object) %>% dplyr::select(-any_of(variable_names)), 
        y = input_df[,c(key, variable_names)], 
        by = key
      )
    
    give_feedback(
      msg = glue::glue("Added all specified meta variables."), 
      verbose = verbose
    )
    
    return(object)
    
  }
)

#' @rdname addClusterVariables
#' @export
setMethod(
  f = "addClusterVariables", 
  signature = "CyproTimeLapseMP", 
  definition = function(object, 
                        phase,
                        input_df, 
                        variable_names, 
                        key = "cell_id", 
                        verbose = TRUE){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    check_none_of(
      input = variable_names, 
      against = getMetaVariableNames(object, phase = phase), 
      ref.against = "meta variables names", 
      force = force
    )
    
    check_data_frame(
      df = input_df, 
      var.class = purrr::map(variable_names, .f = "factor") %>% purrr::set_names(nm = variable_names)
    )
    
    object@cdata$meta[[phase]] <-
      dplyr::left_join(
        x = getMetaDf(object, phase = phase) %>% dplyr::select(-any_of(variable_names)), 
        y = input_df[,c(key, variable_names)], 
        by = key
      )
    
    give_feedback(
      msg = glue::glue("Added all specified meta variables."), 
      verbose = verbose
    )
    
    return(object)
    
  }
)

# R -----------------------------------------------------------------------

#' @title Add variabels to layout nesting
#' 
#' @description Adds variable names to the attributes with with
#' \code{nestLayoutDf()} / \code{unnestLayoutDf()} shift between 
#' the layout data.frame scops. 
#'
#' @param df A layout data.frame.
#' @param vars Character vector. Names of variables. 
#' 
#' @details A call to \code{base::unique()} silently prevents 
#' duplicated values.
#'
#' @return Input data.frame with adjusted attributes.
#' @export
#'
addRoiInfoVarNames <- function(df, vars){
  
  UseMethod(generic = "addRoiInfoVarNames", object = df)
  
}

#' @rdname addRoiInfoVarNames
#' @export

addRoiInfoVarNames.layout_df <- function(df, vars){
  
  attr <- base::attributes(df)
  
  attr$roi_info_vars <- 
    base::unique(c(attr$roi_info_vars, vars))
  
  base::attributes(df) <- attr
  
  return(df)
  
}


# S -----------------------------------------------------------------------


#' @title Add subset information 
#' 
#' @description Adds a new object of class \code{CyproSubset} to @@slot 
#' subset.
#' 
#' @inherit argument_dummy params
#' @param subset_object An object of class \code{CyproSubset}.
#' 
#' @inherit add_family return

setGeneric(name = "addSubset", def = function(object, subset_object){
  
  standardGeneric(f = "addSubset")
  
})

#' @rdname addSubset
#' @export
setMethod(f = "addSubset", signature = "Cypro", definition = function(object, subset_object){
  
  base::stopifnot(isOfClass(subset_object, valid_class = "CyproSubset"))
  
  subset_slot <- english::ordinal(nSubsets(object) + 1)
  
  object@subsets[[subset_slot]] <- subset_object
  
  return(object)
  
})

# W -----------------------------------------------------------------------

#' @title Add a well plate 
#' 
#' @description Creates and adds a new object of class \code{WellPlate} in the respective
#' slot of input for argument \code{object}.
#'
#' @inherit argument_dummy params
#' @param name,layout Input for the \code{WellPlate} object as described in documentation of class \code{WellPlate}.
#' See details for more information. 
#' 
#' @param ... Additional arguments given to \code{methods::new(Class = \emph{'WellPlate'}, ...)}.
#' 
#' @details Slot @@index of the new well plate is automatically defined by adding 1 to the sum of all well plates found.
#' Slot @@experiment of the new well plate is inherited by the input for argument \code{object}. Slot @@type
#' is defined by the attribute \emph{well_plate_type} of the input for argument \code{layout} (which has to be a \code{layout_df}).
#' 
#' Input for slot @@name must not be already used by another well plate.
#' @return The input object.
#' 
#' @seealso \code{WellPlate-class}
#' 
#' @export
#'
setGeneric(name = "addWellPlate", def = function(object, ...){
  
  standardGeneric(f = "addWellPlate")
  
})


#' @rdname addWellPlate
#' @export
setMethod(f = "addWellPlate", signature = "ExperimentDesign", function(object, name, layout, ... ){
  
  n_wps <- nWellPlates(object)
  
  if(n_wps != 0){
    
    confuns::check_none_of(
      input = name, 
      against = getWellPlateNames(object), 
      ref.against = "well plate names"
    )
    
  }
  
  well_plate <- 
    methods::new(
      Class = "WellPlate", 
      experiment = object@experiment, 
      name = name,
      layout = base::as.data.frame(layout),
      index = n_wps + 1,
      type = getWellPlateType(layout),
      ...
    )
  
  object@well_plates[[name]] <- well_plate
  
  return(object)
  
})

