#' @include S4-classes.R
NULL

#' @include S4-method-skeletons.R
NULL

# G -----------------------------------------------------------------------

#' @title Obtain cell grouping data 
#' 
#' @description Extract a data.frame that contains variables that group 
#' cells.
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
      ) %>% 
      purrr::discard(.p = base::is.numeric)
    
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
      ) %>% 
      purrr::discard(.p = base::is.numeric)
    
    return(df)
    
  }
)

#' @title Obtain group names a grouping variable contains
#' 
#' @description Extracts the names of the groups in which a specific grouping
#' variable groups the cells. Useful to obtain input options for arguments like \code{across_subset}. 
#'
#' @inherit argument_dummy params
#'
#' @return A character vector. 
#' 
#' @export

setGeneric(name = "getGroupNames", def = function(object, ...){
  
  standardGeneric(f = "getGroupNames")
  
})

#' @rdname getGroupNames
#' @export
setMethod(f = "getGroupNames", signature = "Cypro", definition = function(object, grouping_variable, ...){
  
  is_value(grouping_variable, mode = "character")
  
  group_vec <- 
    getGroupingDf(object = object, verbose = FALSE) %>% 
    dplyr::select(-cell_id) %>% 
    dplyr::pull(var = {{grouping_variable}}) 
  
  if(base::is.factor(group_vec)){
    
    group_vec <- base::levels(x = group_vec)
    
  } else if(base::is.character(group_vec)){
    
    group_vec <- base::unique(group_vec)
    
  } else {
    
    msg <- glue::glue("The result of grouping variable '{grouping_variable}' must be a character vector or a factor.")
    
    give_feedback(msg = msg, fdb.fn = "stop")
    
  }
  
  res <- vselect(input = group_vec, ...)
  
  return(res)
  
})

#' @rdname getGroupNames
#' @export
setMethod(
  f = "getGroupNames",
  signature = "CyproTimeLapseMP",
  definition = function(object, grouping_variable, ...){
    
    is_value(grouping_variable, "character")
    
    group_vec <- 
      getGroupingDf(object = object, phase = phase, verbose = FALSE) %>% 
      dplyr::select(-cell_id) %>% 
      dplyr::pull(var = {{grouping_variable}}) 
    
    if(base::is.factor(group_vec)){
      
      group_vec <- base::levels(x = group_vec)
      
    } else if(base::is.character(group_vec)){
      
      group_vec <- base::unique(group_vec)
      
    } else {
      
      msg <- glue::glue("The result of grouping variable '{grouping_variable}' must be a character vector or a factor.")
      
      give_feedback(msg = msg, fdb.fn = "stop")
      
    }
    
    res <- vselect(input = group_vec, ...)
    
    return(res)
    
  })


#' @title Obtain grouping variable names of cell data
#' 
#' @description Extracts the names of variables that group cells. Useful to obtain 
#' valid input options for recurring arguments like \code{across} or \code{grouping_variable}.
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
    
    group_df <- 
      getGroupingDf(object, verbose = verbose) %>% 
      dplyr::select(-cell_id)
    
    res <- 
      get_grouping_variable_names_hlpr(
        group_df = group_df, 
        named = named, 
        verbose = verbose, 
      ) %>% 
      vselect(input = ., ...)
    
    return(res)
    
  }
)


#' @rdname getGroupingVariableNames
#' @export
setMethod(
  f = "getGroupingVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, ..., named = FALSE, phase = NULL, verbose = NULL){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    group_df <- 
      getGroupingDf(object, verbose = verbose, phase = phase) %>% 
      dplyr::select(-cell_id)
    
    res <- 
      get_grouping_variable_names_hllpr(
        group_df = group_df, 
        named = named, 
        verbose = verbose, 
        ...
      ) %>% 
      vselect(input = ., ...)
    
    return(res)
    
  }
)

get_grouping_variable_names_hlpr <- function(group_df, ..., named = FALSE, verbose = TRUE){
  
  all_var_names <- 
    base::colnames(group_df)
  
  if(base::isTRUE(named)){
    
    sources <- base::vector("character", base::length(all_var_names))
    
    cluster_names <-
      getClusterVariableNames(object, phase = phase, verbose = verbose)
    
    meta_names <- getMetaVariableNames(object, phase = phase)
    
    wp_names <- getWellPlateVariableNames(object)
    
    for(i in base::seq_along(all_var_names)){
      
      var <- all_var_names[i]
      
      if(var %in% cluster_names){
        
        sources[i] <- "cluster"
        
      } else if(var %in% meta_names){
        
        sources[i] <- "meta"
        
      } else if(var %in% wp_names){
        
        sources[i] <- "well_plate"
        
      }
      
    }
    
    base::names(all_var_names) <- sources
    
  }
  
  return(all_var_names)
  
}



# I -----------------------------------------------------------------------



#' @title Obtain frame interval
#' 
#' @description Extracts the interval between the frames as the numeric 
#' value.
#'
#' @inherit argument_dummy params
#'
#' @return A numeric value. 
#' @export
#'
setGeneric(name = "getInterval", def = function(object){
  
  standardGeneric(f = "getInterval")
  
})

#' @rdname getInterval
#' @export
setMethod(f = "getInterval", signature = "ExperimentDesignTimeLapse", definition = function(object){
  
  object@interval
  
})

#' @rdname getInterval
#' @export
setMethod(f = "getInterval", signature = "CyproTimeLapse", definition = function(object){
  
  exp_design <- getExperimentDesign(object)
  
  out <- getInterval(exp_design)
  
  return(out)
  
})


#' @rdname getInterval
#' @export
setMethod(f = "getInterval", signature = "CyproTimeLapseMP", definition = function(object){
  
  exp_design <- getExperimentDesign(object)
  
  out <- getInterval(exp_design)
  
  return(out)
  
})

#' @title Obtain frame interval unit
#' 
#' @description Extracts the unit of the interval between the frames as
#' a character value.
#'
#' @inherit argument_dummy params
#'
#' @return A character value. 
#' @export
#'
setGeneric(name = "getIntervalUnit", def = function(object){
  
  standardGeneric(f = "getIntervalUnit")
  
})

#' @rdname getIntervalUnit
#' @export
setMethod(f = "getIntervalUnit", signature = "ExperimentDesignTimeLapse", definition = function(object){
  
  object@interval_unit
  
})

#' @rdname getIntervalUnit
#' @export
setMethod(f = "getIntervalUnit", signature = "CyproTimeLapse", definition = function(object){
  
  ed <- getExperimentDesign(object)
  
  out <- getIntervalUnit(ed)
  
  return(out)
  
})

#' @title Obtain image directories
#' 
#' @description Extracts a data.frame in which the added directories
#' to the original images are set by well-roi. 
#'
#' @inherit argument_dummy params
#'
#' @return A data.frame.
#' @export
#'
setGeneric(name = "getImageDirDf", def = function(object){
  
  standardGeneric(f = "getImageDirDf")
  
})

#' @rdname getImageDirDf
#' @export

setMethod(f = "getImageDirDf", signature = "Cypro", definition = function(object){
  
  object@image_directories %>% 
    tibble::as_tibble()
  
})

# L -----------------------------------------------------------------------


#' @title Obtain layout attributes
#' 
#' @description Extracts the well plate layout specific attributes
#' from the input data.frame.
#' 
#' @param df A data.frame of class \code{layout_df}.
#' 
#' @return A named list. 
#' 
#' @export

getLayoutAttributes<- function(df){
  
  base::attributes(df)[layout_df_attributes] %>% 
    purrr::discard(.p = ~ base::is.null(.x))
  
}


#' @title Obtain well plate layout data.frame
#' 
#' @description Extracts the well plate layout in form of a data.frame.
#' 
#' @inherit argument_dummy params
#'
#' @return A \code{layout_df} - data.frame.
#' @export
#'
setGeneric(name = "getLayoutDf", def = function(object, ...){
  
  standardGeneric(f = "getLayoutDf")
  
})

#' @rdname getLayoutDf
#' @export
setMethod(f = "getLayoutDf", signature = "WellPlate", definition = function(object, ...){
  
  df <- 
    tibble::as_tibble(object@layout) %>% 
    as_layout_df(well_plate_type = object@type)
  
  n_phases <- base::attr(df, which = "n_phases")
  
  if(base::is.numeric(n_phases)){
    
    df <- as_layout_df_mp(df = df, well_plate_type = object@type, n_phase = n_phases)
    
  }
  
  return(df)
  
})

#' @rdname getLayoutDf
#' @export
setMethod(f = "getLayoutDf", signature = "ExperimentDesign", definition = function(object, well_plate = NULL, ...){
  
  layout_df <- 
    getWellPlate(object, well_plate = well_plate) %>% 
    getLayoutDf()
  
  return(layout_df)
  
})

#' @rdname getLayoutDf
#' @export
setMethod(f = "getLayoutDf", signature = "Cypro", definition = function(object, well_plate = NULL, ...){
  
  well_plate <- check_wp_name(object, well_plate)
  
  layout_df <- 
    getExperimentDesign(object) %>% 
    getLayoutDf(well_plate = well_plate)
  
  return(layout_df)
  
})


#' @title Obtain layout variable names
#' 
#' @description Extracts the variable names of the layout data.frame
#' as a character vector.
#' 
#' @inherit argument_dummy params
#' @param named Logical. If TRUE, the returned vector of names is named by 
#' the class of the variable.
#' 
#' @return Character vector.
#' 
#'  @export 
#' 
setGeneric(name = "getLayoutVariableNames", def = function(object, named = TRUE, ...){
  
  standardGeneric(f = "getLayoutVariableNames")
  
})

#' @rdname getLayoutVariableNames
#' @export
setMethod(f = "getLayoutVariableNames", signature = "layout_df", definition = function(object, named = TRUE){
  
  layout_df <- object
  
  vnames <- base::names(layout_df)
  
  if(base::isTRUE(named)){
    
    base::names(vnames) <- 
      purrr::map_chr(.x = layout_df, .f = base::class)
    
  }
  
  return(vnames)
  
}) 

#' @rdname getLayoutVariableNames
#' @export
setMethod(f = "getLayoutVariableNames", signature = "Cypro", definition = function(object, named = TRUE, well_plate = NULL, ...){
  
  layout_df <- getLayoutDf(object, well_plate = well_plate)
  
  out <- getLayoutVariableNames(layout_df, named = named)
  
  return(out)
  
})


#' @title Obtain loading status summary
#' 
#' @description Extracts a data.frame that summarizes the loading status
#' of the data by well plate.
#' 
#' @inherit argument_dummy params 
#' 
#' @return A data.frame. 
#' 
#' @export

setGeneric(name = "getLoadingStatusDf", def = function(object, ...){
  
  standardGeneric(f = "getLoadingStatusDf")
  
})

#' @rdname getLoadingStatusDf
#' @export
setMethod(
  f = "getLoadingStatusDf",
  signature = "Cypro",
  definition = function(object,
                        well_plates = NULL,
                        with_transferred = TRUE,
                        n_letters = 40){
    
    well_plate_list <- getWellPlates(object, well_plates = NULL)
    
    out <- list()
    
    if(byFolder(object)){
      
      out$well_plate <- base::names(well_plate_list)
      
      out$folder <- getWellPlateDirectories(object, well_plates = well_plates)
      
      out$valid_files <- 
        purrr::map_int(.x = well_plate_list, .f = ~ base::length(.x@files)) 
      
      out$expected_files <- 
        purrr::map_int(.x = well_plate_list, .f = nExpectedFiles)
      
      out$ambiguous_files <- 
        purrr::map_int(.x = well_plate_list, .f = nAmbiguousFiles)
      
      out$loaded_files <- 
        purrr::map_int(
          .x = well_plate_list,
          .f = ~ purrr::keep(.x = .x@files, .p = ~ containsData(.x)) %>% base::length()
        )
      
      if(base::isTRUE(with_transferred)){
        
        out$transferred_files <- 
          purrr::map_int(
            .x = well_plate_list, 
            .f = ~ purrr::keep(.x = .x@files, .p = ~ base::isTRUE(.x@transferred)) %>% base::length()
          )
        
      }
      
      df <- 
        base::as.data.frame(out) %>% 
        tibble::remove_rownames() %>% 
        tibble::as_tibble()
      
    } else if(byWellPlate(object)){
      
      df <- 
        purrr::map(.x = well_plate_list, .f = ~ .x@files) %>% 
        purrr::flatten() %>% 
        purrr::map_df(.f = function(data_file){
          
          pattern <- stringr::str_c(".{1,", n_letters, "}$", sep = "")
          
          dir <- 
            purrr::map_chr(
              .x = data_file@directory,
              .f = ~ stringr::str_extract(.x, pattern = pattern)
            ) %>% 
            stringr::str_c("...~", .)
          
          df <- 
            tibble::tibble(
              well_plate = data_file@well_plate, 
              directory = dir,
              loaded = containsData(data_file)
            )
          
          if(base::isTRUE(with_transferred)){
            
            df$transferred = data_file@transferred
            
          }
          
          return(df)
          
        }) %>% 
        tibble::as_tibble()
      
    } else {
      
      df <- data.frame()
      
    }
    
    return(df)
    
  })



# M -----------------------------------------------------------------------


#' @title Obtain merged data.frame
#' 
#' @description Merges @@slot content of all objects of class \code{DataFiles}
#' that contain data and that do not contain errors to a single data.frame. 
#' 
#' @inherit argument_dummy params
#' 
#' @details The returned data.frame contains the complete data set including 
#' id-, numeric-, -meta, -well plate and cluster variables. It is then split 
#' into the slots of \code{Cdata} object via \code{setCdataContent()}.
#' 
#' @return A data.frame of class \code{cypro_df}.
#' 
#' @export

setGeneric(name = "getMergedDf", def = function(object, verbose = TRUE){
  
  standardGeneric(f = "getMergedDf")
  
})

#' @rdname getMergedDf
#' @export
setMethod(f = "getMergedDf", signature = "Cypro", definition = function(object, verbose = TRUE){
  
  well_plate_list <- getWellPlates(object)
  
  data_list <- 
    purrr::map(
      .x = well_plate_list,
      .f = ~ purrr::keep(.x = .x@files, .p = ~ containsData(.x) & !containsErrors(.x))
    ) %>% 
    purrr::keep(.p = ~ !isOfLength(x = .x, l = 0))
  
  well_plates <- base::names(data_list)
  
  data_list <- 
    purrr::flatten(data_list) %>%
    base::unname() 
  
  n_files <- base::length(data_list)
  
  if(n_files == 0){
    
    stop("No data files found that contain data and do not contain error feedbacks.")
    
  } else {
    
    confuns::give_feedback(
      msg = glue::glue("Merging data of {n_files} files."), 
      verbose = verbose
    )
    
  }
  
  if(n_files > 250){
    
    pb <- create_progress_bar(total = n_files)
    
  } else {
    
    pb <- NULL
    
  }
  
  merged_df <- purrr::map_df(.x = data_list, .f = function(data_file){
    
    if(base::isTRUE(verbose) & !base::is.null(pb)){
      
      pb$tick()
      
    }
    
    df <- getDataFileDf(object = data_file)
    
    return(df)
    
  }) %>% 
    dplyr::left_join(x = ., y = getWellPlateIndices(object), by = "well_plate_name") %>% 
    dplyr::mutate(
      cell_id = make_cell_id(
        cell_id = cell_id, 
        well_plate_index = well_plate_index, 
        well = well, 
        roi = roi
      ), 
      well_roi = stringr::str_c(well, roi, sep = "_"),
      indexed_well = stringr::str_c(well, well_roi, sep = "_"),
      indexed_well_roi = stringr::str_c(well_plate_index, well_roi, sep = "_")
    ) %>%
    tibble::as_tibble() 
  
  numeric_vars <- getAdditionalVariableNames(object)$numeric
  
  if(base::length(numeric_vars) >= 1){
    
    merged_df <- 
      dplyr::mutate(
        .data = merged_df,
        dplyr::across(.cols = dplyr::all_of(numeric_vars), .fns = base::as.numeric)
      )  
    
  }
  
  grouping_vars <- getAdditionalVariableNames(object)$grouping
  
  if(base::length(grouping_vars) >= 1){
    
    merged_df <- 
      dplyr::mutate(
        .data = merged_df,
        dplyr::across(.cols = dplyr::all_of(grouping_vars), .fns = base::as.factor)
      )
    
  }
  
  return(merged_df)
  
  
})

#' @title Obtain cell meta data 
#' 
#' @description Extracts cell meta data in form of a data.frame that contains
#' data variables with which cells are grouped. 
#'
#' @inherit argument_dummy params
#' 
#' @return A data.frame.
#' 
#' @seealso \code{getClusterDf()}, \code{getWellPlateDf()}
#' 
#' @export

setGeneric(name = "getMetaDf", def = function(object, ...){
  
  standardGeneric(f = "getMetaDf")
  
})

#' @rdname getMetaDf
#' @export
setMethod(f = "getMetaDf", signature = "Cypro", definition = function(object){
  
  check_object(object)
  
  cdata_object <- getCdata(object)
  
  meta_df <- getMetaDf(cdata_object)
  
  return(meta_df)
  
})

#' @rdname getMetaDf
#' @export
setMethod(f = "getMetaDf", signature = "CyproTimeLapseMP", definition = function(object, phase = NULL){
  
  assign_default(object = object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  cdata_object <- getCdata(object)
  
  df <- getMetaDf(cdata_object, phase = phase)
  
  return(df)
  
})

#' @rdname getMetaDf
#' @export
setMethod(f = "getMetaDf", signature = "Cdata", definition = function(object, ...){
  
  df <- object@meta
  
  return(df)
  
})

#' @rdname getMetaDf
#' @export
setMethod(f = "getMetaDf", signature = "CdataTimeLapseMP", definition = function(object, phase, ...){
  
  confuns::check_one_of(
    input = phase, 
    against = base::names(object@meta)
  )
  
  df <- object@meta[[phase]]
  
  return(df)
  
})


#' @title Extract meta variable names of cell data
#' 
#' @description Obtain the names of variables in the meta data.frame. Useful to obtain 
#' valid input options for recurring arguments like \code{across} or \code{grouping_variable}.
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
  definition = function(object, ..., verbose = NULL){
    
    res <- 
      getMetaDf(object) %>% 
      dplyr::select(-cell_id) %>% 
      dplyr::select(...) %>% 
      base::colnames() 
    
    return(res)
    
  }
)


#' @rdname getMetaVariableNames
#' @export
setMethod(
  f = "getMetaVariableNames",
  signature = "CyproTimeLapseMP", 
  definition = function(object, ..., named = FALSE, phase = NULL, verbose = NULL){
    
    phase <- check_phase(object, phase = phase, max_phases = 1)
    
    res <- 
      getMetaDf(object, verbose = TRUE, phase = phase) %>% 
      dplyr::select(-cell_id) %>% 
      dplyr::select(...) %>% 
      base::colnames() 
    
    return(res)
    
  }
)




#' @title Obtain analysis module
#' 
#' @description Extracts the complete S4-object of class \code{AnalysisModule} from 
#' the \code{Cypro} object with all its content. 
#' 
#' @inherit argument_dummy params
#' @param module_name Character value. The name of the analysis module. 
#' 
#' @return An object of class \code{AnalysisModule}.
#' 
#' @export
#' 
setGeneric(name = "getModule", def = function(object, module_name, ...){
  
  standardGeneric(f = "getModule")
  
})


#' @rdname getModule
#' @export
setMethod(f = "getModule", signature = "Cypro", definition = function(object, module_name, ...){
  
  confuns::check_one_of(
    input = module_name, 
    against = base::names(object@modules), 
    fdb.opt = 2, 
    ref.opt.2 = "names of analysis modules", 
  )
  
  return(object@modules[[module_name]])
  
})





