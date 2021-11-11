


#' @title Create data subset by cell IDs
#' 
#' @description Subset functions allow to conveniently split your data by certain characteristics such 
#' as cell lines, conditions, cluster etc. or for specific cell ids. This might be useful if you want apply some machine learning 
#' algorithms such as clustering and correlation on only a subset of cells. See sections below for more information.
#'
#' @inherit argument_dummy params
#' @param cell_ids Character vector. Denotes the cells to keep unambiguously with their cell IDs.
#' @param new_name Character value. Denotes the name of the output object. If set to NULL the name of the 
#' input object is taken and suffixed with \emph{'_subset'}.
#' @param new_directory Character value. Storage directory for the new object. 
#' @param reasoning Character value. Allows two write a short description of how the cell ids according 
#' to which the object is filtered were selected. This description is included in the output of \code{printSubsetHistory()}.
#' Ignored if set to NULL. 
#' @param prev_subset Object of class \code{CyproSubset} that inherits from \code{CyproSubsetByCellID} or NULL.
#'
#' @details Creating subsets of your data affects analysis results such as clustering,
#' dimensional reduction and correlation which is why slot @@analysis is reset.
#' 
#' Within the data of slot @@cdata, the cluster data.frame is emptied. Additionally,
#' levels of grouping variables that are not used any longer after the subsetting
#' are dropped.
#' 
#' To prevent inadvertent overwriting the default directory is reset as well.
#' Make sure to set a new one via \code{setDefaultDirectory()}. 
#' 
#' The name of the returned \code{Cypro} object (slot @@experiment) is suffixed 
#' with \emph{_Subset\code{<nth_subset>}}.
#' 
#' The mechanism and the information with which you create the subset is stored in the output object.
#' Use \code{printSubsetHistory()} to reconstruct the way from the original object
#' to the current one. 
#' 
#' @section Subset mechanism:
#' 
#' Cells with cell IDs that were not part of the cell IDs in argument \code{cell_ids} are discarded.
#' 
#' @return The input object containing a data subset.
#' @export
#' 
setGeneric(name = "subsetByCellID", def = function(object, ...){
  
  standardGeneric(f = "subsetByCellID")
  
})

#' @rdname subsetByCellID
#' @export
setMethod(
  f = "subsetByCellID",
  signature = "CyproScreening", 
  definition = function(object, 
                        cell_ids, 
                        reasoning = NULL,
                        prev_subset = NULL, 
                        verbose = TRUE, 
                        ...){
    
    cypro_object_is_being_subsetted(
      object = object,
      verbose = verbose,
      by = "cell ID",
      prev_subset = prev_subset
    )
    
    object <- 
      subset_by_cell_id_hlpr(
        object = object, 
        cell_ids = cell_ids, 
        reasoning = reasoning, 
        prev_subset = prev_subset,
        verbose = verbose,
        ...
      )
    
    return(object)
    
  })

#' @rdname subsetByCellID
#' @export
setMethod(
  f = "subsetByCellID",
  signature = "CyproTimeLapse", 
  definition = function(object, 
                        cell_ids, 
                        reasoning = NA_character_,
                        prev_subset = NULL,
                        verbose = TRUE, 
                        ...){
    
    cypro_object_is_being_subsetted(
      object = object,
      verbose = verbose,
      by = "cell ID",
      prev_subset = prev_subset
      )
    
    object <- 
      subset_by_cell_id_hlpr(
        object = object, 
        cell_ids = cell_ids, 
        reasoning = reasoning, 
        prev_subset = prev_subset,
        verbose = verbose,
        ...
      )
    
    return(object)
    
  })

subset_by_cell_id_hlpr <- function(object,
                                   cell_ids, 
                                   reasoning,
                                   prev_subset,
                                   verbose){
  
  cypro_object_is_being_subsetted(
    object = object,
    by = "cell_id",
    prev_subset = prev_subset,
    verbose = verbose
  )
  
  # input validation
  is_vec(x = cell_ids, mode = "character")
  
  cell_ids <- base::unique(cell_ids)
  
  base::stopifnot(base::all(cell_ids %in% getCellIDs(object)))
  
  # extract ids
  all_cell_ids <- getCellIDs(object)
  
  discarded_cell_ids <- all_cell_ids[!all_cell_ids %in% cell_ids]
  
  # filter slot @cdata
  cdata <- getCdata(object)
  
  for(slot in methods::slotNames(cdata)){
    
    df <- methods::slot(object = cdata, name = slot)
    
    filtered_df <- 
      dplyr::filter(df, cell_id %in% {{cell_ids}}) %>% 
      dplyr::mutate(
        dplyr::across(
          .cols = where(base::is.factor), 
          .fns = base::droplevels
        )
      )
    
    # reset cluster data.frame
    if(slot == "cluster"){
      
      filtered_df <- dplyr::select(filtered_df, cell_id)
      
    }
    
    methods::slot(object = cdata, name = slot) <- base::as.data.frame(filtered_df)
    
  }
  
  object@cdata <- cdata
  
  # reset slots 
  object@analysis <- list()
  
  object@quality_checks <- list()
  
  # create new name
  parent_name <- object@experiment
  
  new_name <-
    stringr::str_remove(parent_name, pattern = "Subset\\d$") %>% 
    stringr::str_c(., "_Subset", nSubsets(object) + 1)
  
 # save subset information 
 if(isOfClass(prev_subset, valid_class = "CyproSubset")){
    
    subset_object <- prev_subset
    
    subset_object@ids_discarded <- discarded_cell_ids
    subset_object@ids_remaining <- cell_ids
    
    subset_object@new_name <- new_name
    subset_object@parent_name <- object@experiment
    
    subset_object@reasoning <- reasoning
    
  } else {
    
    subset_object <- 
      CyproSubsetByCellID(
        new_name = new_name, 
        parent_name = parent_name,
        reasoning = reasoning, 
        ids_discarded = discarded_cell_ids,
        ids_remaining = cell_ids
      )
    
  }
  
  object <- addSubset(object, subset_object = subset_object)
  
  # change name and directory
  object <- setExperimentName(object, name = new_name)
  
  object <- discardStorageDirectory(object)
  
  object <- setCellIDs(object, cell_ids = cell_ids)
  
  give_feedback(
    msg = glue::glue("Subsetting done. {nCells(object)} cells remain."), 
    verbose = verbose
  )
  
  return(object)
  
}



#' @title Create data subset by group
#'
#' @inherit subsetByCellID description params details
#'
#' @inherit argument_dummy params
#' @param groups Character vector. The group names of the variable denoted in 
#' \code{grouping_variable} whose cells are supposed to be kept.
#' 
#' @section Subset mechanism:
#' 
#' The cell IDs that belong to the groups denoted in \code{groups} are gathered.
#' The object is then subsetted by the mechanism described in the documentation
#' for function \code{subsetByCellID}.
#' 
#' @return The input object containing a data subset.
#' 
#' @export

setGeneric(name = "subsetByGroup", def = function(object, ...){
  
  standardGeneric(f = "subsetByGroup")
})

#' @rdname subsetByGroup
#' @export
setMethod(
  f = "subsetByGroup",
  signature = "CyproScreening", 
  definition = function(object, 
                        grouping_variable,
                        groups, 
                        reasoning = NA_character_,
                        verbose = TRUE){
    
    object <- 
      subset_by_group_hlpr(
        object = object, 
        grouping_variable = grouping_variable, 
        groups = groups, 
        reasoning = reasoning,
        verbose = verbose
      )
    
    return(object)
    
  })

#' @rdname subsetByGroup
#' @export
setMethod(
  f = "subsetByGroup",
  signature = "CyproTimeLapse", 
  definition = function(object, 
                        grouping_variable,
                        groups, 
                        reasoning = NA_character_,
                        verbose = TRUE){
    
    object <- 
      subset_by_group_hlpr(
        object = object, 
        grouping_variable = grouping_variable, 
        groups = groups, 
        reasoning = reasoning,
        verbose = verbose
      )
    
    return(object)
    
  })

subset_by_group_hlpr <- function(object, grouping_variable, groups, reasoning, verbose){
  
  cypro_object_is_being_subsetted(object, by = "group", verbose = verbose)
  
  group_df <- getGroupingDf(object)
  
  check_one_of(
    input = grouping_variable, 
    against = getGroupingVariableNames(object)
  )
  
  check_one_of(
    input = groups, 
    against = getGroupNames(object, grouping_variable = grouping_variable)
  )
  
  cell_ids <- 
    dplyr::filter(group_df, !!rlang::sym(grouping_variable) %in% {{groups}}) %>% 
    dplyr::pull(cell_id)
  
  subset_object <-
    CyproSubsetByGroup(
      grouping_variable = grouping_variable, 
      groups = groups
    )
  
  object <- 
    subsetByCellID(
      object = object, 
      cell_ids = cell_ids,
      reasoning = reasoning,
      prev_subset = subset_object,
      verbose = verbose
    )
  
  return(object)
  
}


#' @title Create data subset by number
#'
#' @inherit subsetByCellID description params details
#'
#' @inherit argument_dummy params
#' @param across The grouping variables across which to reduce the cell number.
#' @param n_by_group Numeric value or NULL. If numeric, denotes the number of 
#' cells that are selected by every group of the grouping variables
#' denoted in \code{across}..
#' @param n_total Numeric value or NULL, If numeric, denotes the exact number of cells
#' the \code{Cypro} is supposed to contain after the subsetting.
#' @param weighted Logical. Only relevant if \code{n_total} is numeric.
#' If TRUE, the proportion of cells across the groups of the grouping variables
#' denoted in \code{across} is maintained (option: \emph{total_weighted}). If FALSE,
#' an equal number of cells is picked from each group (option: \emph{total_fixed}).
#' @param seed Numeric value or NULL. If numeric, is used to set seed and to make 
#' the subsetting reproducible. Is stored along with the argument input 
#' above in the created \code{CyproSubset} objects.
#' 
#' @section Subset mechanism:
#' 
#' Subsetting by number reduces the number of cells in the object by random selection
#' across certain grouping variables. There are three ways to subset by number:
#' 
#'  \itemize{
#'   \item{\emph{groupwise:}}{ A fixed number of cells is picked from every group. The 
#'   resulting total number of cells in the new \code{Cypro} object is the sum of groups
#'   of the chosen grouping variables multiplied by the denoted number in \code{n_by_group}.}
#'   \item{\emph{total_weighted:}}{ The number of cells in the whole data set is reduced 
#'   to a fixed number. In the new \code{Cypro} object the original proportion of cells in
#'   the groups of the grouping variable across which the object is subsetted stays the same.}
#'   \item{\emph{total_fixed:}}{ The number of cells in the whole data set is reduced 
#'   to a fixed number. In the new \code{Cypro} object very group of the grouping variable
#'   across which the data is subsetted contains the same number of cells.}
#'   } 
#'   
#' @section Detailed example:
#' \code{subsetByNumber()} first unites all grouping variables across which the number
#' of cells is supposed to be reduced to one single new variable. Cell IDs are then
#' grouped by this variable via \code{dplyr::group_by()}. The number of cell IDs is then reduced 
#' via \code{dplyr::slice_sample()}. The exact number of remaining cells can be specified
#' in two different ways by using either argument
#' \code{n_by_group} or \code{n_total}:
#' 
#' If specified with \code{n_by_group()}: The numeric value is given to argument \code{n}
#' of \code{dplyr::slice_sample()}. E.g. \code{across} = \emph{'condition'} and \code{n_by_group}
#' = 1000, if the \code{Cypro} object contains 6 different conditions the returned object contains
#' 6000 randomly selected cells - 1000 of each condition. 
#' 
#' If specified with \code{n_total}: The numeric value given to argument \code{n} of
#' \code{dplyr::slice_sample()} is calculated like this: 
#' 
#'  n = \code{n_total} / number of groups
#'  
#' E.g \code{across} = \emph{'condition'} and \code{n_total} = 10.000, if the \code{Cypro} object contains 
#' 4 different conditions 2500 cells of each condition will be in the returned object. 
#' 
#' If you want to keep the distribution across a grouping variable as is set argument \code{weighted}
#' to TRUE. In this case every groups proportion of cells is computed and the number of cells 
#' representative for each group is adjusted. 
#' 
#' E.g \code{across} = \emph{'condition'} and \code{n_total} = 10.000, if the \code{Cypro} object contains 
#' 4 different conditions and condition a represents 40% of all cells while condition b-d each represent
#' 20 % the returned \code{Cypro} object contains 4000 cells of condition a and each 2000 cells of condition b-d. 
#' 
#' @return The input object containing a data subset.
#' 
#' @export

setGeneric(name = "subsetByNumber", def = function(object, ...){
  
  standardGeneric(f = "subsetByNumber")
  
})

#' @rdname subsetByNumber
#' @export
setMethod(
  f = "subsetByNumber", 
  signature = "CyproScreening", 
  definition = function(object, 
                        across = c("cell_line", "condition"),
                        n_by_group = NULL, 
                        n_total = NULL, 
                        weighted = NULL, 
                        reasoning = NA_character_,
                        seed = NULL,
                        verbose = TRUE){
    
    object <- 
      subset_by_number_hlpr(
        object = object,
        across = across, 
        n_by_group = n_by_group, 
        n_total = n_total, 
        weighted = weighted, 
        reasoning = reasoning, 
        seed = seed, 
        verbose = verbose
      )
    
    return(object)
    
  })

#' @rdname subsetByNumber
#' @export
setMethod(
  f = "subsetByNumber", 
  signature = "CyproTimeLapse", 
  definition = function(object, 
                        across = c("cell_line", "condition"),
                        n_by_group = NULL, 
                        n_total = NULL, 
                        weighted = NULL, 
                        reasoning = NA_character_,
                        seed = NULL,
                        verbose = TRUE){
    
    object <- 
      subset_by_number_hlpr(
        object = object,
        across = across, 
        n_by_group = n_by_group, 
        n_total = n_total, 
        weighted = weighted, 
        reasoning = reasoning, 
        seed = seed, 
        verbose = verbose
      )
    
    return(object)
    
  })

subset_by_number_hlpr <- function(object, 
                                  across, 
                                  n_by_group, 
                                  n_total, 
                                  weighted, 
                                  reasoning, 
                                  seed, 
                                  verbose){
  
  # validate arg input 
  are_values("n_by_group", "n_total", "seed", mode = "numeric", skip.allow = TRUE, skip.val = NULL)
  validate_only_one_arg_specified(input = list(n_by_group = n_by_group, n_total = n_total))
  
  check_one_of(
    input = across, 
    against = getGroupingVariableNames(object)
  )
  
  combined_name <- glue::glue_collapse(x = across, sep = "_")
  
  cell_ids_df <-
    getGroupingDf(object, verbose = FALSE) %>% 
    tidyr::unite(col = {{combined_name}}, dplyr::all_of(across)) %>% 
    dplyr::select(cell_id, !!rlang::sym(combined_name))
  
  # option: groupwise
  if(base::is.numeric(n_by_group)){
    
    option <- "groupwise"
    weighted <- FALSE
    
    set_seed_hlpr(seed)
    
    cell_ids <- 
      dplyr::group_by(cell_ids_df, !!rlang::sym(combined_name)) %>% 
      dplyr::slice_sample(n = n_by_group) %>% 
      dplyr::pull(cell_id)
    
  } else if(base::is.numeric(n_total)){ # option: total_*
    
    n_groups <- 
      dplyr::pull(cell_ids_df, var = {{combined_name}}) %>% 
      dplyr::n_distinct()
    
    all_groups <- 
      dplyr::pull(cell_ids_df, var = {{combined_name}}) %>% 
      base::unique()
    
    if(base::isTRUE(weighted)){ # option total_weighted 
      
      option <- "total_weighted"
      
      set_seed_hlpr(seed)
      
      weights_df <- 
        dplyr::group_by(cell_ids_df, !!rlang::sym(combined_name)) %>% 
        dplyr::summarise(count = dplyr::n()) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(
          total = nCells(object), 
          perc = count / total
        )
      
      cell_ids <- 
        purrr::map(
          .x = all_groups, 
          .f = function(group){
            
            perc_val <- 
              dplyr::filter(weights_df, !!rlang::sym(combined_name) == {{group}}) %>% 
              dplyr::pull(perc)
            
            n_of_group <-
              base::round(n_total * perc_val, digits = 0)
            
            ids <- 
              dplyr::filter(cell_ids_df, !!rlang::sym(combined_name) == {{group}}) %>% 
              dplyr::slice_sample(n = n_of_group) %>% 
              dplyr::pull(cell_id)
            
            return(ids)
            
          }
        ) %>% 
        purrr::set_names(nm = all_groups) %>% 
        purrr::flatten_chr()
      
      
    } else {
      
      option <- "total_fixed"
      
      n_by_group <- base::round(n_total/n_groups, digits = 0)
      
      set_seed_hlpr(seed)
      
      cell_ids <- 
        dplyr::group_by(cell_ids_df, !!rlang::sym(combined_name)) %>% 
        dplyr::slice_sample(n = n_by_group) %>% 
        dplyr::pull(cell_id)
      
    }
    
  }
  
  cypro_object_is_being_subsetted(
    object = object, 
    by = glue::glue("number (option: {option})")
  )
  
  prev_subset <- 
    CyproSubsetByNumber(
      n_by_group = base::ifelse(test = base::is.numeric(n_by_group), yes = n_by_group, no = NA_integer_),
      n_total = base::ifelse(test = base::is.numeric(n_total), yes = n_total, no = NA_integer_), 
      weighted = base::ifelse(test = base::is.numeric(n_total), yes = weighted, no = NA),
      option = option, 
      seed = base::ifelse(test = base::is.numeric(seed), yes = seed, no = NA_integer_)
    )
  
  object <- 
    subsetByCellID(
      object = object, 
      cell_ids = cell_ids, 
      reasoning = reasoning, 
      prev_subset = prev_subset,
      verbose = verbose
    )
  
  return(object)
}
