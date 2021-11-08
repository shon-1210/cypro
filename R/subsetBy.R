


#' @title Create data subset by cell ids
#' 
#' @description Subset functions allow to conveniently split your data by certain characteristics such 
#' as cell lines, conditions, cluster etc. or for specific cell ids. This might be useful if you want apply some machine learning 
#' algorithms such as clustering and correlation on only a subset of cells. See details for more information.
#'
#' @inherit argument_dummy params
#' @param cell_ids Character vector. Denotes the cells to keep unambiguously with their cell IDs.
#' @param new_name Character value. Denotes the name of the output object. If set to NULL the name of the 
#' input object is taken and suffixed with \emph{'_subset'}.
#' @param new_directory Character value. Storage directory for the new object. 
#' @param reasoning Character value. Allows two write a short description of how the cell ids according 
#' to which the object is filtered were selected. This description is included in the output of \code{printSubsetHistory()}.
#' Ignored if set to NULL. 
#'
#' @details Creating subsets of your data affects analysis results such as clustering, dimensional redurction and correlation which 
#' is why these results are reseted in the subsetted object and must be computed again. To prevent inadvertent overwriting 
#' the default directory is reset as well. Make sure to set a new one via \code{setDefaultDirectory()}. 
#' 
#' The mechanism with which you create the subset is stored in the output object. Use \code{printSubsetHistory()}
#' to reconstruct the way from the original object to the current one. 
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
                        new_name, 
                        new_directory = NA,
                        reasoning = NULL,
                        verbose = TRUE, 
                        ...){
    
    object <- 
      subset_by_cell_id_hlpr(
        object = object, 
        new_name = new_name, 
        new_directory = new_directory,
        cell_ids = cell_ids, 
        reasoning = reasoning, 
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
                        new_name, 
                        new_directory = NA,
                        reasoning = NULL,
                        verbose = TRUE, 
                        ...){
    
    object <- 
      subset_by_cell_id_hlpr(
        object = object, 
        new_name = new_name, 
        new_directory = new_directory,
        cell_ids = cell_ids, 
        reasoning = reasoning, 
        verbose = verbose,
        ...
      )
    
    return(object)
    
  })

subset_by_cell_id_hlpr <- function(object,
                                   new_name, 
                                   new_directory, 
                                   cell_ids, 
                                   reasoning,
                                   verbose,
                                   ...){
  
  # input validation
  is_vec(x = cell_ids, mode = "character")
  
  cell_ids <- base::unique(cell_ids)
  
  base::stopifnot(base::all(cell_ids %in% getCellIDs(object)))
  
  base::stopifnot(new_name != object@experiment)
  
  all_cell_ids <- getCellIDs(object)
  
  discarded_cell_ids <- all_cell_ids[!all_cell_ids %in% cell_ids]
  
  # filter slot @cdata
  cdata <- getCdata(object)
  
  for(slot in methods::slotNames(cdata)){
    
    df <- methods::slot(object = cdata, name = slot)
    
    filtered_df <- dplyr::filter(df, cell_id %in% {{cell_ids}})
    
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
  
  # save subset information 
  subset_object_input <- 
    purrr::keep(.x = list(...), .p = ~ isOfClass(x = .x, valid_class = "CyproSubset"))
  
  if(base::length(subset_object_input) > 1){
    
    stop("Invalid input within '...'.")
    
  } else if(base::length(subset_object_input)){
    
    subset_object <- subset_object_input[[1]] 
    
    subset_object@ids_discarded <- discarded_cell_ids
    subset_object@ids_remaining <- cell_ids
    
    subset_object@new_name <- new_name
    subset_object@parent_name <- object@experiment
    
    subset_object@reasoning <- reasoning
    
  } else if(base::length(subset_object_input) == 0){
    
    subset_object <- 
      CyproSubsetByCellID(
        new_name = new_name, 
        parent_name = object@experiment, 
        reasoning = reasoning, 
        ids_discarded = discarded_cell_ids,
        ids_remaining = cell_ids
      )
    
  }
  
  object <- addSubset(object, subset_object = subset_object)
  
  # change name and directory
  object <- setExperimentName(object, name = new_name)
  
  if(!base::is.character(new_directory)){
    
    object <- discardStorageDirectory(object)
    
  } else {
    
    object <- setStorageDirectory(object, directory = new_directory)
    
  }
  
  object <- setCellIDs(object, cell_ids = cell_ids)
  
  give_feedback(
    msg = glue::glue("Subsetting done. {nCells(object)} cells remain."), 
    verbose = verbose
  )
  
  return(object)
  
}