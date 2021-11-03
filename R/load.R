




# d -----------------------------------------------------------------------

load_data_file <- function(directory){
  
  if(stringr::str_detect(string = directory, pattern = ".csv$")){
    
    df <- 
      base::suppressMessages({
        
        base::suppressWarnings({
          
          readr::read_csv(file = directory)
          
        })
        
      })
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".xlsx$")){
    
    df <- readxl::read_xlsx(path = directory, sheet = 1)
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".xls")){
    
    df <- readxl::read_xls(path = directory, sheet = 1)
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".txt")){
    
    df <- utils::read.delim(file = directory, header = TRUE)
    
  }
  
  if(tibble::has_rownames(df)){
    
    df <- tibble::rownames_to_column(df, var = "rownames")
    
  }
  
  return(df)
  
}



# D -----------------------------------------------------------------------

#' @title Data loading - Read data files 
#' 
#' @description Second of three data loading steps. Reads data files and 
#' validates their content based on the variables assigned during 
#' \code{assignVariables()}. See details for more.
#'
#' @inherit argument_dummy params
#' 
#' @details Every well plate that has been prepared via \code{prepareDataLoading()}
#' contains a list of \code{DataFile} objects in slot @@files which again contain 
#' the directories to each single file that is supposed to be loaded. 
#' 
#' \code{DataFiles} that have already been read without any errors are skipped. Every 
#' other \code{DataFiles} is read and the read content is validated regarding
#' the variable assignment during \code{assignVariables()}. The resulting named list 
#' is then stored in slot @@content of the respective \code{DataFile} object. 
#' Error messages that occurred during the validation appear in form of \code{glue}
#' objects of length one in the list of slot @@content.
#' 
#' \code{loadDataFiles()} can be called once after all directories have been set
#' via several calls to \code{prepareDataLoading()} or several times as files that
#' were loaded without errors are skipped. This allows to read files that 
#' contained errors again without reading every other file, too.
#' 
#' Once all files are read, proceed with \code{processData()}.
#'
#' @return The input object. 
#' @export
#'
setGeneric(name = "loadDataFiles", def = function(object,
                                                  well_plates = NULL,
                                                  check_vars = TRUE, 
                                                  check_additional = TRUE, 
                                                  in_shiny = FALSE){
  
  standardGeneric(f = "loadDataFiles")
  
})

#' @rdname loadDataFiles
#' @export
setMethod(
  f = "loadDataFiles", 
  signature = "Cypro", 
  definition = function(object,
                        well_plates = NULL,
                        check_vars = TRUE, 
                        check_additional = TRUE, 
                        in_shiny = FALSE){
    
    well_plate_list <- 
      getWellPlates(object, well_plates = well_plates) %>% 
      purrr::keep(.p = containsDirectory)
    
    n_well_plates <- base::length(well_plate_list)
    
    give_feedback(
      msg = glue::glue(
        "Loading files for {n_well_plates} {ref}.",
        ref = adapt_reference(well_plate_list, "well plate")
      ), 
      in.shiny = in_shiny, 
      duration = 15
    )
    
    for(well_plate_obj in well_plate_list){
      
      data_files <- well_plate_obj@files
      
      # skip files that have been loaded allready without any errors
      # files that have not been loaded or have been loaded and 
      # contained any errors are loaded again 
      data_files_not_loaded <- 
        purrr::discard(.x = data_files, .p = ~ isLoaded(.x) & !containsErrors(.x))
      
      n_files <- base::length(data_files_not_loaded)
      wp_name <- well_plate_obj@name
      
      give_feedback(
        msg = glue::glue(
          "Loading {n_files} {ref} for well plate '{wp_name}'.",
          ref = adapt_reference(data_files_not_loaded, "file")
        ), 
        in.shiny = in_shiny, 
        duration = 15
      )
      
      if(n_files >= 1){
        
        file_names <- base::names(data_files_not_loaded)
        
        pb <- create_progress_bar(total = n_files)
        
        for(file_name in file_names){
          
          pb$tick()
          
          data_file <- data_files_not_loaded[[file_name]]
          
          loaded_df <- load_data_file(directory = data_file@directory) 
          
          data_file@content <- 
            validateInputDf(
              object = object,
              df = loaded_df, 
              check_vars = check_vars, 
              check_additional = check_additional
            )
          
          data_file@valid <- !containsErrors(object = data_file)
          
          # set immediately in original data file list!
          data_files[[file_name]] <- data_file
          
        }
        
        # set files in well plate 
        well_plate_obj@files <- data_files
        
        # set immediately 
        object <- setWellPlate(object = object, well_plate_object = well_plate_obj)
        
        give_feedback(msg = glue::glue("Finished data loading for well plate '{wp_name}'."), in.shiny = in_shiny)
        
      }
      
    }
    
    return(object)
    
  }
)





