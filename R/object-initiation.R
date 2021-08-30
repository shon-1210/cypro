
#' @title Initiate empty cypro object 
#' 
#' @description Initiates an empty cypro object and 
#' adds the version under which is is created.
#'
#' @param ... 
#'
initiateEmptyCyproObject <- function(...){
  
  object <- methods::new(Class = "cypro", ...)
  
  object@version <- current_version
  
  base::return(object)
  
}

#' @title Object initiation: Step 1
#' 
#' @description Opens an interactive application in which the experiment can be set 
#' up. This includes in particular the design of the well plates. 
#'
#' @return An empty cypro object. 
#' @export
#'
designExperiment <- function(){
  
  shiny::runApp(
    shiny::shinyApp(
      ui = function(){
        shinydashboard::dashboardPage(
          
          header = shinydashboard::dashboardHeader(title = app_title), 
          
          sidebar = shinydashboard::dashboardSidebar(
            collapsed = TRUE, 
            shinydashboard::sidebarMenu(
              shinydashboard::menuItem(
                text = "New Session", 
                tabName = "new_session", 
                selected = TRUE
              )
            )
          ), 
          
          body = shinydashboard::dashboardBody(
            
            shinydashboard::tabItems(
              shinydashboard::tabItem(
                tabName = "new_session",
                
                moduleExperimentDesignUI(id = "ed"), 
                shiny::fluidRow(
                  shiny::column(
                    width = 12,
                    align = "center",
                    shiny::uiOutput(outputId = "return_cypro")
                  )
                )
                
              )
            )
            
          )
          
        )
      }, 
      server = function(input, output, session){
        
        # shiny helper 
        shinyhelper::observe_helpers()
        
        ed_results <-
          moduleExperimentDesignServer(id = "ed", usage = "in_function")
        
        output$return_cypro <- shiny::renderUI({
          
          ed_list <- shiny::reactiveValuesToList(ed_results)
          
          if(base::isTRUE(ed_list$proceed)){
            
            color <- "success"
            
          } else {
            
            color <- "warning"
            
          }
          
          shinyWidgets::actionBttn(
            inputId = "return_cypro",
            label = "Return Cypro Object", 
            color = color, 
            style = "gradient"
          )
          
        })
        
        oe <- shiny::observeEvent(input$return_cypro, {
          
          ed_list <- shiny::reactiveValuesToList(ed_results)
          
          check <- base::tryCatch({
            
            base::class(ed_list$object) == "cypro"
            
          }, error = function(error){
            
            FALSE
            
          })
          
          checkpoint(evaluate = check, case_false = "incomplete_cypro")
          
          cypro_object <- ed_list$object
          
          cypro_object@set_up$progress$experiment_design <- TRUE
          
          shiny::stopApp(returnValue = cypro_object)
          
        })
        
      }
    )
  )
  
}


#' @title Object initiation: Step 2
#' 
#' @description Opens an interactive application in which the data to 
#' be analyzed is loaded. 
#' 
#' \code{loadDataFile()} assumes that the data 
#' exist in one .csv, .xls, .xlsx or .txt file and that this data table 
#' contains variables/columns that identify the well plate, the well 
#' and  the region of interest.
#' 
#' \code{loadDataFiles()} assumes that the data tables exist for every well-region 
#' of interest combination (every stack of images). Here you can assign a directory
#' to every well plate leading to a folder in which the files are placed. 
#' Successful reading of the files requires them to be named according to 
#' the syntax: \emph{well_roi.filetype}, e.g A1_1.xlsx, B12_4.csv
#'
#' @inherit argument_dummy params 
#'
#' @return An updated cypro object.
#' @export
#'
loadDataFile <- function(object){
  
  check_object(object, set_up_req = "experiment_design")
  
  shiny::runApp(
    shiny::shinyApp(
      ui = function(){
        shinydashboard::dashboardPage(
          
          header = shinydashboard::dashboardHeader(title = app_title), 
          
          sidebar = shinydashboard::dashboardSidebar(
            collapsed = TRUE, 
            shinydashboard::sidebarMenu(
              shinydashboard::menuItem(
                text = "New Session", 
                tabName = "new_session", 
                selected = TRUE
              )
            )
          ), 
          
          body = shinydashboard::dashboardBody(
            
            shinybusy::add_busy_spinner(spin = "cube-grid", margins = c(0,10), color = "red"),
            
            shinydashboard::tabItems(
              shinydashboard::tabItem(tabName = "new_session",
                                      
                                      moduleLoadDataFileUI(id = "ld"), 
                                      shiny::fluidRow(
                                        shiny::column(width = 12, align = "center", 
                                                      
                                                      shiny::uiOutput(outputId = "return_cypro")
                                                      
                                        )
                                      )
              )
            )
            
          )
          
        )
      }, 
      server = function(input, output, session){
        
        # shiny helper 
        shinyhelper::observe_helpers()
        
        ld_results <-
          moduleLoadDataFileServer(id = "ld", object = object)
        
        output$return_cypro <- shiny::renderUI({
          
          ld_list <- shiny::reactiveValuesToList(ld_results)
          
          if(shiny::isTruthy(ld_list$proceed)){
            
            color <- "success"
            
          } else {
            
            color <- "warning"
            
          }
          
          shinyWidgets::actionBttn(
            inputId = "return_cypro",
            label = "Return Cypro Object", 
            color = color, 
            style = "gradient"
          )
          
        })
        
        
        oe <- shiny::observeEvent(input$return_cypro, {
          
          ld_list <- shiny::reactiveValuesToList(ld_results)
          
          check <- base::tryCatch({
            
            base::class(ld_list$object) == "cypro"
            
          }, error = function(error){
            
            FALSE
            
          })
          
          checkpoint(evaluate = check, case_false = "incomplete_cypro2")
          
          cypro_object <- ld_list$object
          
          cypro_object@set_up$progress$load_data <- TRUE
          
          if(!isTimeLapseExp(cypro_object)){
            
            cypro_object@set_up$progress$quality_check <- TRUE
            
          }
          
          shiny::stopApp(returnValue = cypro_object)
          
        })
        
      }
    )
  )
  
}

#' @rdname loadDataFile
#' @export
loadDataFiles <- function(object){
  
  check_object(object, set_up_req = "experiment_design")
  
  shiny::runApp(
    shiny::shinyApp(
      ui = function(){
        shinydashboard::dashboardPage(
          
          header = shinydashboard::dashboardHeader(title = app_title), 
          
          sidebar = shinydashboard::dashboardSidebar(
            collapsed = TRUE, 
            shinydashboard::sidebarMenu(
              shinydashboard::menuItem(
                text = "New Session", 
                tabName = "new_session", 
                selected = TRUE
              )
            )
          ), 
          
          body = shinydashboard::dashboardBody(
            
            shinydashboard::tabItems(
              shinydashboard::tabItem(tabName = "new_session",
                                      
                                      moduleLoadDataUI(id = "ld"), 
                                      shiny::fluidRow(
                                        shiny::column(width = 12, align = "center", 
                                                      
                                                      shiny::uiOutput(outputId = "return_cypro")
                                                      
                                        )
                                      )
              )
            )
            
          )
          
        )
      }, 
      server = function(input, output, session){
        
        # shiny helper 
        shinyhelper::observe_helpers()
        
        ld_results <-
          moduleLoadDataServer(id = "ld", object = object)
        
        output$return_cypro <- shiny::renderUI({
          
          ld_list <- shiny::reactiveValuesToList(ld_results)
          
          if(shiny::isTruthy(ld_list$proceed)){
            
            color <- "success"
            
          } else {
            
            color <- "warning"
            
          }
          
          shinyWidgets::actionBttn(
            inputId = "return_cypro",
            label = "Return Cypro Object", 
            color = color, 
            style = "gradient"
          )
          
        })
        
        
        oe <- shiny::observeEvent(input$return_cypro, {
          
          ld_list <- shiny::reactiveValuesToList(ld_results)
          
          check <- base::tryCatch({
            
            base::class(ld_list$object) == "cypro"
            
          }, error = function(error){
            
            FALSE
            
          })
          
          checkpoint(evaluate = check, case_false = "incomplete_cypro2")
          
          cypro_object <- ld_list$object
          
          cypro_object@set_up$progress$load_data <- TRUE
          
          if(!isTimeLapseExp(cypro_object)){
            
            cypro_object@set_up$progress$quality_check <- TRUE
            
          }
          
          shiny::stopApp(returnValue = cypro_object)
          
        })
        
      }
    )
  )
  
}


#' @title Object initiation: Step 3
#' 
#' @description Processes the data and constructs all needed slots. Afterwards the 
#' cypro object is set for all subsequent analysis and visualization steps. 
#'
#' @inherit argument_dummy params
#'
#' @inherit updated_object return
#' @export
#'
processData <- function(object,
                        summarize_with = c("max", "min", "mean", "median"),
                        verbose = TRUE){
  
  check_object(object, set_up_req = "load_data")
  
  # add default list 
  object@default <- c(object@default, default_list)
  
  confuns::check_one_of(
    input = summarize_with, 
    against = base::names(stat_funs)
  )
  
  if(base::isTRUE(object@set_up$progress$process_data)){
    
    base::stop("Object has already been processed.")
    
  }

  # update progress slot
  object@set_up$progress$process_data <- TRUE
  
  confuns::give_feedback(msg = "Processing data.", verbose = verbose)
  
  # set up different data slots ---------------------------------------------
  
  # set up wp data
  object <- set_up_cdata_well_plate(object, verbose = verbose)
  
  # set up meta data 
  object <- set_up_cdata_meta(object, verbose = verbose)
  
  # set up cluster data 
  object <- set_up_cdata_cluster(object, verbose = verbose)
  
  # set up tracks 
  object <- set_up_cdata_tracks(object, verbose = verbose)
  
  # set up stats 
  if(isTimeLapseExp(object)){
    
    object <- 
      set_up_cdata_stats(
        object = object,
        summarize_with = summarize_with,
        verbose = verbose
        )  
    
  }
  
  # set up variable data
  object <- set_up_vdata(object, verbose = verbose)
  
  # miscellaneous -----------------------------------------------------------
  
  if(multiplePhases(object)){
    
    all_phases <- getPhases(object)
    
    object@qcheck$na_count <- 
      purrr::map(
        .x = all_phases,
        .f =
          ~ compute_n_missing_values(
              object = object,
              phase = .x,
              verbose = verbose
            )
        ) %>% 
      purrr::set_names(nm = all_phases)
    
  } else if(isTimeLapseExp(object)) {
    
    object@qcheck$na_count <- 
      compute_n_missing_values(object, verbose = verbose)
    
  } else {
    
    # no NA counting by cell id in non time lapse experiments 
    
  }
  
  # remove wp_df in favor of wp_df_eval
  object@well_plates <-
    purrr::map(.x = object@well_plates, .f = function(wp_list){
                 
                 wp_list$wp_df <- NULL
                 
                 base::return(wp_list)
                 
                 })
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}


