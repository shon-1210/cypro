
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
#'

setGeneric(name = "loadData", def = function(object){
  
  standardGeneric(f = "loadData")
  
})

#' @rdname loadData
#' @export

setMethod(f = "loadData", signature = "Cypro", definition = function(object){
  
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
        
        ld_results <- moduleLoadDataServer(id = "ld", object = object)
        
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
  
  
})
