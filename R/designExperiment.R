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
        
        ed_results <- moduleExperimentDesignServer(id = "ed", usage = "in_function")
        
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
          
          cypro_object <- ed_list$object
          
          cypro_object <- setProgress(object = cypro_object, designExperiment = TRUE)
          
          shiny::stopApp(returnValue = cypro_object)
          
        })
        
      }
    )
  )
  
}