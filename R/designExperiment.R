#' @title Set up \code{Cypro} object: Step 1
#' 
#' @description Three functions that provide access to interactive applications 
#' are needed to set up a fully functional \code{Cypro} object. 
#' 
#' \enumerate{
#'  \item \code{designExperiment()}: Reconstructs the experiment design based on which 
#'  input data is expected and read in.
#'  \item \code{assignVariables()}: Accounts for the different naming conventions of 
#'  different image analysis software from which the input data derive from.
#'  \item \code{loadData()}: Provides access to the file system. Depending on 
#'  the example used in \code{assignVariables()} folders or files are assigned 
#'  to well plates designed in \code{designExperiment()}. The respective data is 
#'  loaded, checked for validity and processed.
#'  }
#' 
#' The resulting \code{Cypro} object is ready to use for downstream analysis. 
#' 
#' @details This function does not take any arguments as input and creates 
#' the \code{Cypro} object from scratch.
#' 
#' First the underlying imaging process is denoted which determines the subclass 
#' of the \code{Cypro} object. This can be one of three types:
#' 
#' \itemize{
#'  \item{\code{CyproScreening}}{ Every cell was captured exactly one time. The 
#'  data input must follow a structure in which each row represents a cell identified
#'  by a cell id.}
#'  \item{\code{CyproTimeLapse}}{ Every cell was captured in defined intervals
#'  over a period of time - time lapse experiment. The data input must follow 
#'  a structure in which each row represents a cell at a given time point identified
#'  by a cell ID and a frame number.}
#'  \item{\code{CyproTimeLapseMP}}{ Similar concept as in \emph{CyproTimeLapse}. 
#'  However, the conditions in which cells prevailed over the imaging process 
#'  changed over time. The experiment design distinguishes different phases - 
#'  MP = multiple phases}
#' }
#' 
#' In case of time lapse experiments information regarding the imaging process
#' and the start and end points of different phases must be provided. 
#' 
#' In all cases the layout of the well plates is reconstructed including the
#' cell line seeded in the respective well and the condition(s) in which these
#' cells prevailed. 
#' 
#' See documentation for S3- and S4-classes in the @@seealso section for a more detailed
#' description of how the experiment design is programmatically implemented.
#'
#' @return An S4-object of class \code{CyproScreening}, \code{CyproTimeLapse} or 
#' \code{CyproTimeLapseMP}.
#' 
#' @seealso 
#' 
#' \itemize{
#'  \item{S3-classes:}{ \code{layout_df}, \code{layout_df_mp}}
#'  \item{S4-classes:}{ \code{CyproScreening}, \code{CyproTimeLapse}, \code{CyproTimeLapseMP},
#'  \code{ExperimentDesign}, \code{WellPlate}}
#'  }
#' 
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