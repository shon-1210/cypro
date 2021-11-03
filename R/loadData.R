
#' @title Object initiation: Step 3
#' 
#' @inherit designExperiment description
#'
#' @inherit argument_dummy params 
#' @param process_object Logical. If TRUE (the default), the function \code{processData()}
#' is immediately called. If FALSE it has to be called manually. 
#' 
#' @details Apart from the naming convention they follow, output files from different
#' image analysis software vary in their number. There are four ways how
#' the input data that is supposed to be loaded can exist: 
#' 
#' \enumerate{
#'   \item A single file:
#'   One single file contains all the data. Apart from the data
#'   that has been quantified this includes identifier variables that correspond to the
#'   \emph{cypro-known} data variables \emph{Well plate (well_plate_name)}, \emph{Well (well)},
#'   \emph{Region of interest (roi)} that in combination with the \emph{cell ID (cell_id)} identify
#'   a cell uniquely throughout the whole data set.
#'   \item A file per well plate:
#'    Every file contains the optional identifier variables \emph{Well (well)} and \emph{Region of interest (roi)}.
#'    Information regarding the well plate belonging is added by assigning a single file 
#'    directory to every well plate the experiment design contains.
#'    \item A file per well:
#'     Every file contains the optional identifier variable 
#'    \emph{Region of interest (roi)}. Information regarding the well plate  
#'    belonging is added by assigning each well plate a directory leading 
#'    to a folder. The folder is then skimmed for files of type \emph{.csv, .txt, .xls, .xlsx}.
#'    The well belonging must be encoded in the file directory (e.g ~/image_spreadsheet_A04.csv).
#'    \item A file per image/image stack:
#'    Every image/image stack has been quantified by itself.
#'    The file does not contain any meta data regarding the well plate, the well or the region 
#'    of interest. To add the information regarding the well plate a directory leading to 
#'    a folder is assigned to every well plate. Then, the folder is skimmed for files of type
#'    \emph{.csv, .txt, .xls, .xlsx}. The well-roi belonging must be encoded in the file directory
#'    (e.g. ~assigned_folder/image_spreadsheet_A4_3.xslx).
#'    }
#'  
#'  The first option applies if the example file uploaded during \code{assignVariables()}
#'  contains all identifier variables and the \emph{Well plate (well_plate_name)} variable
#'  contains all well plate names of the experiment design. In this case no interface is 
#'  opened and the example data.frame is considered to contain the whole data set.
#'  
#'  In case of the latter three options the function \code{loadData()} decides which
#'  of the three remaining loading modalities fits your data depending on the example data.frame
#'  of \code{assignVariables()} and the variables that have been assigned - or have
#'  not been assigned.
#'  
#'  Information about the completeness of the data set and
#'  of the validity of each file and its content is immediately provided. 
#'  
#'  After the data loading, by default, the read in data is immediately
#'  processed within the \code{Cypro} object using the function \code{processData()}.
#'  
#'  The returned \code{Cypro} object is ready for downstream analysis. (If argument
#'  \code{process_data} is set to FALSE the function \code{processData()} or its
#'  sub functions must be called manually.)
#'  
#'  See documentation for S4-classes in the @seealso section for a more detailed
#'  description of how the data file reading is programmatically implemented.
#'  
#' @seealso \code{WellPlate}, \code{DataFile}
#'
#' @return The input object.
#' @export

setGeneric(name = "loadData", def = function(object, ...){
  
  standardGeneric(f = "loadData")
  
})

#' @rdname loadData
#' @export

setMethod(f = "loadData", signature = "Cypro", definition = function(object, process_object = TRUE, verbose = TRUE){
  
  object <- 
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
            
            cypro_object <- 
              setProgress(
                object = ld_list$object, 
                loadData = TRUE
              )
            
            shiny::stopApp(returnValue = cypro_object)
            
          })
          
        }
      )
    )
  
  object <- setProgress(object, loadData = TRUE, verbose = verbose)
  
  if(base::isTRUE(process_object)){
    
    object <- processData(object, verbose = verbose)
    
  } else {
    
    give_feedback(
      msg = "Did not call 'processData()' as argument is not TRUE.", 
      verbose = verbose
    )
    
  }
  
  return(object)
  
})
