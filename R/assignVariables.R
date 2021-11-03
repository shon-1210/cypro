



#' @title Set up \code{Cypro} object: Step 2
#' 
#' @inherit designExperiment description 
#'
#' @inherit argument_dummy params
#' 
#' @details The \code{cypro} framework ensures compatibility with the output
#' of virtually every image analysis software like \emph{CellTracker}, \emph{CellProfiler}, 
#' \emph{ImageJ}. The structure of their outputted data files is similar in 
#' so far as that each row represents a cell and every column (or data variable) represents 
#' either a quantified cellular feature or meta data such as cell ID, well belonging, condition etc. 
#' 
#' The outputted data files differ, however, in their naming conventions. Data 
#' variables that are used throughout the analysis of high content screening,
#' such as \emph{x-} and \emph{y-coordinates}, \emph{cell ID}, \emph{frame number}
#' are named differently from image analysis software to image analysis software. 
#' 
#' To account for these differing naming conventions cypro \emph{"knows"} certain 
#' data variables - like those that are exemplarily mentioned and printed in
#' italic in the previous paragraph. 
#' 
#' In order for \code{cypro} to know which of the data variables of the input data
#' are those that are repeatedly used they need to be assigned. The function 
#' \code{assignVariables()} therefore opens an interactive application in which 
#' an example input file is loaded.  By assigning variable names of the input
#' manually to the variables \code{cypro} knows the user can then declare which of the 
#' variables \code{cypro} knows are part of the input that will be loaded later on.
#' 
#' See documentation for S4-classes in the @@seealso section for a more detailed 
#' description of how the \code{DataVariable} ecosystem is programmatically 
#' implemented.
#' 
#' @seealso \code{DataVariable}, \code{AssignableVariable}, \code{ComputableVariable}, 
#' \code{OptionalVariable}, \code{SummaryVariable}, \code{AnalysisModule}
#'
#' @return The input object.
#' @export

setGeneric(name = "assignVariables", def = function(object){
  
  standardGeneric(f = "assignVariables")
  
})

#' @rdname assignVariables
#' @export
setMethod(
  f = "assignVariables",
  signature = "Cypro",
  definition = function(object){
    
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
                                        
                                        moduleAssignVariablesUI(id = "av"), 
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
          
          av_results <- moduleAssignVariablesServer(id = "av", object = object)
          
          output$return_cypro <- shiny::renderUI({
            
            av_list <- shiny::reactiveValuesToList(av_results)
            
            if(shiny::isTruthy(av_list$proceed)){
              
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
            
            av_list <- shiny::reactiveValuesToList(av_results)
            
            cypro_object <- 
              setProgress(
                object = av_list$object, 
                assignVariables = TRUE
              )
            
            shiny::stopApp(returnValue = cypro_object)
            
          })
          
        }
      )
    )
    
  })


#' @title Manual option of assigning variables
#' 
#' @description something
#' 
#' @inherit argument_dummy params
#' @param var_names_required,var_names_optional,var_names_computable Named lists of character values where the names 
#' correspond to the slot @@name_in_cypro and the character value to slot @@name_in_example
#' 
#' @return The input object. 

setGeneric(name = "assignVariablesManually", def = function(object, ...){
  
  standardGeneric(f = "assignVariablesManually")
  
})

#' @rdname assignVariablesManually
#' @export
setMethod(
  f = "assignVariablesManually",
  signature = "Cypro",
  definition = function(object,
                        module_name, 
                        var_names_required = NULL, 
                        var_names_optional = NULL, 
                        var_names_computable = NULL, 
                        fdb_if_false = TRUE, 
                        fdb_fn = "warning",
                        verbose = TRUE,
                        ...){
    
    active_modules <- getActiveModuleNames(object)
    
    confuns::is_value(x = module_name, mode = "character")
    
    confuns::check_one_of(
      input = module_name, 
      against = active_modules, 
      fdb.opt = 2, 
      ref.opt.2 = "used analysis modules"
    )
    
    module <- getModule(object, module_name = module_name)
    
    base::stopifnot(base::is.null(var_names_required) | confuns::is_named(var_names_required))
    
    names_var_names_required <- base::names(var_names_required)
    
    confuns::check_one_of(
      input = names_var_names_required,
      against = base::names(module@variables_required), 
      fdb.opt = 2, 
      ref.input = "required variable names"
    )
    
    for(vname in names_var_names_required){
      
      module@variables_required[[vname]]@name_in_example <- var_names_required[[vname]]
      
    }
    
    if(containsComputableVariables(module)){
      
      base::stopifnot(base::is.null(var_names_computable) | confuns::is_named(var_names_computable))
      
      names_var_names_computable <- base::names(var_names_computable)
      
      confuns::check_one_of(
        input = names_var_names_computable,
        against = base::names(module@variables_computable), 
        fdb.opt = 2, 
        ref.input = "computable variable names"
      )
      
      for(vname in names_var_names_computable){
        
        module@variables_computable[[vname]]@name_in_example <- var_names_computable[[vname]]
        
      }
      
    }
    
    if(containsOptionalVariables(module)){
      
      base::stopifnot(base::is.null(var_names_optional) | confuns::is_named(var_names_optional))
      
      names_var_names_optional <- base::names(var_names_optional)
      
      confuns::check_one_of(
        input = names_var_names_optional,
        against = base::names(module@variables_optional), 
        fdb.opt = 2, 
        ref.input = "optional variable names"
      )
      
      for(vname in names_var_names_optional){
        
        module@variables_optional[[vname]]@name_in_example <- var_names_optional[[vname]]
        
      }
      
    }
    
    object@modules[[module_name]] <- module
    
    isUsable(
      object = object,
      module_name = module_name,
      fdb_if_false = fdb_if_false,
      fdb_fn = fdb_fn, 
      with.time = FALSE,
      verbose = verbose,
      ...
    )
    
    return(object)
    
  })





