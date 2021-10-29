#' @title UI logic: Load Data
#' 

moduleAssignVariablesUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      blue_box(
        title = "Assign Data Variables", width = 12, 
        shiny::fluidRow(
          shiny::column(
            shinydashboard::box(
              shiny::helpText(
                "Choose a example file (.csv, .txt, .xls or .xlsx). ",
                "(Only the first 6 rows will be displayed.)"
              ),
              shiny::fileInput(
                inputId = ns("example_dir"),
                label = "Choose",
                accept = c(".xls", ".xlsx", ".csv"),
                width = "33%"
              ),
              DT::dataTableOutput(outputId = ns("example_table")),
              solidHeader = TRUE,
              status = "success",
              title = "Step 1: Load template file",
              width = 12
            ),
            shiny::HTML("<br><br>"),
            shiny::uiOutput(outputId = ns("module_variables")),
            shiny::HTML("<br><br>"),
            shiny::uiOutput(ns("additional_variables")),
            width = 12
          )
        )
      )
    )
  )
  
}