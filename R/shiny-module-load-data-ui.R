#' @title UI logic: Load Data
#' 

moduleLoadDataUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      blue_box(
        title = "Prepare Data Loading", width = 12, 
        shiny::fluidRow(
          shiny::column(
            shinydashboard::box(
              shiny::helpText(
                "Choose a template file (.csv,.xls or .xlsx) ",
                "that serves as a template to check files you are going to read in. ",
                "(Only the first 6 rows will be displayed.)"
              ),
              shiny::fileInput(
                inputId = ns("pdl_example_dir"),
                label = "Choose",
                accept = c(".xls", ".xlsx", ".csv"),
                width = "33%"
              ),
              DT::dataTableOutput(outputId = ns("pdl_example_table")),
              solidHeader = TRUE,
              status = "success",
              title = "Step 1: Load template file",
              width = 12
            ),
            shiny::HTML("<br><br>"),
            shiny::uiOutput(outputId = ns("identifier_variables")),
            shiny::HTML("<br><br>"),
            shiny::uiOutput(outputId = ns("module_variables")),
            shiny::HTML("<br><br>"),
            shiny::uiOutput(ns("additional_variables")),
            width = 12
          )
        )
      )
    ),
    shiny::fluidRow(
      blue_box(
        title = "Load Data", width = 12, 
        shiny::fluidRow(
          shiny::column(
            align = "left",
            shiny::uiOutput(outputId = ns("ld_well_plate_box")),
            width = 6
          ), 
          shiny::column(
            shiny::fluidRow(
              hs(12, shiny::uiOutput(outputId = ns("ld_loading_box")))
            ), 
            shiny::fluidRow(
              hs(12,shiny::uiOutput(outputId = ns("ld_save_and_proceed_box")))
            ),
            width = 6
          )
        )
      )
    )
  )
  
}