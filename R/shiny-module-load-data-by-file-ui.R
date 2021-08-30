#' @title UI logic: Load Data
#' 

moduleLoadDataFileUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      blue_box(
        title = "Load Data File", width = 12, 
        shiny::fluidRow(
          shiny::column(
            shinydashboard::box(
              shiny::helpText(
                "Choose the file (.csv, .txt, .xls or .xlsx) ",
                "that contains the data you want to analyze." ,
                "Depending on the size of the file this might take a few ",
                "moments."
              ),
              shiny::fileInput(
                inputId = ns("pdl_example_dir"),
                label = "Choose",
                accept = c(".xls", ".xlsx", ".csv", ".txt"),
                width = "33%"
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 8, 
                  DT::dataTableOutput(outputId = ns("pdl_example_table"))
                ), 
                shiny::column(
                  width = 4,
                  shiny::plotOutput(outputId = ns("ld_well_plate_plot")), 
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 8, 
                  shiny::uiOutput(outputId = ns("ld_var_names")) 
                ), 
                shiny::column(
                  width = 4, 
                  align = "center",
                  shiny::uiOutput(outputId = ns("ld_well_plate_names"))
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 12, 
                  shiny::fluidRow(
                    hs(width = 3, 
                       shiny::actionButton(
                         inputId = ns("save_input_file"), 
                         label = "Save & Proceed"
                       )
                    )
                  )
                )
              ),
              solidHeader = TRUE,
              status = "success",
              title = "Step 1: Load file and denote experiment design variables",
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
    )
  )
  
}