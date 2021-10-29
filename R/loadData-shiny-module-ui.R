#' @title UI logic: Load Data
#' 

moduleLoadDataUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      blue_box(
        title = "Load Data", width = 12, 
        shiny::fluidRow(
          shiny::column(
            align = "left",
            shiny::uiOutput(outputId = ns("well_plate_box")),
            width = 6
          ), 
          shiny::column(
            shiny::fluidRow(
              hs(12, shiny::uiOutput(outputId = ns("loading_box")))
            ), 
            shiny::fluidRow(
              hs(12,shiny::uiOutput(outputId = ns("save_and_proceed_box")))
            ),
            width = 6
          )
        )
      )
    )
  )
  
}