
#' @title UI logic: Experiment Set Up
#' 

moduleExperimentDesignUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      blue_box(
        title = "Experiment Design",
        width = 12, 
        shiny::column(
          width = 12, 
          shiny::fluidRow(
            # overall information
            shiny::fluidRow( 
              shiny::column(
                width = 5, 
                shinydashboard::box(
                  shiny::fluidRow( 
                    shiny::column(
                      width = 12,
                      shiny::fluidRow(
                        shiny::column(
                          width = 4, 
                          shiny::textInput(
                            inputId = ns("exp_name"),
                            label = "Experiment Name:",
                            placeholder = "experiment name")
                        ), 
                        shiny::column(
                          width = 8, 
                          shinyWidgets::radioGroupButtons(
                            inputId = ns("exp_type"),
                            label = "Experiment Type:",
                            choices = c("Screening" = "CyproScreening", # change to "screening" 
                                        "Time lapse" = "CyproTimeLapse",
                                        "Time lapse (multiple phases)" = "CyproTimeLapseMP"
                            ),
                            checkIcon = list(
                              yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                              no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))
                          ) %>% add_helper(content = helper_content$experiment_type)
                        )
                      ),
                      shiny::fluidRow( 
                        shiny::column(
                          width = 4, 
                          shiny::actionButton(inputId = ns("overall_info_save"), label = "Save & Proceed")
                        )
                      )
                    )
                  ),
                  solidHeader = TRUE,
                  status = "success",
                  title = "1. Overall Information",
                  width = 12, 
                )
              ), 
              
              # measurements
              shiny::column(
                width = 3,
                shiny::uiOutput(outputId = ns("imaging_set_up_box"))
              ), 
              shiny::column(
                width = 4, 
                shiny::uiOutput(outputId = ns("experiment_phases_box"))
              )
            ),
            shiny::HTML("<br>"), 
            shiny::uiOutput(outputId = ns("well_plate_box")),
            shiny::HTML("<br>"),
            shiny::uiOutput(outputId = ns("save_and_proceed_box"))
            
          )
          
        )
      )
    )
  )
  
}
