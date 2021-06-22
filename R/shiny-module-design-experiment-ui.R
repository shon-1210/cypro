
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
                          shiny::h5(shiny::strong("Experiment Name:")),
                          shiny::textInput(
                            inputId = ns("ed_experiment_name"),
                                           placeholder = "experiment name", 
                                           label = NULL)
                        ), 
                        shiny::column(
                          width = 4, 
                          shiny::h5(shiny::strong("Storage Folder:")),
                          shinyFiles::shinyDirButton(
                            id = ns("ed_experiment_dir"), 
                                                     label = "Browse", 
                                                     title = NULL)
                        ), 
                        shiny::column(
                          width = 4, 
                          shiny::h5(shiny::strong("Storage Directory:")), 
                          shiny::textOutput(outputId = ns("ed_experiment_path")))
                      ),
                      shiny::fluidRow(
                        shiny::column(
                          width = 12, 
                          shinyWidgets::radioGroupButtons(
                            inputId = ns("ed_exp_type"),
                            label = "Experiment Type:",
                            choices = c("One Time Imaging" = "one_time_imaging", "Time lapse" = "time_lapse"),
                            checkIcon = list(
                              yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                              no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))
                          )
                        )
                      ),
                      shiny::fluidRow( 
                        shiny::column(
                          width = 4, 
                          shiny::h5(shiny::strong("Software:")),
                          shiny::selectInput(
                            inputId = ns("ed_software"),
                            label = NULL,
                            choices = c("   ", image_processing_softwares)
                          ), 
                          shiny::actionButton(inputId = ns("ed_overall_info_save"), label = "Save & Proceed")
                        ),
                        shiny::column(
                          width = 8, 
                          shiny::uiOutput(outputId = ns("ed_example")) 
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
                shiny::uiOutput(outputId = ns("ed_imaging_set_up_box"))
              ), 
              shiny::column(
                width = 4, 
                shiny::uiOutput(outputId = ns("ed_experiment_phases_box"))
              )
            ),
            shiny::HTML("<br>"), 
            shiny::uiOutput(outputId = ns("ed_well_plate_box")),
            shiny::HTML("<br>"),
            shiny::uiOutput(outputId = ns("ed_save_and_proceed_box"))
            
          )
          
        )
      )
    )
  )
  
}
