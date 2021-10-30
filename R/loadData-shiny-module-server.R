#' @title Server logic: Load Data
#' 
#' @description Server logic to be used as input for \code{module}-argument
#' of function \code{shiny::moduleServer()}.
#'
#' @param id Namespace ID
#' @param ed_input A reactive and named list. See value of \code{moduleExperimentSetUpServer()}.
#'
#' @return A named list: 
#' 
#' @export

moduleLoadDataServer <- function(id, object){
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){
      
      
      # Reactive values ---------------------------------------------------------
      
      well_plate_list <- shiny::reactiveVal(value = getWellPlates(object))
      well_plate_names <- shiny::reactiveVal(value = getWellPlateNames(object))
      
      read_in_data <- shiny::reactiveVal(value = list())
      
      # - the return value of this module
      ld_output <- shiny::reactiveValues(
        
        track_list = list(),
        well_plate_list = list(),
        proceed = numeric()
        
      )
      
      # -----
      
      # Render UIs --------------------------------------------------------------
      
      output$well_plate_box <- shiny::renderUI({
        
        ns <- session$ns
        
        status <- "success"
        
        shinydashboard::box(
          shiny::fluidRow(
            shiny::column(
              shiny::fluidRow(
                shiny::column(
                  shiny::helpText("Assign folders or files to well plates.") %>% 
                    add_helper(content = helper_content$assign_folder, size = "m"),
                  width = 12, 
                )
              ), 
              shiny::fluidRow(
                shiny::column(
                  shiny::plotOutput(outputId = ns("well_plate_plot")),
                  shiny::textOutput(outputId = ns("chosen_dir")),
                  width = 12, 
                ),
              ), 
              shiny::fluidRow(
                  shiny::uiOutput(outputId = ns("loading_modality"))
              ),
              width = 12,
            )
          ),
          solidHeader = TRUE,
          status = status,
          title = "Assign Folder to Well Plate", 
          width = 12
        )
        
      })
      
      output$selected_well_plate <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::tagList(
          shiny::selectInput(
            inputId = ns("selected_well_plate"),
            label = NULL, 
            choices = well_plate_names()
            )
        )
        
      })
      
      output$loading_modality <- shiny::renderUI({
        
        ns <- session$ns
        
        if(byFolder(object)){
          
          dir_input_html <- 
            shiny::tagList(
              
              shiny::column( 
                 shiny::h5(shiny::strong("Include Subfolders:")),
                 shinyWidgets::materialSwitch(
                   inputId = ns("recursive"), 
                   label = NULL, 
                   value = TRUE,
                   status = "success"),
                 width = 2,
              ),
              shiny::column(
                shiny::h5(shiny::strong("Valid filetypes:")), 
                shinyWidgets::checkboxGroupButtons(
                  inputId = ns("valid_filetypes"),
                  label = NULL, 
                  choices = filetypes_named,
                  selected = filetypes
                ), 
                width = 4
              ),
              shiny::column(
                 shiny::h5(shiny::strong("Select folder:")),
                 shinyFiles::shinyDirButton(
                   id = ns("input_dir"), 
                   label = "Browse", 
                   title = NULL), 
                 width = 2
              )
              
            )
            
        } else if(byWellPlate(object)){
          
          dir_input_html <- 
            shiny::tagList(
              
              hs(4, 
                 shiny::h5(shiny::strong("Select file:")),
                 shinyFiles::shinyFilesButton(
                   id = ns("input_dir"), 
                   label = "Browse", 
                   title = NULL, 
                   multiple = FALSE
                 ))
              
            )
          
        }
        
        out_html <- 
          shiny::tagList(
            hs(3, 
               shiny::h5(shiny::strong("Choose Well Plate:")),
               shiny::uiOutput(outputId = ns("selected_well_plate"))
            ), 
            hs(3,
               shiny::h5(shiny::strong("Color by:")),
               shiny::selectInput(
                 inputId = ns("clr_by"), 
                 label = NULL, 
                 choices = clr_by_well_plate_choices, 
                 selected = "cell_line"
               )
               ),
            dir_input_html
          )
        
        return(out_html)
        
      })
      
      output$well_plate_errors <- shiny::renderUI({
        
        shiny::validate(
          shiny::need(
            expr = base::is.list(read_in_data()) & base::length(read_in_data()) != 0, 
            message = "No folders have been loaded yet."
          )
        )
        
        shiny::req(well_plates_with_errors())
        
        ns <- session$ns
        
        shinyWidgets::pickerInput(
          inputId = ns("well_plate_errors"), 
          label = "Well Plate:", 
          choices = well_plates_with_errors(), 
          choicesOpt = list(
            subtext = stringr::str_c(
              "Errors", 
              well_plate_error_count(), 
              sep = ": "
            )
          )
        )
        
      })
      
      output$well_image_errors <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::req(well_plates_with_errors())
        
        shinyWidgets::pickerInput(
          inputId = ns("well_image_errors"), 
          label = "Failed Files:", 
          choices = well_images_with_errors())
        
      })
      
      
      output$loading_box <- shiny::renderUI({
        
        ns <- session$ns 
        
        if(base::all(loading_status()[["Ready to load"]] == "Yes")){
          
          status <- "success"
          
        } else {
          
          status <- "warning"
          
        }
        
        shinydashboard::box(
          shiny::helpText("Check the progress you have made assigning folders to well plates.") %>%
            add_helper(content = helper_content$well_plate_status, size = "m"),
          shiny::fluidRow(
            shiny::column(
              DT::dataTableOutput(outputId = ns("loading_status")),
              width = 12, 
            )
          ),
          shiny::HTML("<br><br>"), 
          shiny::fluidRow(
            shiny::column(
              shiny::actionButton(inputId = ns("load_data"), label = "Load Data"),
              align = "center",
              width = 12
            )
          ),
          solidHeader = TRUE,
          status = status,
          title = "Well Plate Status",
          width = 12
        ) 
        
      })
      
      
      output$save_and_proceed_box <- shiny::renderUI({
        
        ns <- session$ns
        
        if(base::is.list(read_in_data()) & base::length(read_in_data()) != 0){
          
          status <- "success"
          
        } else {
          
          status <- "warning"
          
        }
        
        shinydashboard::box(
          shiny::helpText("Check for errors during the loading process.") %>%
            add_helper(content = helper_content$load_files_and_proceed, size = "m"),
          shiny::uiOutput(outputId = ns("well_plate_errors")),
          shiny::uiOutput(outputId = ns("well_image_errors")),
          shiny::textOutput(outputId = ns("error_message")),
          shiny::HTML("<br>"),
          shiny::column(
            width = 12,
            align = "center",
            shiny::splitLayout(
              cellWidths = c("50%", "50%"),
              shiny::actionButton(inputId = ns("save_and_proceed"), label = "Save & Proceed")
            )
          ),
          solidHeader = TRUE,
          status = status, 
          title = "Save & Proceed",
          width = 12
        )  
        
      })
      
      # ---
      
      
      # -----
      
      # Observe events ----------------------------------------------------------
      
      
      # add new directory to well plate
      oe <- shiny::observeEvent(input_dir_string(), {
        
        well_plate_list_new <- well_plate_list()
        wp_name <- input$selected_well_plate
        
        well_plate_list_new[[wp_name]][["directory"]] <- input_dir_string()
        
        well_plate_list_new[[wp_name]] <-
          evaluate_file_availability_shiny(
            wp_list = well_plate_list_new[[wp_name]],
            recursive = input$recursive, 
            keep = input$keep_filetype
          )
        
        check <- check_wp_directories(well_plate_list = well_plate_list_new)
        
        if(check != "unique"){
          
          message <-
            glue::glue("There are well plates that share their directories: '{check}'
                       Please assign an unambiguous directory to each well plate.")
          
          shiny::showNotification(
            ui = message, 
            type = "warning",
            duration = 20
          )
          
        }
        
        # update well_plate_list and -names
        well_plate_list(well_plate_list_new)
        well_plate_names(base::names(well_plate_list()))
        
      })
      
      # load data
      oe <- shiny::observeEvent(input$load_data,{
        
        checkpoint(evaluate = base::all(loading_status()[["Ready to load"]] == "Yes"),
                   case_false = "well_plates_not_ready")
        
        data_list <- 
          purrr::map2(.x = well_plate_list(),
                      .y = well_plate_names(),
                      .f = load_data_files_shiny, 
                      assembled_module_info_lists = amils(),
                      used_variable_names = used_names_all(),
                      mitosis_module_used = FALSE, # !!!
                      object = object, 
                      session = session)
        
        shiny::showNotification(ui = "Reading done.", type = "message")
        
        #assign("data_list", data_list, envir = .GlobalEnv)
        
        # update read_in_data
        read_in_data(data_list)
        
      })
      
      # save and proceed 
      oe <- shiny::observeEvent(input$save_and_proceed, {
        
        checkpoint(evaluate = base::is.list(read_in_data()) & base::length(read_in_data()) != 0,
                   case_false = "no_data_read_in")
        
        output$well_plate_list <- well_plate_list()
        output$track_list <- track_list()
        
        object@well_plates <- well_plate_list()
        
        object@cdata$tracks <- track_list()
        
        object@information$all_cell_ids <- 
          purrr::map(.x = track_list(), .f = ~ dplyr::pull(.x, var = "cell_id")) %>% 
          purrr::flatten_chr() %>% 
          base::unique()
        
        output$proceed <- input$save_and_proceed
        
        object <-
          add_vardenotation_to_cypro_object_shiny(
            object = object, 
            amils = amils()
          )
        
        output$object <- object
        
        shiny_fdb(in_shiny = TRUE, ui = "Results have been saved. Click on 'Return Cypro Object' and proceed with checkDataQuality().")
        
      })
      
      # ----- 
      
      # Reactive expressions ----------------------------------------------------
      
      # directory handling ---
      
      # shinyFiles::shinyDirButton() - server
      
      system_info <- base::Sys.info()
      sysname <- system_info["sysname"]
      
      if(sysname == "Windows"){
        
        dir_roots <- shinyFiles::getVolumes()
        
      } else {
        
        dir_roots <- c("wd" = "~")
        
      }
      
      if(sysname == "Windows"){
        
        if(byFolder(object)){
          
          shinyFiles::shinyDirChoose(
            input = input, 
            id = "input_dir", 
            session = session, 
            roots = dir_roots()
          )
          
        } else if(byWellPlate(object)){
          
          shinyFiles::shinyFileChoose(
            input = input, 
            id = "input_dir", 
            session = session, 
            roots = dir_roots()
          )
          
        }
        
      } else {
        
        if(byFolder(object)){
          
          shinyFiles::shinyDirChoose(
            input = input, 
            id = "input_dir", 
            session = session, 
            roots = dir_roots
          )
          
        } else if(byWellPlate(object)){
          
          shinyFiles::shinyDirChoose(
            input = input, 
            id = "input_dir", 
            session = session, 
            roots = dir_roots, 
            restrictions = base::system.file(package = "base")
          )
          
        }
        
      }
      
      
      
      # assembled directory 
      input_dir_string <- shiny::reactive({ 
        
        debug_assign(input$input_dir, as = "input_dir")
        
        shiny::validate(
          shiny::need(
            expr = base::is.list(x = input$input_dir),
            message = "No folder chosen."
          )
        )
        
        if(sysname == "Windows"){
          
          out <- flatten_folder_path(input_dir = input$input_dir)
          
        } else {
          
          out <- shinyFiles::parseDirPath(roots = c(wd = "~"), input$input_dir)
          
        }
        
        return(out)
        
      })
      
      # ---
      
      # current well plate
      well_plate <- shiny::reactive({
        
        shiny::req(input$selected_well_plate)
        
        well_plate_list()[[input$selected_well_plate]]
        
      })
      
      # current, evaluated well-plate data.frame ready to be plotted
      layout_df <- shiny::reactive({
        
        shiny::req(input$selected_well_plate)
        
        layout_df <- getLayoutDf(object = well_plate())
        
        print(layout_df)
        
        return(layout_df)
        
      })
      
      # loading status
      loading_status <- shiny::reactive({
        
        shiny::validate(
          shiny::need(
            expr = well_plate_names(), 
            message = "No well plates have been added yet."
          )
        )
        
        loading_status_table_shiny(well_plate_list = well_plate_list())
        
      })
      
      # well plate plot visualizes the file availability
      well_plate_plot <- shiny::reactive({
        
        plotWellPlate(
          object = layout_df(), 
          clr_by = input$clr_by, 
          plot_type = "well"
        )
        
      })
      
      # read in data error processing ---
      
      failed_list <- shiny::reactive({
        
        shiny::req(read_in_data())
        
        purrr::map(.x = read_in_data(), .f = ~ .x$failed)
        
      })
      
      well_plates_with_errors <- shiny::reactive({
        
        base::names(failed_list())
        
      })
      
      well_plate_error_count <- shiny::reactive({
        
        shiny::req(failed_list())
        
        purrr::map_int(.x = failed_list(), .f = base::length) %>% 
          base::unname()
        
      })
      
      well_images_with_errors <- shiny::reactive({
        
        shiny::req(input$well_plate_errors)
        
        base::names(failed_list()[[input$well_plate_errors]])
        
      })
      
      well_image_error_message <- shiny::reactive({
        
        shiny::req(input$well_image_errors)
        
        failed_list()[[input$well_plate_errors]][[input$well_image_errors]] %>% 
          base::as.character()
        
      })
      
      # ---
      
      # assemble results lists depending on data input type
      
      track_list <- shiny::reactive({
        
        if(isTimeLapseExp(object)){
          
          track_list <-
            assemble_tracks_time_lapse_shiny(
              track_data_list = read_in_data(),
              well_plate_list = well_plate_list(),
              object = object
            )
          
        } else {
          
          track_list <-
            assemble_tracks_one_time_imaging_shiny(
              stat_data_list = read_in_data(),
              well_plate_list = well_plate_list(),
              object = object
            )
          
        }
        
        #assign("track_list", track_list, envir = .GlobalEnv)
        
        base::return(track_list)
        
      })
      
      # -----
      
      # Plot outputs ------------------------------------------------------------
      
      # well plate 
      output$well_plate_plot <- shiny::renderPlot({
        
        well_plate_plot()
        
      })
      
      # -----
      
      # Text outputs ------------------------------------------------------------
      
      output$all_missing_files <- shiny::renderText({
        
        shiny::validate(
          shiny::need(
            expr = !base::is.null(well_plate()[["missing_files"]]), 
            message = "No folder has been chosen for this well plate."
          )
        )
        
        shiny::validate(
          shiny::need(
            expr = base::length(well_plate()[["missing_files"]]) == 0, 
            message = "No missing files."
          )
        )
        
        stringr::str_c(well_plate()[["missing_files"]], collapse = ", ")
        
      })
      
      output$chosen_dir <- shiny::renderText({
        
        shiny::req(input$selected_well_plate)
        #shiny::req(well_plate()[["directory"]])
        
        #well_plate()[["directory"]]
        
        "change this"
        
      })
      
      output$error_message <- shiny::renderText({
        
        shiny::req(well_plates_with_errors())
        
        well_image_error_message()
        
      })
      
      # -----
      
      
      # Table outputs -----------------------------------------------------------
      
      output$pdl_example_table <- DT::renderDataTable({
        
        shiny::validate(
          shiny::need(
            expr = example_df(), 
            message = "No example data has been loaded yet."
          )
        )
        
        utils::head(example_df())
        
      }, options = list(scrollX = TRUE))
      
      
      output$pdl_final_df <- DT::renderDataTable({
        
        shiny::req(final_df())
        
        utils::head(final_df())
        
      }, options = list(scrollX = TRUE))
      
      
      output$ambiguous_directories <- DT::renderDataTable({
        
        shiny::validate(
          shiny::need(
            expr = well_plate()[["ambiguous_directories"]], 
            message = "No folder has been chosen for this well plate."
          )
        )
        
        shiny::validate(
          shiny::need(
            expr = !base::identical(well_plate()[["ambiguous_directories"]], base::data.frame()), 
            message = "No ambiguous directories detected."
          )
        )
        
        # print output
        well_plate()[["ambiguous_directories"]]
        
      })
      
      output$loading_status <- DT::renderDataTable({
        
        loading_status()
        
      }, options = list(scrollX = TRUE))
      
      # Module return value -----------------------------------------------------
      
      base::return(output)
      
    })
  
}