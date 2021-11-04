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
      
      cypro_object <- shiny::reactiveVal(value = object)
      
      read_in_data <- shiny::reactiveVal(value = list())
      
      # - the return value of this module
      output_list <- shiny::reactiveValues(
        
        object = object, 
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
            hs(2, 
               shiny::h5(shiny::strong("Well Plate:")),
               shiny::uiOutput(outputId = ns("selected_well_plate"))
            ), 
            hs(2,
               shiny::h5(shiny::strong("Color by:")),
               shiny::selectInput(
                 inputId = ns("clr_by"), 
                 label = NULL, 
                 choices = clr_by_well_plate_choices, 
                 selected = "condition"
               )
               ),
            dir_input_html
          )
        
        return(out_html)
        
      })
      
      output$well_plate_errors <- shiny::renderUI({
        
        shiny::req(well_plates_with_errors())
        
        ns <- session$ns
        
        shinyWidgets::pickerInput(
          inputId = ns("well_plate_with_errors"), 
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
      
      output$file_with_errors <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::req(well_plates_with_errors())
        
        choices <- 
          purrr::set_names(
            x = files_with_errors(), 
            nm = stringr::str_extract(files_with_errors(), pattern = ".{5,30}$") %>% stringr::str_c("~ ...", .)
          )
        
        shinyWidgets::pickerInput(
          inputId = ns("file_with_errors"), 
          label = "Files with errors:", 
          choices = choices
          )
        
      })
      
      
      output$loading_box <- shiny::renderUI({
        
        ns <- session$ns 
        
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
          status = "success",
          title = "Well Plate Status",
          width = 12
        ) 
        
      })
      
      
      output$save_and_proceed_box <- shiny::renderUI({
        
        ns <- session$ns
        
        status <- 
          base::ifelse(
            test = containsData(cypro_object()),
            yes = "success",
            no = "warning"
            )
        
        shinydashboard::box(
          shiny::helpText("Check for errors during the loading process.") %>%
            add_helper(content = helper_content$load_files_and_proceed, size = "m"),
          shiny::uiOutput(outputId = ns("well_plate_errors")),
          shiny::uiOutput(outputId = ns("file_with_errors")),
          shiny::verbatimTextOutput(outputId = ns("error_message")),
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
        
        ns <- session$ns
        
        dirs <- 
          getWellPlateDirectories(cypro_object()) %>% 
          purrr::keep(.p = ~isOfLength(.x, l = 1) & !base::is.na(.x))
        
        object <- 
          prepareDataLoading(
            object = cypro_object(), 
            directory = input_dir_string(), 
            valid_filetypes = input$valid_filetypes,
            well_plate = input$selected_well_plate, 
            recursive = input$recursive,
            in_shiny = TRUE
          )
        
        cypro_object(object)

      })
      
      # load data
      oe <- shiny::observeEvent(input$load_data,{
        
        validate_no_overlap_directories(
          directories = getWellPlateDirectories(cypro_object()),
          fdb_if_false = TRUE, 
          fdb_fn = "stop", 
          in_shiny = TRUE
        )
        
        object <- 
          loadDataFiles(
            object = cypro_object(),
            in_shiny = TRUE, 
            check_additional = TRUE,
            check_vars = TRUE
            )
        
        debug_assign(object, "object_new")
        
        cypro_object(object)
        
        shiny::showNotification(ui = "Reading done.", type = "message")
        
      })
      
      # save and proceed 
      oe <- shiny::observeEvent(input$save_and_proceed, {
        
        output_list$proceed <- input$save_and_proceed
        
        output_list$object <- cypro_object()
        
        shiny_fdb(in_shiny = TRUE, ui = "Results have been saved. Click on 'Return Cypro Object'.")
        
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
      
      # well plates
      well_plate <- shiny::reactive({
        
        shiny::req(input$selected_well_plate)
        
        well_plate_list()[[input$selected_well_plate]]
        
      })
      
      well_plate_list <- shiny::reactive({
        
        getWellPlates(object = cypro_object())
        
      })
      
      well_plate_names <- shiny::reactive({
        
        getWellPlateNames(cypro_object())
        
      })
      
      # current, evaluated well-plate data.frame ready to be plotted
      layout_df <- shiny::reactive({
        
        shiny::req(input$selected_well_plate)
        
        layout_df <- 
          getLayoutDf(
            object = cypro_object(),
            well_plate = input$selected_well_plate
            )
        
        return(layout_df)
        
      })
      
      # loading status
      loading_status <- shiny::reactive({

        getLoadingStatusDf(object = cypro_object(), with_transferred = FALSE) %>% 
          make_pretty_df()
        
      })
      
      # well plate plot visualizes the file availability
      well_plate_plot <- shiny::reactive({
        
        shiny::req(layout_df())
        
        plotWellPlate(
          object = layout_df(), 
          clr_by = input$clr_by, 
          plot_type = "well"
        )
        
      })
      
      # read in data error processing ---
      
      error_list <- shiny::reactive({
        
        getErrors(object = cypro_object())
        
      })
      
      well_plates_with_errors <- shiny::reactive({
        
        base::names(error_list())
        
      })
      
      well_plate_error_count <- shiny::reactive({
        
        shiny::req(error_list())
        
        purrr::map_int(.x = error_list(), .f = base::length) %>% 
          base::unname()
        
      })
      
      files_with_errors <- shiny::reactive({
        
        shiny::req(input$well_plate_with_errors)
        
        base::names(error_list()[[input$well_plate_with_errors]])
        
      })
      
      file_error_messages <- shiny::reactive({
        
        shiny::req(input$file_with_errors)
        
        errors_occured <- 
          error_list()[[input$well_plate_with_errors]][[input$file_with_errors]]
        
        shiny::req(errors_occured)
        
        out <- 
          purrr::flatten_chr(errors_occured) %>% 
          stringr::str_c(collapse = "\n")
        
        return(out)
        
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
        
        getWellPlateDirectories(cypro_object(), well_plates = input$selected_well_plate) %>% 
          base::unname()
        
      })
      
      output$error_message <- shiny::renderText({
        
        shiny::validate(
          shiny::need(
            expr = containsData(cypro_object()), 
            message = "No data has been loaded yet."
          )
        )
        
        shiny::validate(
          shiny::need(
            expr = shiny::isTruthy(file_error_message()), 
            message = "No errors occured."
          )
        )
        
        file_error_messages()
        
      })
      
      # -----
      
      
      # Table outputs -----------------------------------------------------------

      
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
      
      return(output_list)
      
    })
  
}