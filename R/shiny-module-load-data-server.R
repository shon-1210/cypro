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
      
      pdl_step_2_info_assembled <- shiny::reactiveVal(value = list())
      pdl_step_3_info_assembled <- shiny::reactiveVal(value = list())
      pdl_step_4_info_assembled <- shiny::reactiveVal(value = list())
      
      used_identifier_names <- shiny::reactiveVal(value = NULL)
      used_analysis_module_names <- shiny::reactiveVal(value = NULL)
      used_additional_names <- shiny::reactiveVal(value = NULL)
      
      used_names_all <- shiny::reactiveVal(value = NULL)
      
      html_and_css <- shiny::reactiveValues(
        
        identifier_variables = NULL,
        analysis_module_variables = NULL, 
        additional_variables = NULL
        
      )
      
      final_df <- shiny::reactiveVal(value = data.frame())
      
      pdl_step_2_complete <- shiny::reactiveVal(value = FALSE)
      pdl_step_3_complete <- shiny::reactiveVal(value = FALSE)
      pdl_step_4_complete <- shiny::reactiveVal(value = FALSE)
      
      well_plate_list <- shiny::reactiveVal(value = object@well_plates)
      well_plate_names <- shiny::reactiveVal(value = base::names(object@well_plates))
      
      read_in_data <- shiny::reactiveVal(value = list())
      
      # - the return value of this module
      ld_output <- shiny::reactiveValues(
        
        track_list = list(),
        well_plate_list = list(),
        proceed = numeric()
        
      )
      
      # -----
      
      # Render UIs --------------------------------------------------------------
      
      # prepare data loading ---
      output$identifier_variables <- shiny::renderUI({
        
        shiny::req(html_and_css$identifier_variables)
        
        ns <- session$ns 
        
        if(shiny::isTruthy(example_df())){
          
          status <- "success"
          
        } else {
          
          status <- "warning"
          
        }
        
        shinydashboard::box(
          html_and_css$identifier_variables,
          solidHeader = TRUE,
          width = 12,
          status = status, 
          title = "Step 2: Denote identifier variables"
          )
        
      })
      
      output$module_variables <- shiny::renderUI({
        
        shiny::req(html_and_css$analysis_module_variables)
        
        ns <- session$ns
        
        if(base::is.character(used_identifier_names()) &
           base::isTRUE(pdl_step_2_complete())){
          
          status <- "success"
          
        } else {
          
          status <- "warning"
          
        }
        
        shinydashboard::box(
          html_and_css$analysis_module_variables,
          solidHeader = TRUE,
          width = 12,
          status = status,
          title = "Step 3: Denote module specific variables"
        )
        
      })
      
      output$additional_variables <- shiny::renderUI({
        
        shiny::req(html_and_css$additional_variables)
        
        ns <- session$ns
        
        if(base::is.character(used_analysis_module_names()) &
           base::isTRUE(pdl_step_3_complete())){
          
          status <- "success"
          
        } else {
          
          status <- "warning"
          
        }
        
        shinydashboard::box(
          html_and_css$additional_variables,
          solidHeader = TRUE,
          width = 12,
          status = status,
          title = "Step 4: Denote additional data variables"
        )
        
      })
      
      # ---
      
      # load data ---
      
      output$ld_well_plate_box <- shiny::renderUI({
        
        ns <- session$ns
        
        if(base::isTRUE(pdl_step_4_complete())){
          
          status <- "success"
          
        } else {
          
          status <- "warning"
          
        }
        
        shinydashboard::box(
          shiny::fluidRow(
            shiny::column(
              shiny::fluidRow(
                shiny::column(
                  shiny::helpText("Assign folders to well plates and check file availability.") %>% 
                    add_helper(content = helper_content$assign_folder, size = "m"),
                  width = 12, 
                )
              ), 
              shiny::fluidRow(
                shiny::column(
                  shiny::plotOutput(outputId = ns("ld_well_plate_plot")),
                  shiny::textOutput(outputId = ns("ld_chosen_dir")),
                  width = 12, 
                ),
              ), 
              shiny::fluidRow(
                hs(4, 
                   shiny::h5(shiny::strong("Choose Well Plate:")),
                   shiny::uiOutput(outputId = ns("ld_added_well_plates"))
                ),
                hs(4, 
                   shiny::h5(shiny::strong("If ambiguous:")) %>% 
                     add_helper(content = helper_content$if_ambiguous),
                   shiny::selectInput(
                     inputId = ns("ld_keep_filetype"), 
                     label = NULL, 
                     choices = c( "keep .csv - files" = "csv$",
                                  "keep .xls - files" = "xls$",
                                  "keep .xlsx - files" = "xlsx$"), 
                     selected = "csv$")
                ),
                hs(2, 
                   shiny::h5(shiny::strong("Include Subfolders")),
                   shinyWidgets::materialSwitch(
                     inputId = ns("ld_recursive"), 
                     label = NULL, 
                     value = TRUE,
                     status = "success")
                ),
                hs(2, 
                   shiny::h5(shiny::strong("Assign Folder:")),
                   shinyFiles::shinyDirButton(
                     id = ns("ld_well_plate_dir"), 
                     label = "Browse", 
                     title = NULL)
                )
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
      
      
      output$ld_added_well_plates <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::tagList(
          shiny::selectInput(inputId = ns("ld_added_well_plates"), 
                             label = NULL, 
                             choices = well_plate_names())
        )
        
      })
      
      output$ld_well_plate_errors <- shiny::renderUI({
        
        shiny::validate(
          shiny::need(
            expr = base::is.list(read_in_data()) & base::length(read_in_data()) != 0, 
            message = "No folders have been loaded yet."
          )
        )
        
        shiny::req(well_plates_with_errors())
        
        ns <- session$ns
        
        shinyWidgets::pickerInput(
          inputId = ns("ld_well_plate_errors"), 
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
      
      output$ld_well_image_errors <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::req(well_plates_with_errors())
        
        shinyWidgets::pickerInput(
          inputId = ns("ld_well_image_errors"), 
          label = "Failed Files:", 
          choices = well_images_with_errors())
        
      })
      
      
      output$ld_loading_box <- shiny::renderUI({
        
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
              DT::dataTableOutput(outputId = ns("ld_loading_status")),
              width = 12, 
            )
          ),
          shiny::HTML("<br><br>"), 
          shiny::fluidRow(
            shiny::column(
               shiny::actionButton(inputId = ns("ld_load_data"), label = "Load Data"),
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
      
      
      output$ld_save_and_proceed_box <- shiny::renderUI({
        
        ns <- session$ns
        
        if(base::is.list(read_in_data()) & base::length(read_in_data()) != 0){
          
          status <- "success"
          
        } else {
          
          status <- "warning"
          
        }
        
        
        shinydashboard::box(
          shiny::helpText("Check for errors during the loading process.") %>%
            add_helper(content = helper_content$load_files_and_proceed, size = "m"),
          shiny::uiOutput(outputId = ns("ld_well_plate_errors")),
          shiny::uiOutput(outputId = ns("ld_well_image_errors")),
          shiny::textOutput(outputId = ns("ld_error_message")),
          shiny::HTML("<br>"),
          shiny::column(
            width = 12,
            align = "center",
            shiny::splitLayout(
              cellWidths = c("50%", "50%"),
              shiny::actionButton(inputId = ns("ld_save_and_proceed"), label = "Save & Proceed")
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

      # events that change html
      
      oe <- shiny::observeEvent(example_df(), {
        
        ns <- session$ns
        
        html_and_css$identifier_variables <- 
          pdl_step_2_identifier_variables_html(
            object = object, 
            example_df = example_df(),
            ns = ns
          )
        
      })
      
      oe <- shiny::observeEvent(c(example_df(), used_identifier_names()), {
        
        ns <- session$ns
        
        html_and_css$analysis_module_variables <- 
          pdl_step_3_module_variables_html(
            object = object, 
            example_df = example_df(), 
            ns = ns,
            exclude_from_choices = used_identifier_names()
          )
        
      })
      
      oe <- shiny::observeEvent(c(example_df(), used_analysis_module_names()), {
        
        ns <- session$ns
        
        exclude_from_choices <- 
          c(used_identifier_names(), used_analysis_module_names())
        
        html_and_css$additional_variables <- 
          pdl_step_4_additional_variables_html(
            object = object, 
            example_df = example_df(),
            ns = ns,
            exclude_from_choices = exclude_from_choices
          )
        
      })
      
      # assemble module and variable assignment information

      oe <- shiny::observeEvent(input$pdl_id_vars_save, {
        
        checkpoint(
          evaluate = shiny::isTruthy(example_df()), 
          case_false = "no_example_df"
        )
        
        res_info <- 
          assemble_id_module_info_shiny(
            input_list = shiny::reactiveValuesToList(input), 
            object = object, 
            example_df = example_df()
          )
        
        used_id_names <- 
          used_variable_names_shiny(
            assembled_module_info = res_info,
            test_overlap = TRUE
          )
        
        if(base::length(used_id_names) >= 1){
        
          shiny_fdb(ui = "Identification variables saved.")  
          
        }
        
        pdl_step_2_info_assembled(res_info)
        used_identifier_names(used_id_names)
        
        # reset 
        used_analysis_module_names(NULL)
        used_additional_names(NULL)
        
        pdl_step_2_complete(TRUE)
        pdl_step_3_complete(FALSE)
        pdl_step_4_complete(FALSE)
        
      })
      
      oe <- shiny::observeEvent(input$pdl_module_vars_save, {
        
        checkpoint(
          evaluate = shiny::isTruthy(used_identifier_names()), 
          case_false = "no_id_vars"
        )
        
        res_info <- 
          assemble_analysis_module_info_shiny(
            input_list = shiny::reactiveValuesToList(input), 
            object = object
          )
        
        used_mod_names <- 
          used_variable_names_shiny(
            assembled_module_info = res_info, 
            test_overlap = TRUE
          )
        
        if(base::length(used_mod_names) >= 1){
          
          shiny_fdb(TRUE, ui = "Module variables saved.")
          
        }
        
        pdl_step_3_info_assembled(res_info)
        used_analysis_module_names(used_mod_names)
        
        # reset
        used_additional_names(NULL)
        
        pdl_step_2_complete(TRUE)
        pdl_step_3_complete(TRUE)
        pdl_step_4_complete(FALSE)
        
      })
      
      oe <- shiny::observeEvent(input$pdl_add_vars_save, {
        
        ns <- session$ns
        
        checkpoint(
          evaluate = base::is.character(used_analysis_module_names()), # can be character(0) -> no modules used
          case_false = "no_module_vars"
        )
        
        res_info <- 
          assemble_additional_data_variables_module_info_shiny(
            input_list = shiny::reactiveValuesToList(input), 
            object = object
          )
        
        used_add_names <-
          used_variable_names_shiny(
            assembled_module_info = res_info, 
            test_overlap = TRUE
          )
        
        if(base::length(used_add_names) >= 1){
        
          shiny_fdb(TRUE, ui = "Additional variables saved.")  
          
        }
        
        pdl_step_4_info_assembled(res_info)
        used_additional_names(used_add_names)
        
        all_denoted_names <- 
          c(used_identifier_names(),
            used_analysis_module_names(),
            used_additional_names()
            )
        
        # set value for reactive used_names_all()
        used_names_all(all_denoted_names)
        
        # set value for reactive final_df()
        final_df(dplyr::select(example_df(), dplyr::all_of(all_denoted_names)))
        
        # shiny helper 
        shiny::showModal(
          ui = shiny::modalDialog(
            shiny::tagList(
              shiny::fluidRow(
                shiny::column(
                  shiny::h4(shiny::strong("Final data output")), 
                  shiny::helpText("Cypro expects all files to contain these data variables."),
                  DT::dataTableOutput(outputId = ns("pdl_final_df")),
                  width = 12
                )
              )
            )
          ,
          footer = shiny::tagList(
              shiny::fluidRow(
                hs(width = 4, shiny::actionButton(inputId = ns("pdl_save_and_proceed"), label = "Proceed with data loading")), 
                hs(width = 4, shiny::actionButton(inputId = ns("pdl_change_vardenotation"), label = "Change variable denotation"))
              )
            ), 
            size = "l", 
            easyClose = FALSE
          )
        )
        
      })
      
      
      oe <- shiny::observeEvent(input$pdl_save_and_proceed, {
        
        shiny::removeModal()
        
        pdl_step_2_complete(TRUE)
        pdl_step_3_complete(TRUE)
        pdl_step_4_complete(TRUE)
        
        shiny_fdb(
          ui = "Preparation for data loading completed. Proceed by assigning each well plate its data folder.",
          duration = 15
        )
        
      })
      
      oe <- shiny::observeEvent(input$pdl_change_vardenotation, {
        
        shiny::removeModal()
        
        pdl_step_2_complete(TRUE)
        pdl_step_3_complete(TRUE)
        pdl_step_4_complete(FALSE)
        
        shiny_fdb(
          ui = "If you are done denoting data variables. Click again on 'Save and Proceed' of Step 4.",
          duration = 15
        )
        
      })
      
      # add new directory to well plate
      oe <- shiny::observeEvent(well_plate_dir_string(), {
        
        checkpoint(evaluate = base::isTRUE(pdl_step_4_complete()),
                   case_false = "incomplete_vardenotation")
        
        # actual code !!!
        checkpoint(evaluate = !base::is.null(well_plate_list()),
                   case_false = "no_set_up_saved")
        
        well_plate_list_new <- well_plate_list()
        wp_name <- input$ld_added_well_plates
        
        well_plate_list_new[[wp_name]][["directory"]] <- well_plate_dir_string()
        
        well_plate_list_new[[wp_name]] <-
          evaluate_file_availability_shiny(
            wp_list = well_plate_list_new[[wp_name]],
            recursive = input$ld_recursive, 
            keep = input$ld_keep_filetype
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
      oe <- shiny::observeEvent(input$ld_load_data,{
        
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
      oe <- shiny::observeEvent(input$ld_save_and_proceed, {
        
        checkpoint(evaluate = base::is.list(read_in_data()) & base::length(read_in_data()) != 0,
                   case_false = "no_data_read_in")
        
        ld_output$well_plate_list <- well_plate_list()
        ld_output$track_list <- track_list()
        
        object@well_plates <- well_plate_list()
        
        object@cdata$tracks <- track_list()
        
        object@information$all_cell_ids <- 
          purrr::map(.x = track_list(), .f = ~ dplyr::pull(.x, var = "cell_id")) %>% 
          purrr::flatten_chr() %>% 
          base::unique()
        
        ld_output$proceed <- input$ld_save_and_proceed
        
        object <-
          add_vardenotation_to_cypro_object_shiny(
            object = object, 
            amils = amils()
          )
        
        ld_output$object <- object
        
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
        
        shinyFiles::shinyDirChoose(
          input = input, 
          id = "pdl_example_dir", 
          session = session, 
          roots = dir_roots()
        )
        
        shinyFiles::shinyDirChoose(
          input = input, 
          id = "ld_well_plate_dir", 
          session = session, 
          roots = dir_roots()
        )
        
      } else {
        
        shinyFiles::shinyDirChoose(
          input = input, 
          id = "pdl_example_dir", 
          session = session, 
          roots = dir_roots
        )
        
        shinyFiles::shinyDirChoose(
          input = input, 
          id = "ld_well_plate_dir", 
          session = session, 
          roots = dir_roots, 
          restrictions = base::system.file(package = "base")
        )
        
      }
      
      # assembled directory 
      well_plate_dir_string <- shiny::reactive({ 
        
        shiny::validate(
          shiny::need(
            expr = base::is.list(x = input$ld_well_plate_dir),
            message = "No folder chosen."
            )
        )
        
        if(sysname == "Windows"){
          
          hlpr_assemble_directory(input_list = input$ld_well_plate_dir)
          
        } else {
          
          shinyFiles::parseDirPath(roots = c(wd = "~"), input$ld_well_plate_dir)
          
        }
        
      })
      
      # ---
      
      # --- prepare data loading 
      
      # example data.frame 
      example_df <- shiny::reactive({
        
        shiny::req(input$pdl_example_dir)
        
        input_file <- input$pdl_example_dir
        directory <- input_file$datapath
        
        read_example_file_shiny(directory = directory)
        
      })
      
      # assembled module information lists (amils)
      amils <- shiny::reactive({
        
        list(
          identifier = pdl_step_2_info_assembled(),
          analysis_modules = pdl_step_3_info_assembled(),
          additional = pdl_step_4_info_assembled()
        )
        
      })
      
      # ---
      
      # current well plate
      current_well_plate <- shiny::reactive({
        
        shiny::req(input$ld_added_well_plates)
        
        well_plate_list()[[input$ld_added_well_plates]]
        
      })
      
      # current, evaluated well-plate data.frame ready to be plotted
      evaluated_wp_df <- shiny::reactive({
        
        shiny::req(input$ld_added_well_plates)
        
        shiny::validate(
          shiny::need(
            expr = base::is.data.frame(current_well_plate()[["wp_df_eval"]]), 
            message = "No folder has been chosen for this well plate.")
        )
        
        current_well_plate()[["wp_df_eval"]]
        
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
        
        plot_well_plate_shiny(
          wp_df = evaluated_wp_df(), 
          selected_wells_df = NULL, 
          aes_fill = "availability_status", 
          aes_color = "availability_status", 
          fill_guide = TRUE,
          fill_values = ggplot2::alpha(status_colors, .5),
          color_values = ggplot2::alpha(status_colors, .5)
        ) + 
          ggplot2::labs(fill = "File Availability") + 
          ggplot2::guides(color = FALSE)
        
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
        
        shiny::req(input$ld_well_plate_errors)
        
        base::names(failed_list()[[input$ld_well_plate_errors]])
        
      })
      
      well_image_error_message <- shiny::reactive({
        
        shiny::req(input$ld_well_image_errors)
        
        failed_list()[[input$ld_well_plate_errors]][[input$ld_well_image_errors]] %>% 
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
      output$ld_well_plate_plot <- shiny::renderPlot({
        
        well_plate_plot()
        
      })
      
      # -----
      
      # Text outputs ------------------------------------------------------------
      
      output$ld_all_missing_files <- shiny::renderText({
        
        shiny::validate(
          shiny::need(
            expr = !base::is.null(current_well_plate()[["missing_files"]]), 
            message = "No folder has been chosen for this well plate."
          )
        )
        
        shiny::validate(
          shiny::need(
            expr = base::length(current_well_plate()[["missing_files"]]) == 0, 
            message = "No missing files."
          )
        )
        
        stringr::str_c(current_well_plate()[["missing_files"]], collapse = ", ")
        
      })
      
      output$ld_chosen_dir <- shiny::renderText({
        
        shiny::req(input$ld_added_well_plates)
        shiny::req(current_well_plate()[["directory"]])
        
        current_well_plate()[["directory"]]
        
      })
      
      output$ld_error_message <- shiny::renderText({
        
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
      
      
      output$ld_ambiguous_directories <- DT::renderDataTable({
          
          shiny::validate(
            shiny::need(
              expr = current_well_plate()[["ambiguous_directories"]], 
              message = "No folder has been chosen for this well plate."
            )
          )
          
          shiny::validate(
            shiny::need(
              expr = !base::identical(current_well_plate()[["ambiguous_directories"]], base::data.frame()), 
              message = "No ambiguous directories detected."
            )
          )
          
          # print output
          current_well_plate()[["ambiguous_directories"]]
          
        })
      
      output$ld_loading_status <- DT::renderDataTable({
        
        loading_status()
        
      }, options = list(scrollX = TRUE))
      
      # Module return value -----------------------------------------------------
      
      base::return(ld_output)
      
    })
  
}