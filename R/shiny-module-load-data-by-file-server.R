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

moduleLoadDataFileServer <- function(id, object){
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){
      
      options(shiny.maxRequestSize=500*1024^2)
      
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
      well_plate_list_eval <- shiny::reactiveVal(value = list())
      well_plate_names <- shiny::reactiveVal(value = base::names(object@well_plates))
      
      wp_var <- shiny::reactiveVal(value = NULL)
      well_var <- shiny::reactiveVal(value = NULL)
      roi_var <- shiny::reactiveVal(value = NULL)
      
      
      
      ed_vars <- shiny::reactiveVal(value = list())
      ed_vars_vec <- shiny::reactiveVal(value = character())
      ed_df <- shiny::reactiveVal(value = NULL)
      
      clean_df <- shiny::reactiveVal(value = data.frame())
      input_df_checked <- shiny::reactiveVal(value = NULL)
      
      read_in_data <- shiny::reactiveVal(value = list())
      
      # - the return value of this module
      ld_output <- shiny::reactiveValues(
        
        track_list = list(),
        well_plate_list = list(),
        proceed = numeric()
        
      )
      
      # -----
      
      # Render UIs --------------------------------------------------------------
      
      output$ld_var_names <- shiny::renderUI({
        
        shiny::req(input_df())
        
        ns <- session$ns
 
        var_name_options <-
          utils::head(input_df(), 6) %>% 
          base::colnames()
        
        shiny::fluidRow(
          hs(width = 4,
             shiny::selectInput(
               inputId = ns("well_plate_var"), 
               label = "Well Plate",
               choices = c("none", var_name_options), 
             ) %>% 
               add_helper(content = helper_content$well_plate_var)
            ), 
          hs(width = 4, 
             shiny::selectInput(
               inputId = ns("well_var"), 
               label = "Well", 
               choices = var_name_options
             ) %>% 
               add_helper(content = helper_content$well_var)
             ),
          hs(width = 4, 
             shiny::selectInput(
               inputId = ns("roi_var"), 
               label = "Region of Interest", 
               choices = var_name_options
             ) %>% 
               add_helper(content = helper_content$roi_var)
             )
        )
        
      })
      
      output$ld_well_plate_names <- shiny::renderUI({
        
        ns <- session$ns 
        
        shiny::req(!base::identical(well_plate_list_eval(), list()))
        
        wp_names <- well_plate_list_eval() %>% base::names()
        
        shiny::selectInput(
          inputId = ns("ld_well_plate_names"), 
          label = "Well Plate",
          width = "50%",
          choices = wp_names
        )
        
      })
      
      output$identifier_variables <- shiny::renderUI({
        
        shiny::req(html_and_css$identifier_variables)
        
        ns <- session$ns 
        
        if(shiny::isTruthy(input_df_checked())){
          
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
      
      # -----
      
      # Observe events ----------------------------------------------------------
      
      oe <- shiny::observeEvent(input$save_input_file, {
        
        checkpoint(
          evaluate = shiny::isTruthy(input_df()), 
          case_false = "no_input_file"
        )
        
        # transfer
        wp_var <- input$well_plate_var
        well_var <- input$well_var
        roi_var <- input$roi_var
        
        # save as reactive values 
        wp_var(wp_var)
        well_var(well_var)
        roi_var(roi_var)
        
        n_vars <- dplyr::n_distinct(c(wp_var, well_var, roi_var))
        
        if(n_vars != 3){
          
          msg <- "Please choose a different variable for each of the three options."
          
          shiny_fdb(ui = msg, type = "error")
          
          shiny::req(FALSE)
          
        }
        
        ed_vars <- list()
        
        ed_vars$wp <- wp_var
        ed_vars$well <- well_var
        ed_vars$roi <- roi_var
        
        ed_vars_vec <- c(wp_var, well_var, roi_var)
        
        input_file <- input$pdl_example_dir
        dir <- input_file$datapath
        
        out_list <- 
          evaluate_file_content_shiny(
            var_name_well_plate = wp_var, 
            var_name_well = well_var, 
            var_name_roi = roi_var, 
            wp_list = well_plate_list(), 
            df = input_df(), 
            dir = dir
          )
        
        wp_list_new <- out_list$wp_list_new
        
        clean_df(out_list$df_clean)
        
        print(clean_df())
        
        input_df_checked <- dplyr::select(input_df(), -dplyr::any_of(ed_vars_vec))
        
        assign("input_df_checked", input_df_checked, envir = .GlobalEnv)
        
        # save as reactive values
        ed_vars(ed_vars)
        ed_vars_vec(ed_vars_vec)
        well_plate_list_eval(wp_list_new)
        input_df_checked(input_df_checked)
        
      })
      
      # events that change html
      oe <- shiny::observeEvent(input_df_checked(), {
        
        ns <- session$ns
        
        html_and_css$identifier_variables <- 
          pdl_step_2_identifier_variables_html(
            object = object, 
            example_df = input_df_checked(),
            ns = ns
          )
        
      })
      
      oe <- shiny::observeEvent(c(input_df_checked(), used_identifier_names()), {
        
        ns <- session$ns
        
        html_and_css$analysis_module_variables <- 
          pdl_step_3_module_variables_html(
            object = object, 
            example_df = input_df_checked(), 
            ns = ns,
            exclude_from_choices = used_identifier_names()
          )
        
      })
      
      oe <- shiny::observeEvent(c(input_df_checked(), used_analysis_module_names()), {
        
        ns <- session$ns
        
        exclude_from_choices <- 
          c(used_identifier_names(), used_analysis_module_names())
        
        html_and_css$additional_variables <- 
          pdl_step_4_additional_variables_html(
            object = object, 
            example_df = input_df_checked(),
            ns = ns,
            exclude_from_choices = exclude_from_choices
          )
        
      })
      
      # assemble module and variable assignment information
      
      oe <- shiny::observeEvent(input$pdl_id_vars_save, {
        
        checkpoint(
          evaluate = shiny::isTruthy(input_df_checked()), 
          case_false = "no_example_df"
        )
        
        res_info <- 
          assemble_id_module_info_shiny(
            input_list = shiny::reactiveValuesToList(input), 
            object = object, 
            ed_vars = ed_vars_vec(),
            example_df = input_df()
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
        
        print(all_denoted_names)
        
        assign("amils", amils(), envir = .GlobalEnv)
        
        # set value for reactive used_names_all()
        used_names_all(all_denoted_names)
        
        # set value for reactive final_df()
        final_df(dplyr::select(input_df_checked(), dplyr::all_of(all_denoted_names)))
        
        ld_output$well_plate_list <- well_plate_list_eval()
        ld_output$track_list <- track_list()
        
        object@well_plates <- well_plate_list()
        
        object@cdata$tracks <- track_list()
        
        object@information$all_cell_ids <- 
          purrr::map(.x = track_list(), .f = ~ dplyr::pull(.x, var = "cell_id")) %>% 
          purrr::flatten_chr() %>% 
          base::unique()
        
        ld_output$proceed <- TRUE
        
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

      } else {
        
        shinyFiles::shinyDirChoose(
          input = input, 
          id = "pdl_example_dir", 
          session = session, 
          roots = dir_roots
        )

      }
 
      
      # ---
      
      # --- prepare data loading 
      
      # example data.frame 
      input_df <- shiny::reactive({
        
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
      
      
      # assemble results lists depending on data input type
      
      track_list <- shiny::reactive({
        
        if(isTimeLapseExp(object)){
          
          track_list <-
            join_well_plate_data_and_input_timelapse_shiny(
              input_df = clean_df(), 
              well_plate_list = well_plate_list_eval(), 
              module_vars = used_names_all(), 
              ed_vars = ed_vars(), 
              amils = amils(), 
              object = object
            )
          
        } else {
          
          track_list <-
            join_well_plate_data_and_input_one_time_imaging_shiny(
              input_df = clean_df(), 
              well_plate_list = well_plate_list_eval(), 
              module_vars = used_names_all(), 
              ed_vars = ed_vars(), 
              amils = amils(), 
              object = object
            )
          
        }
        
        base::return(track_list)
        
      })
      
      # -----
      
      # Plot outputs ------------------------------------------------------------
      
      output$ld_well_plate_plot <- shiny::renderPlot({
        
        shiny::req(!base::identical(well_plate_list_eval(), list()))
        
        wp_list <- well_plate_list_eval()[[input$ld_well_plate_names]]
        
        plot_well_plate_shiny(
          wp_df = wp_list$wp_df_eval, 
          aes_fill = "availability_status",
          aes_color = "information_status",
          fill_values = status_colors, 
          fill_guide = TRUE,
          color_values = status_colors, 
          color_guide = FALSE, 
        ) + 
          ggplot2::labs(fill = "Data Availability")
        
      })
      
      
      # -----
      
      # -----
      
      
      # Table outputs -----------------------------------------------------------
      
      output$pdl_example_table <- DT::renderDataTable({
        
        shiny::validate(
          shiny::need(
            expr = input_df(), 
            message = "No  data has been loaded yet."
          )
        )
        
        utils::head(input_df())
        
      }, options = list(scrollX = TRUE))
      
      
      output$pdl_final_df <- DT::renderDataTable({
        
        shiny::req(final_df())
        
        utils::head(final_df())
        
      }, options = list(scrollX = TRUE))
      
      # Module return value -----------------------------------------------------
      
      base::return(ld_output)
      
    })
  
}