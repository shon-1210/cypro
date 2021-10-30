#' @title Server logic: Assign variables
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

moduleAssignVariablesServer <- function(id, object){
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){
      
      # Reactive values ---------------------------------------------------------
      
      cypro_object <- shiny::reactiveVal(value = object)
      
      used_analysis_variable_names <- shiny::reactiveVal(value = character(0))
      validation_feedback <- shiny::reactiveVal(value = character(0))
      
      html_and_css <- shiny::reactiveValues(
        
        analysis_module_variables = NULL, 
        additional_variables = NULL
        
      )
      
      final_df <- shiny::reactiveVal(value = data.frame())
      
      # - the return value of this module
      output_list <- shiny::reactiveValues(
        
        object = methods::new("Cypro"), # placeholder for the actual object
        proceed = FALSE
        
      )
      
      # -----
      
      # Render UIs --------------------------------------------------------------
      
      output$module_variables <- shiny::renderUI({
        
        shiny::req(html_and_css$analysis_module_variables)
        
        shinydashboard::box(
          html_and_css$analysis_module_variables,
          solidHeader = TRUE,
          width = 12,
          status = "success",
          title = "Step 2: Denote module specific variables"
        )
        
      })
      
      output$additional_variables <- shiny::renderUI({
        
        shiny::req(html_and_css$additional_variables)
        
        ns <- session$ns
        
        if(shiny::isTruthy(used_analysis_variable_names())){
          
          status <- "success"
          
        } else {
          
          status <- "warning"
          
        }
        
        shinydashboard::box(
          html_and_css$additional_variables,
          solidHeader = TRUE,
          width = 12,
          status = status,
          title = "Step 3: Denote additional data variables"
        )
        
      })
      
      # ---
      
      # -----
      
      # Observe events ----------------------------------------------------------
      
      # update html if example data.frame is loaded
      oe <- shiny::observeEvent(c(example_df()), {
        
        ns <- session$ns
        
        html_and_css$analysis_module_variables <- 
          write_html_variable_assignment_modules(
            object = object, 
            example_df = example_df(), 
            ns = ns,
            exclude_from_choices = NULL
          )
        
      })
      
      # set module usage 
      oe <- shiny::observeEvent(module_usage(), {
        
        shiny::req(base::length(module_usage()) >= 1)
        
        object <- 
          setModuleActivity(
            object = cypro_object(), 
            activity = module_usage(), 
            verbose = TRUE, 
            in_shiny = TRUE
          )
        
        cypro_object(object)
        
      })
      
      # save module variable assignment
      oe <- shiny::observeEvent(input$module_vars_save, {
        
        ns <- session$ns
        
        debug_assign(shiny::reactiveValuesToList(input), as = "input_list")
        
        object <- 
          setVariableAssignmentShiny(
            object = cypro_object(), 
            input_list = shiny::reactiveValuesToList(input), 
            validate = TRUE
          )
        
        # update cypro object
        cypro_object(object)
        
        errors_occured <-
          validateInputDf(
            object = cypro_object(),
            df = example_df(),
            check_additional = FALSE
            ) %>% 
          purrr::keep(.p = ~ isOfClass(x = .x, valid_class = "glue"))
        
        if(base::length(errors_occured) >= 1){
          
          #error_fdb <- confuns::glue_list_report(lst = errors_occured, separator = NULL)
          
          error_fdb <-
            purrr::flatten_chr(errors_occured) %>% 
            stringr::str_c(collapse = "\n")
          
          validation_feedback(error_fdb)
          
          # shiny helper 
          shiny::showModal(
            ui = shiny::modalDialog(
              shiny::tagList(
                shiny::fluidRow(
                  shiny::column(
                    shiny::h4(shiny::strong("Invalid variable assignment.")), 
                    shiny::helpText(
                      "The following problems occured while checking the example file
                       and the assigned variables."),
                    shiny::verbatimTextOutput(outputId = ns("validation_feedback")),
                    width = 12
                  )
                )
              )
              ,
              footer = shiny::tagList(
                shiny::fluidRow(
                  hs(width = 4, shiny::actionButton(inputId = ns("change_vardenotation"), label = "Change variable denotation"))
                )
              ), 
              size = "l", 
              easyClose = FALSE
            )
          )
          
          shiny::req(FALSE)
          
        } 
        
        # update module vars
        module_vars <- 
          getVariableAssignment(object = cypro_object()) %>% 
          purrr::flatten_chr() %>% 
          purrr::keep(.p = ~ base::is.character(.x)) %>% 
          purrr::discard(.p = ~ base::is.na(.x))
        
        used_analysis_variable_names(module_vars)
        
        # update html
        exclude_from_choices <- used_analysis_variable_names()
        
        choices_grouping <- 
          dplyr::select(example_df(), -dplyr::any_of(exclude_from_choices)) %>% 
          dplyr::select_if(.predicate = is_grouping_candidate) %>% 
          base::colnames()
        
        choices_numeric <- 
          dplyr::select(example_df(), -dplyr::any_of(exclude_from_choices)) %>% 
          dplyr::select_if(.predicate = is_numeric_candidate) %>% 
          base::colnames()
        
        html_and_css$additional_variables <- 
          shiny::tagList(
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shinyWidgets::pickerInput(
                  inputId = ns("add_vars_grouping"), 
                  label = "Additional Grouping Variables:", 
                  choices = choices_numeric, 
                  multiple = TRUE, 
                  options = list(`live-search` = TRUE, `actions-box` = TRUE)
                ) %>% 
                  add_helper(
                    title = "Choose addtional grouping variables:", 
                    content = helper_content$additional_variables_grouping
                  )
              ),
              shiny::column(
                width = 6,
                shinyWidgets::pickerInput(
                  inputId = ns("add_vars_numeric"), 
                  label = "Additional Numeric Variables:", 
                  choices = choices_numeric, 
                  multiple = TRUE, 
                  options = list(`live-search` = TRUE, `actions-box` = TRUE)
                ) %>% 
                  add_helper(
                    title = "Choose addtional numeric variables:", 
                    content = helper_content$additional_variables_numeric
                  )
            )
            ), 
            shiny::fluidRow(
              shiny::column(
                width = 3, 
                shiny::actionButton(inputId = ns("add_vars_save"), label = "Save & Proceed")
              )
            )
          )
        
      })
      
      # save additional variables
      oe <- shiny::observeEvent(input$add_vars_save, {
        
        ns <- session$ns
        
        checkpoint(
          evaluate = base::is.character(used_analysis_variable_names()), # can be character(0) -> no modules used
          case_false = "no_module_vars"
        )
        
        object <- 
          setAdditionalVariableNames(
            object = cypro_object(), 
            grouping_vars = input$add_vars_grouping, 
            numeric_vars = input$add_vars_numeric, 
            in_shiny = TRUE
          )
        
        # update object
        cypro_object(object)
        
        all_var_names <- 
          c(used_analysis_variable_names(), input$add_vars_grouping, input$add_vars_numeric) %>% 
          base::unname()
        
        # set value for reactive final_df()
        final_df(dplyr::select(example_df(), dplyr::all_of(all_var_names)))
          
        # shiny helper 
        shiny::showModal(
          ui = shiny::modalDialog(
            shiny::tagList(
              shiny::fluidRow(
                shiny::column(
                  shiny::h4(shiny::strong("Final data output")), 
                  shiny::helpText("Cypro expects all files to contain at least these data variables."),
                  DT::dataTableOutput(outputId = ns("final_df")),
                  width = 12
                )
              )
            )
            ,
            footer = shiny::tagList(
              shiny::fluidRow(
                hs(width = 4, shiny::actionButton(inputId = ns("save_and_proceed"), label = "Proceed with data loading")), 
                hs(width = 4, shiny::actionButton(inputId = ns("change_vardenotation"), label = "Change variable denotation"))
              )
            ), 
            size = "l", 
            easyClose = FALSE
          )
        )
        
      })
      
      oe <- shiny::observeEvent(input$save_and_proceed, {
        
        shiny::removeModal()
        
        output_list$object <- cypro_object()
        output_list$proceed <- TRUE
        
        confuns::give_feedback(
          msg = "Variable assignment complete. Click on 'Return Cypro Object' to continue.", 
          with.time = FALSE, 
          in.shiny = TRUE,
          duration = 15
        )
        
      })
      
      oe <- shiny::observeEvent(input$change_vardenotation, {
        
        shiny::removeModal()
        
        used_analysis_variable_names(character(0))
        
        output_list$proceed <- FALSE
        
        shiny_fdb(
          ui = "If you are done denoting data variables. Click again on 'Save and Proceed'.",
          duration = 15
        )
        
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
          id = "example_dir", 
          session = session, 
          roots = dir_roots()
        )
        
      } else {
        
        shinyFiles::shinyDirChoose(
          input = input, 
          id = "example_dir", 
          session = session, 
          roots = dir_roots
        )
        
      }
        
      # ---
      
      # --- prepare data loading 
      
      # example data.frame 
      example_df <- shiny::reactive({
        
        shiny::req(input$example_dir)
        
        input_file <- input$example_dir
        
        directory <- input_file$datapath
        
        df <- read_example_file_shiny(directory = directory)
        
        # store input example immediately in object
        object <- 
          setInputExample(
            object = shiny::isolate(cypro_object()), 
            df = df, 
            dir = input_file$name
          )
        
        debug_assign(x = df, as = "df")
        
        cypro_object(object)
        
        return(df)
        
      })
      
      example_df_loaded <- shiny::reactive({
        
        !base::identical(example_df(), data.frame()) 
        
      })
      
      module_usage <- shiny::reactive({
        
        shiny::req(html_and_css$analysis_module_variables)
        
        input_list <- shiny::reactiveValuesToList(input)
        
        usage_list <- 
          input_list[stringr::str_detect(base::names(input_list), pattern = "module_usage")] 
        
        usage_list <- 
          usage_list[!stringr::str_detect(base::names(input_list), pattern = "identification")] %>% 
          purrr::discard(.p = ~ base::is.null(.x))
        
        shiny::req(base::length(usage_list) >= 1)
        
        nm <- 
          base::names(usage_list) %>%
          stringr::str_remove(pattern = "^module_usage-")
        
        usage_vec <- 
          purrr::flatten_lgl(.x = usage_list) %>% 
          purrr::set_names(nm = nm)
        
        return(usage_vec)
        
      })
      
      # ---
      
      # Table outputs -----------------------------------------------------------
      
      output$example_table <- DT::renderDataTable({
        
        shiny::validate(
          shiny::need(
            expr = example_df(), 
            message = "No example data has been loaded yet."
          )
        )
        
        utils::head(example_df())
        
      }, options = list(scrollX = TRUE))
      
      
      output$final_df <- DT::renderDataTable({
        
        shiny::req(final_df())
        
        utils::head(final_df())
        
      }, options = list(scrollX = TRUE))
      
      

      # Text outputs ------------------------------------------------------------
      
      output$validation_feedback <- shiny::renderText({
        
        shiny::req(validation_feedback())
        
        validation_feedback()
        
      })
    
      
      
      
      # Module return value -----------------------------------------------------
      
      base::return(output_list)
      
    })
  
}