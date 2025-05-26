
#' @title Server logic: Experiment Design
#' 
#' @description Server logic to be used as input for \code{module}-argument
#' of function \code{shiny::moduleServer()}.
#'
#' @param id Namespace ID
#'
#' @export

moduleExperimentDesignServer <- function(id, usage = "in_function"){

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){

      
  options(shiny.maxRequestSize = 50*1024^2)

# Reactive values ---------------------------------------------------------

  measurements_string <- shiny::reactiveVal(value = base::character())
  
  example_list <- shiny::reactiveVal(value = list(loaded = FALSE))
  
  well_plate_df <- shiny::reactiveVal(value = data.frame())
  well_plate_list <- shiny::reactiveVal(value = list())
  well_plate_name <- shiny::reactiveVal(value = base::character(1))
  
  # list containing all the information regarding the experiment's design
  # - the return value of this module
  ed_list <- shiny::reactiveValues(
    
    default_directory = base::character(),
    experiment_name = base::character(),
    module_progress = list(overall_information = FALSE, 
                           imaging_set_up = FALSE, 
                           experiment_phases = FALSE),
    proceed = base::logical(),
    set_up = list()
    
  )
  
  ###--- module progress
  
  module_progress_overall_information <- shiny::reactiveVal(value = FALSE)
  module_progress_imaging_set_up <- shiny::reactiveVal(value = FALSE)
  module_progress_experiment_phases <- shiny::reactiveVal(value = FALSE)
  
  # -----  
  
# Render UIs --------------------------------------------------------------

  # layout boxes 
  output$ed_imaging_set_up_box <- shiny::renderUI({
    
    ns <- session$ns
    
    shinydashboard::box(
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::fluidRow(
            shiny::column(
              width = 12, 
              shiny::uiOutput(outputId = ns("ed_imaging_set_up"))
            )
          ) 
        )
      ),
      solidHeader = TRUE, 
      status = css_status(evaluate = base::isTRUE(module_progress_overall_information())), 
      title = "2. Imaging Set Up", 
      width = 12
    )
    
  })
  
  output$ed_experiment_phases_box <- shiny::renderUI({
    
    ns <- session$ns
    
    shinydashboard::box(
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            shiny::fluidRow( 
              shiny::column(
                width = 8, 
                shiny::uiOutput(outputId = ns("ed_phases_number")), 
                shiny::uiOutput(outputId = ns("ed_phases_start")), 
                shiny::uiOutput(outputId = ns("ed_phases_save"))
              )
            )
          )
        ), 
        solidHeader = TRUE, 
        status = css_status(evaluate = base::isTRUE(module_progress_imaging_set_up())),
        title = "3. Experiment Phases", 
        width = 12
    )
    
  })
  
  output$ed_well_plate_box <- shiny::renderUI({
    
    ns <- session$ns
    
    shinydashboard::box(
      shiny::fluidRow(
        shiny::column(
          width = 12, 
          # new well plate
          shiny::fluidRow(
            shiny::column(
              width = 2,
              shiny::h5(shiny::strong("Step 1:")),
              shiny::actionButton(
                inputId = ns("ed_new_well_plate"), 
                label = "New Well Plate", 
                width = "100%")
            ),
            shiny::column(
              width = 2,
              shiny::h5(shiny::strong("Well Plate:")),
              shiny::selectInput(
                inputId = ns("ed_well_plate"), 
                label = NULL, 
                choices = valid_well_plates, 
                selected = "8x12 (96)")
            ), 
            shiny::column(
              width = 2,
              shiny::h5(shiny::strong("Well Plate Name:")), 
              shiny::textInput(
                inputId = ns("ed_well_plate_name"), 
                label = NULL, 
                placeholder = "well plate name")), 
            shiny::column(
              width = 2,
              shiny::h5(shiny::strong("ROIS per Well:")) %>% 
                add_helper(
                  content = helper_content$rois_per_well,
                  title = "In how many fields of view has each well been divided?"
                  ), 
              shiny::numericInput(
                inputId = ns("ed_rois_per_well"),
                min = 0, value = 0, step = 1,
                label = NULL))
          ),
          # well information 
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::h5(shiny::strong("Step 2:")),
              shiny::splitLayout(
                shiny::actionButton(
                  inputId = ns("ed_add_well_info"), 
                  label = "Add Info",
                  width = "100%"), 
                shiny::actionButton(
                  inputId = ns("ed_delete_well_info"), 
                  label = "Delete Info", 
                  width = "100%"), 
                cellWidths = c("50%", "50%")
              ), 
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::h5(shiny::strong("Cell Line:")),
                  shiny::textInput(
                    inputId = ns("ed_cell_line"), 
                    label = NULL,
                    placeholder = "cell line") 
                ),
                shiny::column(
                  width = 6,
                  shiny::uiOutput(outputId = ns("ed_conditions"))
                )
              )
            ),
            shiny::column(
              width = 8,
              # well plate plot
              shiny::plotOutput(outputId = ns("ed_well_plate_plot"), brush = ns("well_plate_brush"), hover = ns("well_plate_hover"))
            )
            
          ),
          shiny::HTML("<br>"),
          # add well plate
          shiny::fluidRow(
            shiny::column(
              width = 2,
              shiny::h5(shiny::strong("Step 3:")),
              shiny::actionButton(
                inputId = ns("ed_add_well_plate"), 
                label = "Save Well Plate",
                width = "100%")
            ),
            shiny::column(
              width = 2,
              shiny::h5(shiny::strong("Empty Wells:")),
              shinyWidgets::materialSwitch(
                inputId = ns("ed_dismiss_unknown"), 
                label = NULL, 
                value = FALSE, 
                status = "warning")
            ), 
            shiny::column(
              width = 8, 
              shiny::h5(shiny::strong("Well Information:")), 
              shiny::tableOutput(outputId = ns("ed_well_info")))
          )
        )
      ), 
      solidHeader = TRUE, 
      status = css_status(evaluate = shiny::isTruthy(module_progress_experiment_phases())), 
      title = "4. Well Plate Set Up",
      width = 12
    )
    
  })
  
  output$ed_save_and_proceed_box <- shiny::renderUI({
    
    ns <- session$ns
    
    shinydashboard::box(
      shiny::uiOutput(outputId = ns("ed_save_and_proceed_box_html")), 
      title = "Save Experiment Design & Proceed",
      solidHeader = TRUE,
      status = css_status(evaluate = base::length(well_plate_list()) >= 1),
      width = 12
    )
    
  })
  
  output$ed_save_and_proceed_box_html <- shiny::renderUI({
    
    ns <- session$ns
    
    shiny::validate(
      shiny::need(
        expr = base::names(well_plate_list()), 
        message = "No well plates have been added yet."
      )
    )
    
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          DT::dataTableOutput(outputId = ns("ed_well_plate_folders")),
          align = "center",
          width = 4
        ), 
        shiny::column(
          shiny::plotOutput(outputId = ns("ed_well_plate_plot2")),
          align = "center", 
          width = 8
        )
      ),
      shiny::fluidRow(
        shiny::column(
          shiny::actionButton(
            inputId = ns("ed_save_and_proceed"), 
            label = "Save & Proceed" 
          ), 
          align = "center",
          width = 4
        ),
        shiny::column(
          shiny::selectInput(
            inputId = ns("ed_well_plate_select"),
            label = NULL,
            choices = base::names(well_plate_list())
          ),
          align = "left",
          width = 2
        ),
        shiny::column(
          shiny::selectInput(
            inputId = ns("ed_well_plate_plot2_color"),
            label = NULL,
            choices = c("Condition" = "condition", "Cell Line" = "cell_line")
          ),
          align = "left",
          width = 2
        ),
        shiny::column(
          shiny::actionButton(
            inputId = ns("ed_edit_well_plate"),
            label = "Edit", 
            width = "100%"
            ), 
          align = "center", 
          width = 2
        ),
        shiny::column(
          shiny::actionButton(
            inputId = ns("ed_delete_well_plate"),
            label = "Remove",
            width = "100%"
            ),
          align = "center", 
          width = 2
        )
      )
    )
    
  })
  
  ###--- overall information
  
  ###--- imaging set up
  
  output$ed_imaging_set_up <- shiny::renderUI({
    
    ns <- session$ns
    
    # make sure that overall information has been provided
    shiny::validate(
      shiny::need(
        expr = base::isTRUE(module_progress_overall_information()), 
        message = "Overall information has not been saved yet."
      )
    )
    
    # make sure that it's a time lapse experiment
    shiny::validate(
      shiny::need(
        expr = experiment_type() == "time_lapse", 
        message = "Imaging set up not required for non time-lapse experiments. (Proceed with well plate set up.)"
      )
    )
    
    output_list <- 
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            width = 6, 
            shiny::numericInput(
              inputId = ns("ed_meas_num"),
              label = "Total Number of Frames:",
              min = 0, step = 1, value = 0
              ) %>% 
              add_helper(
                content = helper_content$total_number_of_images
              )
          )
        ),
        shiny::fluidRow(
          hs(6,
             shiny::h5(shiny::strong("Interval:")) %>%
               add_helper(
                 content = helper_content$interval
               ), 
             shiny::numericInput(
               inputId = ns("ed_meas_interval"), 
               label = NULL, 
               min = 1, step = 0.5, value = 1)
             ) , 
          hs(6,
             shiny::h5(shiny::strong("Interval Unit:")) %>% 
               add_helper(
                 content = helper_content$interval_unit
               ),
             shiny::selectInput(
               inputId = ns("ed_interval_unit"), 
               label = NULL, 
               choices = interval_options, 
               selected = "hours") )
        ),
        shiny::actionButton(
          inputId = ns("ed_imaging_set_up_save"), 
          label = "Save & Proceed")
        
      )
    
    return(output_list)
    
  })
  
  
  ###--- experiment phases set up
  output$ed_phases_number <- shiny::renderUI({
    
    ns <- session$ns
    
    # make sure that imaging set up has been provided
    shiny::validate(
      shiny::need(
        expr = base::isTRUE(module_progress_imaging_set_up()), 
        message = "Imaging set up has not been saved yet."
      )
    )
    
    # make sure that it's a time lapse experiment
    shiny::validate(
      shiny::need(
        expr = experiment_type() == "time_lapse", 
        message = "Phase set up not required for non time-lapse experiments. (Proceed with well plate set up.)"
      )
    )
    
    shiny::numericInput(
      inputId = ns("ed_phases_number"), 
      label = "Number of Phases:",
      value = 1, min = 1, max = 10, step = 1
    ) %>% 
      add_helper(
        content = helper_content$number_of_phases, 
        title = "How many phases does the experiment design contain?"
      )
    
  })
  
  output$ed_phases_start <- shiny::renderUI({
    
    ns <- session$ns
    
    # not going to appear if non time lapse exp as ed_phases_number is not going to appear 
    #shiny::req(base::isTRUE(module_progress_imaging_set_up()) & shiny::isTruthy(input$ed_phases_number))
    
    shiny::req(ed_list$set_up$experiment_type == "time_lapse" & shiny::isTruthy(input$ed_phases_number))
    
    all_phases <- 1:input$ed_phases_number
    
    ordinal_phases <- 
      english::ordinal(all_phases) %>%
      confuns::make_capital_letters(collapse.with = NULL) %>% 
      stringr::str_c(., "Phase starts from:", sep = " ")
    
    measurements <- measurements_string()
    
    n_meas <- base::length(measurements)
    
    output_list <- 
      purrr::map2(
        .x = all_phases,
        .y = ordinal_phases, 
        .f = function(x, y){
          
          if(x == 1){
            
            choices <- measurements[1]
            
          } else {
            
            choices <- measurements[x:n_meas]
            
          }
          
          shiny::selectInput(
            inputId = ns(stringr::str_c("ed_phases_start", x, sep = "_")),
            label = y,
            choices = choices
            )
          
        }
      )
    
    base::return(output_list[2:n_meas])
    
  })
  
  output$ed_phases_save <- shiny::renderUI({
    
    ns <- session$ns
    
    # not going to appear if non time lapse exp as ed_phases_start is not going to appear
    #shiny::req(module_progress_imaging_set_up() & shiny::isTruthy(input$ed_phases_number))
    
    shiny::req(ed_list$set_up$experiment_type == "time_lapse" & shiny::isTruthy(input$ed_phases_number))
    
    
    shiny::actionButton(inputId = ns("ed_phases_save"), label = "Save & Proceed")
    
  })
  
  
  ###--- well plate set up 
  output$ed_added_well_plates <- shiny::renderUI({
    
    ns <- session$ns
    
    shinyWidgets::pickerInput(
      inputId = ns("ed_added_well_plates"), 
      label = NULL, 
      choices = base::names(well_plate_list()), 
      multiple = TRUE
    )
    
  })
  
  
  output$ed_conditions <- shiny::renderUI({
    
    ns <- session$ns
    
    shiny::req(module_progress_experiment_phases())
    
    if(base::length(all_phases()) == 1){
      
      output_list <- 
        shiny::tagList(
          shiny::h5(shiny::strong("Condition:")),
          shiny::textInput(
            inputId = ns("ed_conditions"), 
            label = NULL, 
            value = "", 
            placeholder = "condition"
          )
        )
      
    } else {
      
      output_list <- 
        purrr::map(.x = all_phases(), .f = function(p){
                     
          
          label <- stringr::str_c("Condition",
                                  confuns::make_capital_letters(english::ordinal(p), collapse.with = NULL),
                                  "Phase:",
                                  sep = " ")
          
          shiny::tagList(
            shiny::h5(shiny::strong(label)), 
            shiny::textInput(
              inputId = ns(stringr::str_c("ed_condition", p, sep = "_")), 
              label = NULL, 
              value = "", 
              placeholder = "condition"
            )
            
          )
          
                   })
      
    }
    
    base::return(output_list)
    
  })

  
  # -----
  
# Observe events ----------------------------------------------------------
  
  ###--- overall information 
  
  # save and proceed
  oe <- shiny::observeEvent(input$ed_overall_info_save, {
    
    checkpoint(
      evaluate = shiny::isTruthy(ed_list$default_directory), 
      case_false = "no_storage_directory"
    )
    
    checkpoint(
      evaluate = input$ed_software %in% image_processing_softwares, 
      case_false = "no_software_input"
    )
    
    ed_list$experiment_name <- input$ed_experiment_name
    
    ed_list$storage_info$default_directory <- full_experiment_dir()
    
    ed_list$set_up$experiment_type <- input$ed_exp_type
    
    # unlock subsequent steps
    module_progress_overall_information(TRUE)
    
    if(ed_list$set_up$experiment_type == "time_lapse"){
    
      # set to FALSE to make sure that changing experiment type does not make 
      # the respective input options appear
      module_progress_imaging_set_up(FALSE)
      module_progress_experiment_phases(FALSE)
      
      shiny_fdb(
        in_shiny = TRUE,
        ui = "Overall information successfully stored. Proceed with imaging set up."
        )  
      
    } else {
      
      shiny_fdb(
        in_shiny = TRUE,
        ui = "Overall information successfully stored. Proceed with well plate set up."
        )  
      
      # no imaging set up needed
      ed_list$set_up$nom <- NULL
      ed_list$set_up$n_meas <- NULL
      ed_list$set_up$itvl <- NULL
      ed_list$set_up$itvl_u <- NULL
      ed_list$set_up$image_string <- NULL
      ed_list$set_up$measurement_string <- NULL
      ed_list$set_up$time_string <- NULL
      
      # no phases set up needed
      ed_list$set_up$phases <- list(first = "One Image")
      
      # unlock/skip subsequent steps if non time lapse
      module_progress_imaging_set_up(TRUE)
      module_progress_experiment_phases(TRUE)      
      
    }
    
  })
  
  
  ###--- imaging set up 
  
  # save and proceed 
  oe <- shiny::observeEvent(input$ed_imaging_set_up_save, {
    
    check_set_up_progress(
      progress_input = module_progress(),
      subset = "overall_information",
      proceed_with = "the imaging set up."
      )
    
    checkpoint(
      evaluate = input$ed_meas_num > 0, 
      case_false = "invalid_meas_number"
    )
    
    measurements_string(stringr::str_c(time_string(), image_string(), sep = " / "))
    
    ed_list$set_up$nom <- input$ed_meas_num
    ed_list$set_up$n_meas <- input$ed_meas_num
    ed_list$set_up$itvl <- input$ed_meas_interval
    ed_list$set_up$itvl_u <- input$ed_interval_unit
    ed_list$set_up$image_string <- image_string()
    ed_list$set_up$measurement_string <- measurements_string()
    ed_list$set_up$time_string <- time_string()
    
    module_progress_imaging_set_up(TRUE)
    
    shiny_fdb(in_shiny = TRUE, ui = "Imaging set up successfully saved. Proceed with experiment phases.")
    
  })
  
  
  ###--- experiment phases
  
  # save and proceed
  oe <- shiny::observeEvent(input$ed_phases_save, {
    
    all_phases <- 1:input$ed_phases_number
        
    phase_list <- purrr::map(
      .x = all_phases,
      .f = function(p){
        
        if(p == 1){
          
          res <- measurements_string()[1]
          
        } else {
          
          res <- input[[stringr::str_c("ed_phases_start", p, sep = "_")]]
          
        }
        
        base::return(res)
      
      
    }) %>% 
      purrr::set_names(nm = english::ordinal(all_phases))

    # reset well plate data
    well_plate_df(data.frame())
    well_plate_list(list())
    
    ed_list$set_up$phases <- phase_list
    module_progress_experiment_phases(TRUE)
    
    shiny_fdb(in_shiny = TRUE, ui = "Experiment phases successfully saved. Proceed with well plate set up.")
    
  })
  
  
  ###--- well plate set up 
  
  # new well plate
  oe <- shiny::observeEvent(input$ed_new_well_plate, {
    
    check_set_up_progress(progress_input = module_progress())
    
    # check input frame number per well
    checkpoint(
      evaluate = input$ed_rois_per_well != 0,
      case_false = "invalid_image_number"
      )
    
    # check well plate name
    wp_list_new <- well_plate_list()
    wp_name <- input$ed_well_plate_name 
    
    new_name <- !wp_name %in% base::names(wp_list_new)
    valid_name <- !wp_name == ""
    
    checkpoint(
      evaluate = base::all(new_name, valid_name),
      case_false = "invalid_wp_name"
      )
    
    # check if there is current well plate
    if(is_modified_wp_df(wp_df = well_plate_df())){
      
      ns <- session$ns
      
      shiny::showModal(
        ui = shiny::modalDialog(
          shiny::helpText(
            glue::glue(
              "You have not saved progress of the current well plate design. ",
              "Setting up a new well plate will result in all modifications of ",
              "the current well plate design beeing lost. "
              )
          ), 
          footer = shiny::tagList(
            act_button(
              inputId = ns("ed_overwrite_well_plate_confirmed"),
              label = "New Well Plate"
              ),
            act_button(
              inputId = ns("ed_overwrite_well_plate_cancelled"),
              label = "Cancel"
              )
          )
        )
      )
      
      shiny::req(FALSE)
      
    } 
    
    # update current well plate name
    well_plate_name(wp_name)
    
    # data.frame (obs => well)
    well_plate_df_new <- 
      setUpWellPlateDf(type = input$ed_well_plate, phases = all_phases())
    
    # update well_plate_df()
    well_plate_df(well_plate_df_new)
    
  })
  
  # overwrite well plate 
  oe <- shiny::observeEvent(input$ed_overwrite_well_plate_confirmed, {
    
    shiny::removeModal()
    
    # has been checked in observeEvent(input$ed_new_well_plate, ...) 
    wp_name <- input$ed_well_plate_name
    
    # update current well plate name
    well_plate_name(wp_name)
    
    # data.frame (obs => well)
    well_plate_df_new <- 
      setUpWellPlateDf(type = input$ed_well_plate, phases = all_phases())
    
    # update well_plate_df()
    well_plate_df(well_plate_df_new)
    
  })
  
  # dont overwrite well plate
  oe <- shiny::observeEvent(input$ed_overwrite_well_plate_cancelled, {
    
    shiny::removeModal()
    
  })
  
  # add well information 
  oe <- shiny::observeEvent(input$ed_add_well_info, {
    
    # check all_wells()
    checkpoint(evaluate = !base::identical(all_wells(), base::data.frame()), 
               case_false = "no_well_plate_chosen")
    
    checkpoint(evaluate = base::nrow(selected_wells()) != 0, 
               case_false = "no_wells_chosen")
    
    # -
    
    new_cell_line <- input$ed_cell_line
    
    if(base::length(all_phases()) > 1){
     
      new_condition <- 
        purrr::map(.x = all_phases(), .f = function(p){
                     
                     input[[stringr::str_c("ed_condition", p, sep = "_")]]
                     
                   }) %>% 
        purrr::flatten_chr()
      
    } else {
      
      new_condition <- input$ed_conditions
      
    }
    
    if(!base::all(new_condition == "")){
      
      checkpoint(
        evaluate = !base:::any(new_condition == ""), 
        case_false = "missing_condition_input", 
        duration = 10
      )
      
    }
    
    checkpoint(
      evaluate = !base::all(new_condition == "") | !base::all(new_cell_line == ""), 
      case_false = "missing_both_inputs"
    )
    
    checkpoint(
      evaluate = base::length(base::intersect(x = new_cell_line, y = new_condition)) == 0, 
      case_false = "cell_line_condition_overlap"
    )
    
    selected_wells <- selected_wells()$well
    
    well_plate_df_new <- all_wells()
    well_plate_df_new$selected <- well_plate_df_new$well %in% selected_wells
    
    if(new_cell_line != ""){
      
      well_plate_df_new <- 
        dplyr::mutate(well_plate_df_new, 
          cell_line = dplyr::case_when(
            selected ~ {{new_cell_line}},
            TRUE ~ cell_line
            )
        )
      
    }
    
    if(base::all(new_condition != "")){
      
      well_plate_df_new$condition <-
        update_condition(
          var_condition = well_plate_df_new$condition, 
          var_selected = well_plate_df_new$selected, 
          input_condition = new_condition
          )
      
      
      well_plate_df_new$condition_df <- 
        update_condition_df(
          var_condition_df = well_plate_df_new$condition_df, 
          var_selected = well_plate_df_new$selected, 
          input_condition = new_condition
        )
      
    }
    
    
    well_plate_df_final <- 
      dplyr::mutate(.data = well_plate_df_new, 
        cl_condition = stringr::str_c(cell_line, condition, sep = " & "),
        information_status = base::as.character(information_status),
        information_status = dplyr::case_when(
            condition == "unknown" & cell_line == "unknown" ~ "Missing", 
            condition != "unknown" & cell_line != "unknown" ~ "Complete", 
            TRUE ~ "Incomplete"
            ), 
        information_status = base::factor(x = information_status, levels = c("Complete", "Incomplete", "Missing"))
      ) %>% 
      dplyr::select(-selected)
    
    # update well_plate_df()
    well_plate_df(well_plate_df_final)
    
    # give feedback 
    shiny_fdb(in_shiny = TRUE, ui = "Well info successfully added.")
    
  })
  
  # delete well information 
  oe <- shiny::observeEvent(input$ed_delete_well_info, {
    
    # check all_wells()
    checkpoint(evaluate = !base::identical(all_wells(), base::data.frame()), 
               case_false = "no_well_plate_chosen")
    
    checkpoint(evaluate = base::nrow(selected_wells()) != 0, 
               case_false = "no_wells_chosen")
    
    # -
    
    condition_df_exmpl <- all_wells()$condition_df[[1]]
    phase_names <- base::colnames(condition_df_exmpl)
    
    # empty condition df
    ecdf <-
      purrr::map_df(.x = condition_df_exmpl, .f = function(phase){return(NA)}) %>% 
      purrr::set_names(phase_names) %>% 
      base::as.data.frame()
    
    well_plate_df_new <- 
      dplyr::mutate(.data = all_wells(), 
        selected = well %in% selected_wells()$well, 
        condition = dplyr::case_when(selected ~ "unknown", TRUE ~ condition),
        cell_line = dplyr::case_when(selected ~ "unknown", TRUE ~ cell_line),
        cl_condition = stringr::str_c(cell_line, condition, sep = " & "),
        condition_df = purrr::map2(.x = condition_df, .y = selected, ecdf = ecdf, .f = function(cdf, selected, ecdf){
                
              if(base::isTRUE(selected)){
                        
                        return(ecdf)
                        
                      } else{
                        
                        return(cdf)
                        
                      }
                      
                    }),
        information_status = base::as.character(information_status),
        information_status = dplyr::case_when(
            condition == "unknown" & cell_line == "unknown" ~ "Missing", 
            condition != "unknown" & cell_line != "unknown" ~ "Complete", 
            TRUE ~ "Incomplete"
            ), 
        information_status = base::factor(x = information_status, levels = c("Complete", "Incomplete", "Missing"))
      ) %>% 
      dplyr::select(-selected)
    
    # update well_plate_df()
    well_plate_df(well_plate_df_new)
    ed_list$object <- set_up_cdata_meta(ed_list$object)
    shiny_fdb(in_shiny = TRUE, ui = "Well information successfully removed.")
    
  })
  
  # add well plate
  oe <- shiny::observeEvent(input$ed_add_well_plate, {
    
    wp_list_new <- well_plate_list()
    
    # check all_wells()
    checkpoint(evaluate = !base::identical(all_wells(), base::data.frame()), 
               case_false = "no_well_plate_chosen")
    
    
    checkpoint(evaluate = base::sum(all_wells()$information_status == "Complete") >= 1, 
               case_false = "empty_well_plate")
    
    wp_df <- dplyr::mutate(.data = all_wells(), rois_per_well = input$ed_rois_per_well)
    
    # check information status
    if(base::isFALSE(input$ed_dismiss_unknown)){
      
      check <- check_wp_df_shiny(wp_df = wp_df)
      
      checkpoint(evaluate = check$evaluate, 
                 case_false = "case_false", 
                 error_notifications = check, 
                 duration = 15) 
      
    } 
    
    # add new well plate
    wp_list_new[[well_plate_name()]][["wp_df"]] <- wp_df
    
    # update reactive values
    well_plate_list(wp_list_new)
    well_plate_df(data.frame())
    
  })
  
  # save and proceed
  oe <- shiny::observeEvent(input$ed_save_and_proceed, {
    
    checkpoint(
      evaluate = base::length(well_plate_list()) != 0,
      case_false = "no_well_plates_added"
      )
    
    ed_list$proceed <- TRUE
    
    ed_list$object <- 
      initiateEmptyCyproObject(
        information = list(storage_directory = ed_list$default_directory),
        name = ed_list$experiment_name,
        set_up = ed_list$set_up, 
        well_plates = well_plate_list()
      )
    
    
    shiny_fdb(
      in_shiny = TRUE,
      ui = "Experiment design successfully saved. Click on 'Return Cypro Object' and proceed with loadData()."
      )
    
    
  })
  
  # -----
  
  ###--- save experiment design and proceed
  
  oe <- shiny::observeEvent(input$ed_edit_well_plate, {
    
    ns <- session$ns
    
    if(is_modified_wp_df(well_plate_df())){
      
      shiny::showModal(
        ui = shiny::modalDialog(
          shiny::helpText(
            glue::glue(
              "You have not saved progress of the current well plate design. ",
              "Editing a well plate will result in all modifications of ",
              "the current well plate design beeing lost. "
            )
          ), 
          footer = shiny::tagList(
            act_button(
              inputId = ns("ed_overwrite_well_plate2_confirmed"),
              label = "Edit Well Plate"
              ),
            act_button(
              inputId = ns("ed_overwrite_well_plate2_cancelled"),
              label = "Cancel"
              )
          )
        )
      )
      
    } else {
      
      wp_df <- well_plate_list()[[input$ed_well_plate_select]]$wp_df
      
      wp_name <- input$ed_well_plate_select
      
      well_plate_name()
      well_plate_df(wp_df)
      
    }
    
  })
  
  # overwrite the well plate that is currently being designed to edit 
  # a well plate that has been saved already 
  oe <- shiny::observeEvent(input$ed_overwrite_well_plate2_confirmed, {
    
    shiny::removeModal()
    
    wp_list <- well_plate_list()
    wp_name <- input$ed_well_plate_select
    
    wp_df <- wp_list[[wp_name]]$wp_df
    
    well_plate_name(wp_name)
    well_plate_df(wp_df)
    
  })
  
  oe <- shiny::observeEvent(input$ed_overwrite_well_plate2_cancelled, {
    
    shiny::removeModal()
    
  })
  
  oe <- shiny::observeEvent(input$ed_delete_well_plate, {
    
    ns <- session$ns
    
    shiny::showModal(
      ui = shiny::modalDialog(
        shiny::helpText(
          glue::glue(
            "A deleted well plate can not be restored and has to be designed ",
            "again. Are you sure you want to remove well plate '{input$ed_well_plate_select}'?"
          )
        ),
        footer = shiny::tagList(
          act_button(inputId = ns("ed_delete_well_plate_confirmed"), label = "Yes, delete well plate"),
          act_button(inputId = ns("ed_delete_well_plate_cancelled"), label = "No, cancel")
        )
      )
    )
    
    
  })
  
  oe <- shiny::observeEvent(input$ed_delete_well_plate_confirmed, {
    
    shiny::removeModal()
    
    wp_list <- well_plate_list()
    
    wp_list[[input$ed_well_plate_select]] <- NULL
    
    well_plate_list(wp_list)
    
  })
  
  oe <- shiny::observeEvent(input$ed_delete_well_plate_cancelled, {
    
    shiny::removeModal()
    
  })
  
# Reactive expressions ----------------------------------------------------

  
  ###--- module progress
  
  module_progress <- shiny::reactive({
    
    list(
      overall_information = module_progress_overall_information(),
      imaging_set_up = module_progress_imaging_set_up(), 
      experiment_phases = module_progress_experiment_phases()
    )
    
  })
  
  ###--- overall information 
  
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
      id = "ed_experiment_dir", 
      session = session, 
      roots = dir_roots()
    )
    
  } else {
    
    shinyFiles::shinyDirChoose(
      input = input, 
      id = "ed_experiment_dir", 
      session = session, 
      roots = dir_roots, 
      restrictions = base::system.file(package = "base")
    )
    
  }
  
  full_experiment_dir <- shiny::reactive({
    
    shiny::validate(
      shiny::need(
        expr = shiny::isTruthy(input$ed_experiment_name), 
        message = "Experiment has not been named yet."
      )
    )
    
    shiny::validate(
      shiny::need(expr = base::is.list(x = input$ed_experiment_dir), 
                  message = "No folder chosen.")
    )
    
    if(sysname == "Windows"){
      
      folder_dir <- hlpr_assemble_directory(input_list = input$ed_experiment_dir)
      
    } else {
      
      folder_dir <- shinyFiles::parseDirPath(roots = c(wd = "~"), input$ed_experiment_dir)
      
    }
    
    full_dir <- stringr::str_c(folder_dir, "/", input$ed_experiment_name, ".RDS", sep = "")
    
    ed_list$default_directory <- full_dir
    
    base::return(full_dir)
    
  })
  
  experiment_type <- shiny::eventReactive(input$ed_overall_info_save, {
    
    return(input$ed_exp_type)
        
  })
  
  ###--- imaging set up 
  time_string <- shiny::reactive({
    
    stringr::str_c(1:input$ed_meas_num * input$ed_meas_interval,
                   input$ed_interval_unit,
                   sep = " ")
    
  })
  
  image_string <- shiny::reactive({
    
    english::ordinal(x = 1:input$ed_meas_num) %>% 
      confuns::make_capital_letters(collapse.with = NULL) %>% 
      stringr::str_c(., "Image", sep = " ")
    
  })
  
  ###--- experiment phases 
  
  all_phases <- shiny::reactive({
    
    base::seq_along(ed_list$set_up$phases)
    
  })
  
  
  ###--- well plate set up
  
  # well data.frames ---
  all_wells <- shiny::reactive({
    
    well_plate_df()
    
  })
  
  selected_wells <- shiny::reactive({
    
    xmin <- input$well_plate_brush$xmin
    xmax <- input$well_plate_brush$xmax
    
    ymin <- input$well_plate_brush$ymin
    ymax <- input$well_plate_brush$ymax
    
    is_selected <-
      !base::any(
        base::is.null(xmin), 
        base::is.null(xmax), 
        base::is.null(ymin), 
        base::is.null(ymax)
       )
    
    if(base::isTRUE(is_selected)){
      
      selected_wells <- 
      dplyr::filter(well_plate_df(),
        dplyr::between(col_num, xmin, xmax),
        dplyr::between(row_num, ymin, ymax)
        )
      
    } 
    
    base::return(selected_wells)
    
  })
  
  hovered_well_info <- shiny::reactive({
    
    xh <- input$well_plate_hover$x
    
    yh <- input$well_plate_hover$y
    
    hovering <-
      base::all(
        base::is.numeric(xh), 
        base::is.numeric(yh) 
      )
    
    shiny::validate(
      shiny::need(
        expr = base::isTRUE(hovering), 
        message = "Hover over a well to have it's information printed here."
      )
    )
    
    xh <- base::round(xh, digits = 0)
    yh <- base::round(yh, digits = 0)
      
    hovered_well <- 
      dplyr::filter(.data = well_plate_df(), row_num == yh & col_num == xh) %>% 
      dplyr::ungroup()
    
    shiny::validate(
      shiny::need(
        expr = base::nrow(hovered_well) == 1, 
        message = "Hover over a well to have it's information printed here."
      )
    )
      
    condition_df <- hovered_well$condition_df[[1]]
    
    if(base::ncol(condition_df) == 1){
      
      base::colnames(condition_df) <- "Condition"
      
    } else {
      
      base::colnames(condition_df) <- 
        stringr::str_replace_all(
          base::colnames(condition_df),
          pattern = "Phase",
          replacement = "Condition"
          )
      
    }
    
    condition_df <- 
      purrr::map_df(.x = condition_df, .f = ~ tidyr::replace_na(data = .x, replace = "unknown"))
    
    well_cell_line_df <-
      dplyr::select(hovered_well, `Well:` = well, Status = information_status, `Cell Line:` = cell_line)
    
    base::return(base::cbind(well_cell_line_df, condition_df))
    
  })
  
  well_plate_df2 <- shiny::reactive({
    
    wp_list <- well_plate_list()
    
    df <- wp_list[[input$ed_well_plate_select]]$wp_df
    
    return(df)
    
  })
  
  # well plate visualization 
  
  ed_well_plate_plot <- shiny::reactive({
    
    shiny::validate(
      shiny::need(
        expr = !base::identical(all_wells(), base::data.frame()),
        message = "No well plate chosen.")
      )
    
    plot_well_plate_shiny(
      wp_df = all_wells(),
      selected_wells_df = selected_wells(),
      aes_fill = "cl_condition",
      aes_color = "information_status",
      color_values = status_colors
      ) +
      ggplot2::labs(
        color = "Information Status",
        subtitle = stringr::str_c("Name:", well_plate_name(), sep = " ")
        ) 
    
  })
  
  ed_well_plate_plot2 <- shiny::reactive({
    
    shiny::req(shiny::isTruthy(base::names(well_plate_list())))
    
    plot_well_plate_shiny(
      wp_df = well_plate_df2(),
      selected_wells_df = NULL,
      aes_fill = input$ed_well_plate_plot2_color, 
      aes_color = "information_status",
      color_values = status_colors
    ) +
      ggplot2::labs(
        color = "Information Status",
        subtitle = stringr::str_c("Name:", input$ed_well_plate_select, sep = " ")
      ) 
    
  })
  
  # -----
  
# Plot outputs ------------------------------------------------------------
  
  output$ed_well_plate_plot <- shiny::renderPlot({
    
    ed_well_plate_plot()
    
  })
  
  output$ed_well_plate_plot2 <- shiny::renderPlot({
    
    ed_well_plate_plot2()
    
  })
  
  # -----
  
# Table outputs -----------------------------------------------------------
  
  output$ed_well_info <- shiny::renderTable({
    
    hovered_well_info()
    
  })
  
  output$ed_well_plate_folders <- DT::renderDataTable({
    
    df <- 
      data.frame(
        "Name" = base::names(well_plate_list()),
        "Type" = purrr::map_chr(.x = well_plate_list(), ~ base::unique(.x[["wp_df"]][["type"]])), 
        "ROIS per Well" = purrr::map_dbl(.x = well_plate_list(), ~ base::unique(.x[["wp_df"]][["rois_per_well"]]))
      )
    
    base::rownames(df) <- NULL
    
    return(df)
    
  })
  
# Text outputs ------------------------------------------------------------
  
  output$ed_chosen_dir <- shiny::renderText({
    
    well_plate_dir()
    
  })
  
  output$ed_experiment_path <- shiny::renderText({
    
    full_experiment_dir()
    
  })
  
  
# Module return value -----------------------------------------------------
  
  base::return(ed_list)
  
  })

}





# helper ------------------------------------------------------------------

#' Title
#'
#' @description Helps to generate the rendered UIs with which to denote the 
#' x-, y-, etc. colums if the data has been generated by cell profiler.

example_selected <- function(lst, slot = "denoted_columns", col, nth, choices = NULL){
  
  if(base::is.character(lst[[slot]][[col]])){
    
    base::return(lst$denoted_columns[[col]])
    
  } else if(col != "additional") {
    
    base::return(base::colnames(lst$df)[nth])
    
  } else if(col == "additional"){
    
    additional_cols <- base::is.character(lst[[slot]][[col]])
    
    if(base::is.character(additional_cols) & base::length(additional_cols) >= 1){
      
      base::return(lst[[slot]][[col]])
      
    } else {
    
      base::return(choices)  
      
    }
    
  }
  
}


#' Title
#'
#' @description Take shiny input and update the condition and the condition_df variables
#' of the well plate data.frame.

update_condition <- function(var_condition, var_selected, input_condition){ #!!! not longer needed
  
  purrr::map2(.x = var_condition, 
              .y = var_selected, 
              .f = function(x, .selected){
                
                if(base::isTRUE(.selected)){
                  
                  if(base::length(input_condition) > 1){
                    
                    input_condition <- 
                      stringr::str_c(
                        base::seq_along(input_condition), 
                        input_condition, 
                        sep = "."
                      )
                    
                    res <- stringr::str_c(input_condition, collapse = " -> ")
                    
                  } else {
                    
                    res <- input_condition
                  }
                  
                } else {
                  
                  res <- x
                  
                }
                
                base::return(res)
                
              }) %>% 
    purrr::flatten_chr()
  
}

#' @rdname update_condition 
update_condition_df <- function(var_condition_df, var_selected, input_condition, input_phases){
  
  purrr::map2(.x = var_condition_df,
              .y = var_selected, 
              .f = function(df, .selected){
                
                phases <- base::colnames(df)
                
                if(base::isTRUE(.selected)){
                  
                  for(phase in base::seq_along(phases)){
                    
                    df[1, phase] <- input_condition[phase]
                    
                  }
                  
                } 
                
                base::return(df)
                
              })
  
}


#' Title
#'
#' @description Make sure that information input is sufficient before proceeding with well plates. 
check_set_up_progress <- function(progress_input,
                                  subset = c("overall_information", "imaging_set_up", "experiment_phases"), 
                                  proceed_with = "the well plate set up"){
  
  missing <- 
    purrr::keep(progress_input[subset], .p = base::isFALSE) 
  
  progress_named <- c("overall_information" = "Overall Information", 
                      "imaging_set_up" = "Imaging Set Up", 
                      "experiment_phases" = "Experiment Phases")
  
  if(base::length(missing) >= 1){
    
    missing_names <- progress_named[base::names(missing)]
    
    msg <- glue::glue("Make sure to provide and save required information about '{missing_names}' before proceeding with {proceed_with}.", 
                      missing_names = glue::glue_collapse(missing_names, sep = "', '", last = "' and '"))
    
    shiny_fdb(in_shiny = TRUE, ui = msg, type = "error", duration = 10)
    
    shiny::req(FALSE)
    
  }
  
}


#' Title
#' 
#' @description Make sure that all denoted columns appear only one time. 
#'
valid_example_cols <- function(example_list){ #!!! not longer needed?
  
  col_count <- 
    example_list$denoted_columns %>%
    purrr::flatten_chr() %>%
    base::table() %>%
    base::as.data.frame() %>% 
    purrr::set_names(nm = c("col", "count")) %>% 
    dplyr::filter(count > 1)
  
  if(base::nrow(col_count) >= 1){
    
    dupl_names <- glue::glue_collapse(col_count$col, sep = "', '", last = "' and '")
    
    ref1 <- confuns::adapt_reference(col_count$col, sg = "variable", pl = "variables")
    ref2 <- confuns::adapt_reference(col_count$col, sg = "is", pl = "are")
    
    msg <- glue::glue("Invalid variable denotation. The {ref1} '{dupl_names}' {ref2} used more than one time.")
    
    shiny_fdb(in_shiny = TRUE, ui = msg, type = "error", duration = 20)
    
    shiny::req(FALSE)
    
  }
  
}



