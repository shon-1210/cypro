
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


# reactive values ---------------------------------------------------------
     
  example_list <- shiny::reactiveVal(value = list(loaded = FALSE))
  
  # s4 objects
  new_cypro_object <- shiny::reactiveVal(value = FALSE)
  new_exp_design <- shiny::reactiveVal(value = FALSE)
  
  
  # well plate handling
  current_layout_df <- shiny::reactiveVal(value = data.frame())
  current_well_plate <- shiny::reactiveVal(value = FALSE)
  current_well_plate_name <- shiny::reactiveVal(value = character(1))
  
  # list containing all the information regarding the experiment's design
  # - the return value of this module
  output_list <- shiny::reactiveValues(
    proceed = base::logical(),
    object = methods::new(Class = "Cypro")
  )
  
  ###--- module progress
  
  module_progress_overall_information <- shiny::reactiveVal(value = FALSE)
  module_progress_imaging_set_up <- shiny::reactiveVal(value = FALSE)
  module_progress_experiment_phases <- shiny::reactiveVal(value = FALSE)
  
  # -----  
  
# Render UIs --------------------------------------------------------------

  # layout boxes 
  output$imaging_set_up_box <- shiny::renderUI({
    
    ns <- session$ns
    
    shinydashboard::box(
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::fluidRow(
            shiny::column(
              width = 12, 
              shiny::uiOutput(outputId = ns("imaging_set_up"))
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
  
  output$experiment_phases_box <- shiny::renderUI({
    
    ns <- session$ns
    
    shinydashboard::box(
        shiny::fluidRow(
          shiny::column(
            width = 12, 
            shiny::fluidRow( 
              shiny::column(
                width = 8, 
                shiny::uiOutput(outputId = ns("phases_number")), 
                shiny::uiOutput(outputId = ns("phases_start")), 
                shiny::uiOutput(outputId = ns("phases_save"))
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
  
  output$well_plate_box <- shiny::renderUI({
    
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
                inputId = ns("new_well_plate"), 
                label = "New Well Plate", 
                width = "100%")
            ),
            shiny::column(
              width = 2,
              shiny::h5(shiny::strong("Well Plate:")),
              shiny::selectInput(
                inputId = ns("well_plate_type"), 
                label = NULL, 
                choices = valid_well_plates, 
                selected = "8x12")
            ), 
            shiny::column(
              width = 2,
              shiny::h5(shiny::strong("Well Plate Name:")), 
              shiny::textInput(
                inputId = ns("well_plate_name"), 
                label = NULL, 
                placeholder = "well plate name")), 
            shiny::column(
              width = 2,
              shiny::h5(shiny::strong("ROIS per Well:")) %>% 
                add_helper(
                  content = helper_content$rois_per_well,
                  title = "In how many regions of interest / fields of view has each well been divided?"
                  ), 
              shiny::numericInput(
                inputId = ns("rois_per_well"),
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
                  inputId = ns("add_well_info"), 
                  label = "Add Info",
                  width = "100%"), 
                shiny::actionButton(
                  inputId = ns("delete_well_info"), 
                  label = "Delete Info", 
                  width = "100%"), 
                cellWidths = c("50%", "50%")
              ), 
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::h5(shiny::strong("Cell Line:")),
                  shiny::textInput(
                    inputId = ns("cell_line"), 
                    label = NULL,
                    placeholder = "cell line") 
                ),
                shiny::column(
                  width = 6,
                  shiny::uiOutput(outputId = ns("conditions"))
                )
              )
            ),
            shiny::column(
              width = 8,
              # well plate plot
              shiny::plotOutput(outputId = ns("well_plate_plot"), brush = ns("well_plate_brush"), hover = ns("well_plate_hover"))
            )
            
          ),
          shiny::HTML("<br>"),
          # add well plate
          shiny::fluidRow(
            shiny::column(
              width = 2,
              shiny::h5(shiny::strong("Step 3:")),
              shiny::actionButton(
                inputId = ns("add_well_plate"), 
                label = "Save Well Plate",
                width = "100%")
            ),
            shiny::column(width = 2),
            shiny::column(
              width = 8, 
              shiny::h5(shiny::strong("Well Information:")), 
              shiny::tableOutput(outputId = ns("well_info")))
          )
        )
      ), 
      solidHeader = TRUE, 
      status = css_status(evaluate = shiny::isTruthy(module_progress_experiment_phases())), 
      title = "4. Well Plate Set Up",
      width = 12
    )
    
  })
  
  output$save_and_proceed_box <- shiny::renderUI({
    
    ns <- session$ns
    
    shinydashboard::box(
      shiny::uiOutput(outputId = ns("save_and_proceed_box_html")), 
      title = "Save Experiment Design & Proceed",
      solidHeader = TRUE,
      status = css_status(evaluate = base::length(well_plate_list()) >= 1),
      width = 12
    )
    
  })
  
  output$save_and_proceed_box_html <- shiny::renderUI({
    
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
          DT::dataTableOutput(outputId = ns("well_plate_folders")),
          align = "center",
          width = 4
        ), 
        shiny::column(
          shiny::plotOutput(outputId = ns("well_plate_plot2")),
          align = "center", 
          width = 8
        )
      ),
      shiny::fluidRow(
        shiny::column(
          shiny::actionButton(
            inputId = ns("save_and_proceed"), 
            label = "Save & Proceed" 
          ), 
          align = "center",
          width = 4
        ),
        shiny::column(
          shiny::selectInput(
            inputId = ns("well_plate_select"),
            label = NULL,
            choices = base::names(well_plate_list())
          ),
          align = "left",
          width = 2
        ),
        shiny::column(
          shiny::selectInput(
            inputId = ns("well_plate_plot2_color"),
            label = NULL,
            choices = c("Condition" = "condition", "Cell Line" = "cell_line")
          ),
          align = "left",
          width = 2
        ),
        shiny::column(
          shiny::actionButton(
            inputId = ns("edit_well_plate"),
            label = "Edit", 
            width = "100%"
            ), 
          align = "center", 
          width = 2
        ),
        shiny::column(
          shiny::actionButton(
            inputId = ns("delete_well_plate"),
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
  
  output$imaging_set_up <- shiny::renderUI({
    
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
        expr = exp_type() != "CyproScreening", 
        message = "Imaging set up not required for non time lapse experiments. (Proceed with well plate set up.)"
      )
    )
    
    output_list <- 
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(
            width = 6, 
            shiny::numericInput(
              inputId = ns("n_frames"),
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
               inputId = ns("interval"), 
               label = NULL, 
               min = 1, step = 0.5, value = 1)
             ) , 
          hs(6,
             shiny::h5(shiny::strong("Interval Unit:")) %>% 
               add_helper(
                 content = helper_content$interval_unit
               ),
             shiny::selectInput(
               inputId = ns("interval_unit"), 
               label = NULL, 
               choices = interval_options, 
               selected = "hours") )
        ),
        shiny::actionButton(
          inputId = ns("imaging_set_up_save"), 
          label = "Save & Proceed")
        
      )
    
    return(output_list)
    
  })
  
  
  ###--- experiment phases set up
  output$phases_number <- shiny::renderUI({
    
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
        expr = multiple_phases(), 
        message = "Phase set up not required for non-multiple-phase experiments. (Proceed with well plate set up.)"
      )
    )
    
    shiny::numericInput(
      inputId = ns("phases_number"), 
      label = "Number of Phases:",
      value = 1, min = 1, max = 10, step = 1
    ) %>% 
      add_helper(
        content = helper_content$number_of_phases, 
        title = "How many phases does the experiment design contain?"
      )
    
  })
  
  output$phases_start <- shiny::renderUI({
    
    ns <- session$ns
    
    # not going to appear if non time lapse exp as phases_number is not going to appear 
    #shiny::req(base::isTRUE(module_progress_imaging_set_up()) & shiny::isTruthy(input$phases_number))
    
    shiny::req(
      multiple_phases() &
      shiny::isTruthy(input$phases_number)
      )
    
    phases <- 1:input$phases_number
    
    ordinal_phases <- 
      english::ordinal(phases) %>%
      confuns::make_capital_letters(collapse.with = NULL) %>% 
      stringr::str_c(., "Phase starts from:", sep = " ")
    
    frames <- time_frame_vec()
    
    n_frames <- base::length(frames)
    
    output_list <- 
      purrr::map2(
        .x = phases,
        .y = ordinal_phases, 
        .f = function(x, y){
          
          if(x == 1){
            
            choices <- frames[1]
            
          } else {
            
            choices <- frames[x:n_frames]
            
          }
          
          shiny::selectInput(
            inputId = ns(stringr::str_c("phases_start", x, sep = "_")),
            label = y,
            choices = choices
            )
          
        }
      )
    
    base::return(output_list[2:n_frames])
    
  })
  
  output$phases_save <- shiny::renderUI({
    
    ns <- session$ns
    
    # not going to appear if non time lapse exp as phases_start is not going to appear
    #shiny::req(module_progress_imaging_set_up() & shiny::isTruthy(input$phases_number))
    
    shiny::req(
      multiple_phases() &
      shiny::isTruthy(input$phases_number)
      )
    
    shiny::actionButton(inputId = ns("phases_save"), label = "Save & Proceed")
    
  })
  
  
  ###--- well plate set up 
  output$added_well_plates <- shiny::renderUI({
    
    ns <- session$ns
    
    shinyWidgets::pickerInput(
      inputId = ns("added_well_plates"), 
      label = NULL, 
      choices = base::names(well_plate_list()), 
      multiple = TRUE
    )
    
  })
  
  
  output$conditions <- shiny::renderUI({
    
    ns <- session$ns
    
    shiny::req(module_progress_experiment_phases())
    
    if(n_phases() == 0){
      
      output_list <- 
        shiny::tagList(
          shiny::h5(shiny::strong("Condition:")),
          shiny::textInput(
            inputId = ns("conditions"), 
            label = NULL, 
            value = "", 
            placeholder = "condition"
          )
        )
      
    } else {
      
      phases <- base::seq_along(all_phases())
      
      output_list <- 
        purrr::map(.x = phases, .f = function(p){
                     
          ordinal_phase <- confuns::make_capital_letters(english::ordinal(p), collapse.with = NULL)
          
          label <- 
            stringr::str_c("Condition", ordinal_phase , "Phase:", sep = " ")
          
          shiny::tagList(
            shiny::h5(shiny::strong(label)), 
            shiny::textInput(
              inputId = ns(stringr::str_c("condition", p, sep = "_")), 
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
  oe <- shiny::observeEvent(input$overall_info_save, {
    
    checkpoint(
      evaluate = validate_experiment_name(exp_name = input$exp_name), 
      case_false = "invalid_exp_name"
    )
    
    exp_name <- remove_empty_space(string = input$exp_name)
    
    # set up new cypro object
    cypro_object <- 
      methods::new(
        Class = input$exp_type,
        experiment = exp_name
        )
    
    new_cypro_object(cypro_object)
    
    # set up new experiment design
    exp_design <- 
      methods::new(
        Class = "ExperimentDesign",
        type = input$exp_type, 
        experiment = exp_name
        )
    
    new_exp_design(exp_design)
    
    # unlock subsequent steps
    module_progress_overall_information(TRUE)
    
    if(input$exp_type != "CyproScreening"){
    
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
      
      # skip subsequent steps if non time lapse
      module_progress_imaging_set_up(TRUE)
      module_progress_experiment_phases(TRUE)      
      
    }
    
  })
  
  
  ###--- imaging set up 
  
  # save and proceed 
  oe <- shiny::observeEvent(input$imaging_set_up_save, {
    
    check_set_up_progress(
      progress_input = module_progress(),
      subset = "overall_information",
      proceed_with = "the imaging set up."
      )
    
    checkpoint(
      evaluate = input$n_frames > 0, 
      case_false = "invalid_n_frames"
    )
    
    exp_design <- new_exp_design()
    
    exp_design@interval <- input$interval
    exp_design@interval_unit <- input$interval_unit
    exp_design@n_frames <- input$n_frames
    
    new_exp_design(exp_design)
    
    if(multiple_phases()){
      
      module_progress_imaging_set_up(TRUE)
      
      shiny_fdb(in_shiny = TRUE, ui = "Imaging set up successfully saved. Proceed with experiment phases.")
      
    } else {
      
      module_progress_imaging_set_up(TRUE)
      module_progress_experiment_phases(TRUE) # set to TRUE as not needed
      
      shiny_fdb(in_shiny = TRUE, ui = "Imaging set up successfully saved. Proceed with well plate set up.")
      
    }
    
  })
  
  
  ###--- experiment phases
  
  # save and proceed
  oe <- shiny::observeEvent(input$phases_save, {
    
    all_phases <- 1:input$phases_number
        
    phase_list <- purrr::map(
      .x = all_phases,
      .f = function(p){
        
        if(p == 1){
          
          res <- time_frame_vec()[1]
          
        } else {
          
          res <- input[[stringr::str_c("phases_start", p, sep = "_")]]
          
        }
        
        base::return(res)
      
      
    }) %>% 
      purrr::set_names(nm = english::ordinal(all_phases))

    # reset well plate data
    current_layout_df(data.frame())
    well_plate_list(list())
    
    exp_design <- new_exp_design()
    
    exp_design@phases <- phase_list
    
    new_exp_design(exp_design)
    
    module_progress_experiment_phases(TRUE)
    
    shiny_fdb(in_shiny = TRUE, ui = "Experiment phases successfully saved. Proceed with well plate set up.")
    
  })
  
  
  ###--- well plate set up 
  
  # new well plate
  oe <- shiny::observeEvent(input$new_well_plate, {
    
    check_set_up_progress(progress_input = module_progress())
    
    # check input frame number per well
    checkpoint(
      evaluate = input$rois_per_well != 0,
      case_false = "invalid_roi_number"
      )
    
    # check well plate name
    wp_list <- well_plate_list()
    wp_name <- input$well_plate_name 
    
    is_new_name <- !wp_name %in% base::names(wp_list)
    is_valid_name <- validate_well_plate_name(wp_name)
    
    checkpoint(
      evaluate = base::all(is_new_name, is_valid_name),
      case_false = "invalid_wp_name"
      )
    
    # check if there is current well plate
    if(isLayoutDf(df = current_layout_df()) &&
       isModifiedLayoutDf(df = current_layout_df())){
      
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
              inputId = ns("overwrite_well_plate_confirmed"),
              label = "New Well Plate"
              ),
            act_button(
              inputId = ns("overwrite_well_plate_cancelled"),
              label = "Cancel"
              )
          )
        )
      )
      
      shiny::req(FALSE)
      
    } 
    
    # update 
    current_well_plate_name(wp_name)

    layout_df <- 
      createLayoutDf(
        well_plate_name = wp_name, 
        well_plate_type = input$well_plate_type, 
        rois_per_well = input$rois_per_well,
        n_phases = n_phases()
      )
    
    current_layout_df(layout_df)
    
  })
  
  # overwrite well plate 
  oe <- shiny::observeEvent(input$overwrite_well_plate_confirmed, {
    
    shiny::removeModal()
    
    # has been checked in observeEvent(input$new_well_plate, ...) 
    wp_name <- input$well_plate_name
    
    # update current well plate name
    current_well_plate_name(wp_name)
    
    # data.frame (obs => well)
    layout_df<- 
      createLayoutDf(
        well_plate_name = wp_name, 
        well_plate_type = input$well_plate_type, 
        n_phases = n_phases()
      )
    
    # update layout_df()
    current_layout_df(layout_df)
    
  })
  
  # dont overwrite well plate
  oe <- shiny::observeEvent(input$overwrite_well_plate_cancelled, {
    
    shiny::removeModal()
    
  })
  
  # add well information 
  oe <- shiny::observeEvent(input$add_well_info, {
    
    # check current_layout_df()
    checkpoint(evaluate = !base::identical(current_layout_df(), base::data.frame()), 
               case_false = "no_well_plate_chosen")
    
    checkpoint(evaluate = base::length(selected_wells()) != 0, 
               case_false = "no_wells_chosen")
    
    # check and assemble new info
    new_cell_line <- input$cell_line
    
    if(n_phases() > 1){
     
      new_condition <- 
        purrr::map(.x = 1:n_phases(), .f = function(p){
                     
                     input[[stringr::str_c("condition", p, sep = "_")]]
                     
                   }) %>% 
        purrr::flatten_chr()
      
    } else {
      
      new_condition <- input$conditions
      
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
    
    # set new info     
    layout_df <- 
      setWellInfo(
        df = current_layout_df(),
        selected_wells = selected_wells(), 
        cell_line = new_cell_line, 
        condition = new_condition, 
        verbose = FALSE, 
        in_shiny = TRUE
        )
    
    # update layout_df()
    current_layout_df(layout_df)
    
    # give feedback 
    shiny_fdb(in_shiny = TRUE, ui = "Well info successfully added.")
    
  })
  
  # delete well information 
  oe <- shiny::observeEvent(input$delete_well_info, {
    
    # check current_layout_df()
    checkpoint(evaluate = !base::identical(current_layout_df(), base::data.frame()), 
               case_false = "no_well_plate_chosen")
    
    checkpoint(evaluate = base::length(selected_wells()) != 0, 
               case_false = "no_wells_chosen")
    
    # -
    
    layout_df <- 
      setWellInfo(
        df = current_layout_df(), 
        selected_wells = selected_wells(), 
        cell_line = NA, 
        condition = NA, 
        verbose = FALSE, 
        in_shiny = TRUE
      )
    
    # update layout_df()
    current_layout_df(layout_df)
    
    shiny_fdb(in_shiny = TRUE, ui = "Well information successfully removed.")
    
  })
  
  # add well plate
  oe <- shiny::observeEvent(input$add_well_plate, {
    
    # check current_layout_df()
    checkpoint(
      evaluate = !base::identical(current_layout_df(), base::data.frame()), 
      case_false = "no_well_plate_chosen"
      )
    
    checkpoint(
      evaluate = isModifiedLayoutDf(df = current_layout_df()), 
      case_false = "empty_well_plate"
      )
    
    # update exp design
    exp_design <- new_exp_design()
    
    if(editing_well_plate()){
      
      well_plate <- 
        setLayoutDf(
          object = current_well_plate(),
          layout_df = current_layout_df()
          )
      
      exp_design <- 
        setWellPlate(
          object = exp_design, 
          well_plate = well_plate
        )
      
    } else {
      
      exp_design <- 
        addWellPlate(
          object = exp_design, 
          layout = current_layout_df(), 
          name = current_well_plate_name()
        )
      
    }
    
    new_exp_design(exp_design)
    
    # update reactive values
    current_layout_df(data.frame())
    current_well_plate(FALSE)
    current_well_plate_name(character(1))
    
  })
  
  # save and proceed
  oe <- shiny::observeEvent(input$save_and_proceed, {
    
    checkpoint(
      evaluate = base::length(well_plate_list()) != 0,
      case_false = "no_well_plates_added"
      )
    
    cypro_object <- 
      setExperimentDesign(
        object = new_cypro_object(),
        exp_design = new_exp_design()
      )
    
    output_list$object <- cypro_object
    output_list$proceed <- TRUE
    
    shiny_fdb(
      in_shiny = TRUE,
      ui = "Experiment design successfully saved. Click on 'Return Cypro Object' and proceed with loadData()."
      )
    
    
  })
  
  # -----
  
  ###--- save experiment design and proceed
  
  oe <- shiny::observeEvent(input$edit_well_plate, {
    
    ns <- session$ns
    
    if(isLayoutDf(df = current_layout_df()) &&
       isModifiedLayoutDf(current_layout_df())){
      
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
              inputId = ns("overwrite_well_plate2_confirmed"),
              label = "Edit Well Plate"
              ),
            act_button(
              inputId = ns("overwrite_well_plate2_cancelled"),
              label = "Cancel"
              )
          )
        )
      )
      
    } else {
      
      wp_name <- input$well_plate_select
      
      well_plate <- 
        getWellPlate(
          object = new_exp_design(),
          well_plate = wp_name
        )
      
      layout_df <- 
        getLayoutDf(
          object = well_plate, 
          well_plate = wp_name
        )
      
      current_layout_df(layout_df)
      current_well_plate(well_plate)
      current_well_plate_name(wp_name)
      
    }
    
  })
  
  # overwrite the well plate that is currently being designed to edit 
  # a well plate that has been saved already 
  oe <- shiny::observeEvent(input$overwrite_well_plate2_confirmed, {
    
    shiny::removeModal()
    
    wp_name <- input$well_plate_select
    
    layout_df <- 
      getLayoutDf(
        object = exp_design(), 
        well_plate = wp_name
      )
    
    well_plate <- 
      getWellPlate(object = new_exp_design(), well_plate = wp_name)
    
    current_layout_df(layout_df)
    current_well_plate(well_plate)
    current_well_plate_name(wp_name)
    
  })
  
  oe <- shiny::observeEvent(input$overwrite_well_plate2_cancelled, {
    
    shiny::removeModal()
    
  })
  
  oe <- shiny::observeEvent(input$delete_well_plate, {
    
    ns <- session$ns
    
    shiny::showModal(
      ui = shiny::modalDialog(
        shiny::helpText(
          glue::glue(
            "A deleted well plate can not be restored and has to be designed ",
            "again. Are you sure you want to remove well plate '{input$well_plate_select}'?"
          )
        ),
        footer = shiny::tagList(
          act_button(inputId = ns("delete_well_plate_confirmed"), label = "Yes, delete well plate"),
          act_button(inputId = ns("delete_well_plate_cancelled"), label = "No, cancel")
        )
      )
    )
    
  })
  
  oe <- shiny::observeEvent(input$delete_well_plate_confirmed, {
    
    shiny::removeModal()
    
    exp_design <- 
      discardWellPlate(
        object = new_exp_design(), 
        well_plate = input$well_plate_select, 
        verbose = FALSE
      )
    
    new_exp_design(exp_design)
    
  })
  
  oe <- shiny::observeEvent(input$delete_well_plate_cancelled, {
    
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
  
  exp_type <- shiny::eventReactive(input$overall_info_save, {
    
    base::class(new_cypro_object())
        
  })
  
  multiple_phases <- shiny::reactive({
    
    methods::is(new_cypro_object(), "CyproTimeLapseMP")
    
  })
  
  ###--- imaging set up 
  time_vec <- shiny::reactive({
    
    make_time_vec(
      n_frames = input$n_frames, 
      interval = input$interval, 
      interval_unit = input$interval_unit
      )
    
  })
  
  frame_vec <- shiny::reactive({
    
    make_frame_vec(n_frames = input$n_frames)
    
  })
  
  time_frame_vec <- shiny::reactive({
    
    make_time_frame_vec(
      time_vec = time_vec(), 
      frame_vec = frame_vec()
    )
    
  })
  
  ###--- experiment phases 
  
  all_phases <- shiny::reactive({
    
    shiny::req(new_exp_design())
    
    new_exp_design()@phases
    
  })
  
  n_phases <- shiny::reactive({
    
    shiny::req(all_phases())
    
    base::length(all_phases())
    
  })
  
  
  ###--- well plate set up
  
  # well plate list 
  
  well_plate_list <- shiny::reactive({
    
    shiny::req(new_exp_design())
    
    new_exp_design()@well_plates
    
  })
  
  well_plate_names <- shiny::reactive({
    
    base::names(well_plate_list())
    
  })
  
  editing_well_plate <- shiny::reactive({
    
    current_well_plate_name() %in% well_plate_names()
    
  })
  
  selected_wells_layout_df <- shiny::reactive({
    
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
      
      selected_wells_layout_df <- 
      dplyr::filter(current_layout_df(),
        dplyr::between(col_num, xmin, xmax),
        dplyr::between(row_num, ymin, ymax)
        )
      
    } 
    
    base::return(selected_wells_layout_df)
    
  })
  
  selected_wells <- shiny::reactive({
    
    selected_wells_layout_df()$well
    
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
      dplyr::filter(.data = current_layout_df(), row_num == yh & col_num == xh) %>% 
      dplyr::ungroup()
    
    shiny::validate(
      shiny::need(
        expr = base::nrow(hovered_well) == 1, 
        message = "Hover over a well to have it's information printed here."
      )
    )
      
    if(multiple_phases()){
      
      df_res <- hovered_well_info_shiny_mp(df = hovered_well)
      
    } else {
      
      df_res <- hovered_well_info_shiny(df = hovered_well)
      
    }
    
    base::return(df_res)
    
  })
  
  layout_df2 <- shiny::reactive({
    
    df <- 
      getLayoutDf(
        object = new_exp_design(),
        well_plate = input$well_plate_select
        )
    
    return(df)
    
  })
  
  # well plate visualization 
  
  well_plate_plot <- shiny::reactive({
    
    shiny::validate(
      shiny::need(
        expr = !base::identical(current_layout_df(), base::data.frame()),
        message = "No well plate chosen.")
      )
    
    plot_well_plate_shiny(
      wp_df = current_layout_df(),
      selected_wells_df = selected_wells_layout_df(),
      aes_fill = "condition",
      aes_color = "info_status",
      color_values = status_colors
      ) +
      ggplot2::labs(
        color = "Info Status",
        subtitle = stringr::str_c("Name:", current_well_plate_name(), sep = " ")
        ) 
    
  })
  
  well_plate_plot2 <- shiny::reactive({
    
    shiny::req(well_plate_names())
    shiny::req(input$well_plate_select %in% well_plate_names())
    
    
    plot_well_plate_shiny(
      wp_df = layout_df2(),
      selected_wells_df = NULL,
      aes_fill = input$well_plate_plot2_color, 
      aes_color = "info_status",
      color_values = status_colors
    ) +
      ggplot2::labs(
        color = "Info Status",
        subtitle = stringr::str_c("Name:", input$well_plate_select, sep = " ")
      ) 
    
  })
  
  # -----
  
# Plot outputs ------------------------------------------------------------
  
  output$well_plate_plot <- shiny::renderPlot({
    
    well_plate_plot()
    
  })
  
  output$well_plate_plot2 <- shiny::renderPlot({
    
    well_plate_plot2()
    
  })
  
  # -----
  
# Table outputs -----------------------------------------------------------
  
  output$well_info <- shiny::renderTable({
    
    hovered_well_info()
    
  })
  
  output$well_plate_folders <- DT::renderDataTable({
    
    
    df <- 
      data.frame(
        "Name" = base::names(well_plate_list()),
        "Type" = purrr::map_chr(.x = well_plate_list(), ~ base::unique(.x@layout[["well_plate_type"]])), 
        "ROIS per Well" = purrr::map_dbl(.x = well_plate_list(), ~ base::unique(.x@layout[["rois_per_well"]]))
      )
    
    base::rownames(df) <- NULL
    
    return(df)
    
  })
  
  # -----
  

  
  # -----
  
  
# Module return value -----------------------------------------------------
  
  base::return(output_list)
  
  })
  
  # -----

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



