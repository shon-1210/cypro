
#' @title Add helping text 
#'
add_helper <- function(shiny_tag, content, title = "What do I have to do here?", type = "inline", size = "s", ...){
  
  
  res <- 
    shinyhelper::helper(shiny_tag = shiny_tag, 
                        content = content, 
                        title = title, 
                        size = size,
                        type = type, 
                        ...)
  
  base::return(res)
  
}

# variable picker inputs for prepare data loading -------------------------

variable_description <- function(variable_info){
  
  if("relevance" %in% base::names(variable_info)){
    
    description <- 
      c(
        variable_info$descr, 
        "",
        glue::glue(
          "Within cypro and all its functions the variable selected as ",
          "'{variable_info$name_in_app}' is renamed and referred to as '",
          "'{variable_info$name_in_cypro}'."
        ), 
        "",
        stringr::str_c(variable_relevance_descr[[variable_info$relevance]]),
        "",
        stringr::str_c("Requirement: ", variable_info$requirements)
      )
    
  } else {
    
    description <- 
      c(
        variable_info$descr, 
        "",
        glue::glue(
          "Variables selected as '{variable_info$name_in_app}' keep their name ",
          "until you rename them manually."
        ), 
        "",
        stringr::str_c("Requirement: ", variable_info$requirements)
      )
    
  }
  
}

variable_picker <- function(variable_info,
                            example_df,
                            identifier_names = base::character(1),
                            exclude_from_choices = NULL,
                            module_suffix = NULL, 
                            ns = NULL,
                            multiple = FALSE,
                            width = 3,
                            ...){
  
  shiny::req(example_df)
  
  check_fun <- variable_info$check_candidate
  
  if(base::is.character(exclude_from_choices)){
    
    example_df <- 
      dplyr::select(example_df, -dplyr::all_of(exclude_from_choices))
    
  }
  
  # assemble choices 
  choices <-
    dplyr::select_if(example_df, .predicate = check_fun) %>% 
    base::colnames()
  
  # variable$relevance NULL if variable info is info type "general"
  relevance <- variable_info$relevance
  
  general_info <-
    base::ifelse(base::is.null(relevance), TRUE, FALSE)
  
  
  # 'not selected' as default for computable specific variables 
  if(base::isFALSE(general_info) && relevance == "computable"){
    
    choices <- c("not selected", choices)
    selected <- "not selected"
    
  } else {
    
    selected <- choices[1]
    
  }
  
  # 'Rownumber' as additional option for cell id in non time lapse experiments ?
  if(FALSE){
    
    choices <- c("Rownumber", choices)
    
  }
  
  # assemble input id
  input_id <-
    stringr::str_c("vardenotation-", variable_info$name_in_cypro) %>% 
    stringr::str_c(., module_suffix, sep = "-")
  
  if(!base::is.null(ns)){
    
    input_id <- ns(input_id)
    
  }
  
  html_output <- 
    shiny::tagList(
      
      shiny::column(
        width = width,
        shinyWidgets::pickerInput(
          inputId = input_id, 
          label = variable_info$name_in_app,
          choices = choices, 
          options = list(`live-search` = TRUE, `actions-box` = TRUE),
          selected = selected,
          multiple = multiple
        ) %>% 
          add_helper(
            title = variable_info$name_in_app,
            content = variable_description(variable_info), 
            size = "m"
          )
        
      )
      
    )
  
  return(html_output)
  
}

# -----

# Prepare data loading Step 2 ---------------------------------------------

# main html generator

pdl_step_2_identifier_variables_html <- function(object, example_df, ns){
  
  if(isTimeLapseExp(object)){
    
    help_text <- 
      stringr::str_c(
        "Data derived from time lapse experiments needs ",
        "a cell ID variable and frame variable ",
        "to identify observations as cells at a given time point."
      )
    
    frame_picker_input <- 
      variable_picker(
        variable_info = var_frame_num, 
        example_df = example_df, 
        ns = ns, 
        width = 3
      )
    
  } else {
    
    help_text <- 
      stringr::str_c(
        "Data derived from one time imaging experiments only need ",
        "a cell ID variable."
      )
    
    frame_picker_input <- shiny::tagList()
    
  }
  
  cell_id_picker_input <- 
    variable_picker(
      variable_info = var_cell_id, 
      example_df = example_df, 
      ns = ns,
      width = 3
    )
  
  html_output <- 
    shiny::tagList(
      #shiny::fluidRow(
      #  hs(12, shiny::h4(shiny::strong("Step 2: Denote identifier variables")))
      #),
      shiny::fluidRow(
        hs(12, shiny::helpText(help_text))
      ),
      shiny::fluidRow(
        cell_id_picker_input, 
        frame_picker_input
      ), 
      shiny::fluidRow(
        hs(12, shiny::actionButton(inputId = ns("pdl_id_vars_save"), label = "Save & Proceed"))
      )
    )
  
  return(html_output)
    
}

# -----

# Prepare data loading step 3 ---------------------------------------------

# main html generator 

pdl_step_3_module_variables_html <- function(object, 
                                        example_df, 
                                        ns = NULL,
                                        exclude_from_choices = NULL){
  
  if(isTimeLapseExp(object)){
    
    valid_cypro_modules <- 
      purrr::keep(.x = cypro_modules, .p = ~ .x$exp_type %in% c("time_lapse", "both"))
    
  } else {
    
    valid_cypro_modules <- 
      purrr::keep(.x = cypro_modules, .p = ~ .x$exp_type %in% c("one_time_imaging", "both"))
    
  }
  
  help_text <- glue::glue(
    "To use certain functions in cypro the respective variables must be denoted. ",
    "If you do not want to use a module (e.g. because your data does not contain the ",
    "required variables) turn off the module by clicking on 'Use this module'. This makes 
    cypro ignore the modules requirements."
  )
  
  html_output_modules <-
    purrr::imap(
      .x = base::names(valid_cypro_modules), 
      .f = make_module_html, 
      example_df = example_df, 
      ns = ns, 
      exclude_from_choices = exclude_from_choices
    )
  
  html_output <- 
    shiny::tagList(
     # shiny::fluidRow(
     #   hs(12, shiny::h4(shiny::strong("Step 3: Denote module specific variables")))  
     # ),
      shiny::helpText(help_text),
      html_output_modules,
      shiny::fluidRow(
        hs(12, shiny::actionButton(inputId = ns("pdl_module_vars_save"), label = "Save & Proceed"))
      )
    )
  
  return(html_output)  
  
}

# helping html generators
make_module_heading <- function(module_info, nth){
  
  heading <- stringr::str_c(nth, ". ", module_info$pretty_name)
  
  shiny::tagList(
    shiny::fluidRow(
      hs(12, shiny::h5(shiny::strong(heading)))
    ), 
    shiny::fluidRow(
      hs(12, shiny::helpText(module_info$helper_text))
    )
  )
  
}

make_module_variable_pickers <- function(module_info,
                                         example_df,
                                         ns = NULL,
                                         exclude_from_choices = NULL){
  
  needed_variables <- 
    purrr::keep(.x = module_info$variables, .p = ~ .x$relevance == "needed") %>% 
    base::names()
  
  computable_variables <- 
    purrr::keep(.x = module_info$variables, .p = ~ .x$relevance == "computable") %>% 
    base::names()
  
  # html for needed variables of module if it has some
  if(base::length(needed_variables) != 0){
    
    needed_vars_html <- 
      purrr::map(.x = needed_variables, .f = function(var_name){
        
        variable_picker(
          variable_info = module_info$variables[[var_name]], 
          example_df = example_df, 
          ns = ns, 
          multiple = FALSE, 
          module_suffix = module_info$name,
          width = 3,
          exclude_from_choices = exclude_from_choices
        )
        
      }) %>% 
      organize_picker_input_in_rows()
    
  } else {
    
    needed_vars_html <- shiny::tagList()
    
  }
  
  # html for computed variables of module if it has some
  if(base::length(computable_variables) != 0){
    
    computable_vars_html <- 
      purrr::map(.x = computable_variables, .f = function(var_name){
        
        variable_picker(
          variable_info = module_info$variables[[var_name]], 
          example_df = example_df, 
          ns = ns, 
          multiple = FALSE, 
          module_suffix = module_info$name,
          width = 3,
          exclude_from_choices
        )
        
      }) %>% 
      organize_picker_input_in_rows()
    
  } else {
    
    computable_vars_html <- shiny::tagList()
    
  }
  
  html_output <- 
    shiny::tagList(
      needed_vars_html, 
      computable_vars_html
    )
  
  return(html_output)
  
  
}

organize_picker_input_in_rows <- function(tag_list){
  
  n_pickers <- length(tag_list)
  
  seq_pickers <- base::seq(1, n_pickers, 4)
  
  html_output <- 
    purrr::map(
      .x = seq_pickers, 
      .f = function(nth){
        
        # 1:4, 5:8, etc.
        selected_pickers <-
          purrr::discard(tag_list[nth:(nth+3)], .p = base::is.null) %>% 
          shiny::tagList()
        
        # return tag list of up to four pickers in row
        shiny::fluidRow(selected_pickers)
        
      }
    ) %>% 
    shiny::tagList()
  
  return(html_output)
  
}

make_module_html <- function(module_name,
                             nth,
                             example_df,
                             ns = NULL,
                             exclude_from_choices = NULL){
  
  heading <-
    make_module_heading(
      module_info = cypro_modules[[module_name]], 
      nth = nth
    )
  
  pickers <- 
    make_module_variable_pickers(
      module_info = cypro_modules[[module_name]], 
      example_df = example_df, 
      ns = ns,
      exclude_from_choices = exclude_from_choices
    )
  
  usage_switcher <- 
    make_usage_switcher(
      module_info = cypro_modules[[module_name]], 
      ns = ns
    )
  
  html_output <- 
    shiny::tagList(
      heading, 
      pickers, 
      usage_switcher,
      shiny::HTML("<br>")
    )
  
  return(html_output)
  
}

make_usage_switcher <- function(module_info, ns = NULL){
  
  # add module suffix
  input_id <- 
    stringr::str_c("module_usage", module_info$name, sep = "-")
  
  if(!base::is.null(ns)){
    
    input_id <- ns(input_id)
    
  }
  
  shiny::tagList(
    shinyWidgets::materialSwitch(
      inputId = input_id,
      label = "Use this module:", 
      status = "success", 
      value = TRUE
    )
  )
  
  
}

# ----



# Prepare data loadting step 4 --------------------------------------------

pdl_step_4_additional_variables_html <- function(object,
                                            example_df,
                                            ns,
                                            exclude_from_choices = NULL){
  
  help_text <- 
      shiny::helpText(
        "Choose the variables that contain the information you want to analyze later on. "
      )
  
  grouping_picker <-
    variable_picker(
      variable_info = general_grouping_variable_info,
      example_df = example_df, 
      ns = ns, 
      width = 3, 
      multiple = TRUE, 
      exclude_from_choices = exclude_from_choices
    )
  
  numeric_picker <- 
    variable_picker(
      variable_info = general_numeric_variable_info,
      example_df = example_df, 
      ns = ns,
      width = 3,
      multiple = TRUE,
      exclude_from_choices = exclude_from_choices
    )
  
  html_output <- 
    shiny::tagList(
      #shiny::fluidRow(
      #  hs(12, shiny::h4(shiny::strong("Step 4: Denote additional variables of interest"))), 
      #),
      shiny::fluidRow(
        hs(12, shiny::helpText(help_text))
      ),
      shiny::fluidRow(
        numeric_picker, 
        grouping_picker
      ),
      shiny::fluidRow(
        hs(12, shiny::actionButton(inputId = ns("pdl_add_vars_save"), label = "Save & Proceed"))
      )
    )
  
  return(html_output)
  
}

# -----


























