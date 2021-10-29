




#' @param variable An S4 object of class \code{AssignableVariable}.

write_html_variable_picker <- function(variable,
                                       example_df,
                                       identifier_names = base::character(1),
                                       exclude_from_choices = NULL,
                                       module_suffix = NULL, 
                                       ns = NULL,
                                       multiple = FALSE,
                                       width = 3,
                                       ...){
  
  shiny::req(example_df)
  
  check_fun <- variable@check_candidate
  
  if(base::is.character(exclude_from_choices)){
    
    example_df <- 
      dplyr::select(example_df, -dplyr::any_of(exclude_from_choices))
    
  }
  
  # assemble choices 
  choices <-
    dplyr::select_if(example_df, .predicate = check_fun) %>% 
    base::colnames()
  
  if(!methods::is(variable, class2 = "RequiredVariable")){
    
    choices <- c(NA_character_, choices)
    selected <- NA_character_
    
  } else {
    
    selected <- choices[1]
    
  }
  
  # assemble input id
  input_id <-
    stringr::str_c("vardenotation-", variable@name_in_cypro) %>% 
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
          label = variable@name_in_app,
          choices = choices, 
          options = list(`live-search` = TRUE, `actions-box` = TRUE),
          selected = selected,
          multiple = multiple
        ) %>% 
          add_helper(
            title = variable@name_in_app,
            content = write_variable_description(variable), 
            size = "m"
          )
        
      )
      
    )
  
  return(html_output)
  
}

write_html_variable_assignment_modules <- function(object,
                                                   example_df,
                                                   ns = NULL,
                                                   exclude_from_choices = NULL){
  
  if(isTimeLapseExp(object)){
    
    valid_cypro_modules <- cypro_modules$time_lapse
    
  } else {
    
    valid_cypro_modules <- cypro_modules$screening
    
  }
  
  help_text <- glue::glue(
    "To use certain functions in cypro the respective variables must be denoted. ",
    "If you do not want to use a module (e.g. because your data does not contain the ",
    "required variables) turn off the module by clicking on 'Use this module'. This makes 
    cypro ignore the modules requirements."
  )
  
  html_output_modules <-
    purrr::map2(
      .x = valid_cypro_modules,
      .y = base::seq_along(valid_cypro_modules),
      .f = write_html_variable_assignment_module, # != write_html_variable_assignment_moduleS  
      example_df = example_df, 
      ns = ns, 
      exclude_from_choices = exclude_from_choices
    )
  
  html_output <- 
    shiny::tagList(
      #shiny::helpText(help_text),
      html_output_modules,
      shiny::fluidRow(
        hs(12, shiny::actionButton(inputId = ns("module_vars_save"), label = "Save & Proceed"))
      )
    )
  
  return(html_output)
  
}



write_html_variable_assignment_module <- function(module,
                                                  nth,
                                                  example_df,
                                                  ns = NULL,
                                                  exclude_from_choices = NULL){
  
  heading <-
    write_html_module_heading(
      module = module, 
      nth = nth
    )
  
  pickers <- 
    write_html_module_variable_pickers(
      module = module, 
      example_df = example_df, 
      ns = ns,
      exclude_from_choices = exclude_from_choices
    )
  
  if(!module@name_in_cypro %in% required_modules){
    
    usage_switcher <- 
      write_html_usage_switcher(
        module = module, 
        ns = ns
      )
    
  } else {
    
    usage_switcher <- shiny::tagList()
    
  }
  
  html_output <- 
    shiny::tagList(
      heading, 
      pickers, 
      usage_switcher,
      shiny::HTML("<br>")
    )
  
  return(html_output)
  
}



write_html_module_heading <- function(module, nth){
  
  heading <- stringr::str_c(nth, ". ", module@name_in_app)
  
  shiny::tagList(
    shiny::fluidRow(
      hs(4, shiny::h4(shiny::strong(heading)) %>% add_helper(content = module@descr, title = module@name_in_app)) 
    ), 
    shiny::fluidRow(
      hs(12, shiny::helpText(module@descr_short))
    )
  )
  
}

write_html_module_variable_pickers <- function(module,
                                               example_df,
                                               ns = NULL,
                                               exclude_from_choices = NULL){
  
  required_vars_html <- 
    write_html_module_variable_pickers_hlpr(
      var_list = module@variables_required, 
      example_df = example_df, 
      ns = ns, 
      exclude_from_choices = exclude_from_choices, 
      module = module
      )
  
  optional_vars_html <- 
    write_html_module_variable_pickers_hlpr(
      var_list = module@variables_optional,
      example_df = example_df, 
      ns = ns, 
      exclude_from_choices = exclude_from_choices,
      module = module
      )
  
  computable_vars_html <- 
    write_html_module_variable_pickers_hlpr(
      var_list = module@variables_computable,
      example_df = example_df, 
      ns = ns, 
      exclude_from_choices = exclude_from_choices,
      module = module
      )
  
  html_output <- 
    shiny::tagList(
      required_vars_html, 
      optional_vars_html,
      computable_vars_html
    )
  
  return(html_output)
  
  
}

write_html_module_variable_pickers_hlpr <- function(var_list, example_df, ns, exclude_from_choices, module){
  
  if(base::length(var_list) != 0){
    
    html_out <- 
      purrr::map(.x = var_list, .f = function(variable){
        
        write_html_variable_picker(
          variable = variable, 
          example_df = example_df, 
          ns = ns, 
          multiple = FALSE, 
          module_suffix = module@name_in_cypro,
          width = 3,
          exclude_from_choices = exclude_from_choices
        )
        
      }) %>% 
      write_html_organize_picker_input_in_rows()
    
  } else {
    
    html_out <- shiny::tagList()
    
  }
  
  return(html_out)
  
  
}


write_html_organize_picker_input_in_rows <- function(tag_list){
  
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

write_html_usage_switcher <- function(module, ns = NULL){
  
  input_id <- 
    stringr::str_c("module_usage", module@name_in_cypro, sep = "-")
  
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






