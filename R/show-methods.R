




# Class: CyproSubsetBy ----------------------------------------------------

setMethod(f = "show", signature = "CyproSubsetByCellID", definition = function(object){
  
  nth <- english::ordinal(object@nth) %>% make_capital_letters()
  
  nth_text <- glue::glue("\n{nth} subsetting: By Cell ID")
  
  # ----
  
  basic_subset_text <- write_basic_subset_text(object)
  
  base::writeLines(
    c(nth_text, 
      "",
      basic_subset_text
    )
  )
  
  
})

setMethod(f = "show", signature = "CyproSubsetByNumber", definition = function(object){
  
  nth <- english::ordinal(object@nth) %>% make_capital_letters()
  
  option <- object@option
  
  nth_text <- glue::glue("{nth} subsetting: By Number (option '{option})'")
  
  # -----
  
  basic_text <- write_basic_subset_text(object)
  
  # -----
  
  across <- scollapse(object@across)
  across_text <- glue::glue("Across: '{across}'")
  
  # -----
  
  if(option == "groupwise"){
    
    option_text <- glue::glue("Number by Group: {object@n_by_group}")
    
  } else {
    
    option_text <- glue::glue("Number total: {object@n_total}")
    
  }
  
  # -----
  
  seed_text <- glue::glue("Seed: {object@seed}")
  
  base::writeLines(
    c(
      nth_text, 
      "",
      basic_text, 
      across_text, 
      option_text, 
      seed_text
    )
  )
  
})

setMethod(f = "show", signature = "CyproSubsetByGroup", definition = function(object){
  
  nth <- english::ordinal(object@nth) %>% make_capital_letters()
  
  nth_text <- glue::glue("{nth} subsetting: By Group'")
  
  # -----
  
  basic_text <- write_basic_subset_text(object)
  
  # -----
  
  grouping_variable <- scollapse(object@grouping_variable)
  gr_var_text <- glue::glue("Grouping: '{grouping_variable}'")
  
  # -----
  
  groups <- scollapse(object@groups)
  groups_text <- glue::glue("Groups: '{groups}'")
  
  # -----
  
  base::writeLines(
    c(
      nth_text, 
      "",
      basic_text, 
      gr_var_text, 
      groups_text
    )
  )
  
})