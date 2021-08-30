


join_well_plate_data_and_input_timelapse_shiny <- function(input_df,
                                                           well_plate_list, 
                                                           module_vars,
                                                           ed_vars, 
                                                           amils,
                                                           object){
  
  
  all_vars <- c(module_vars)
  
  all_var_name_pairs <- 
    get_all_var_name_pairs(amils = amils)
  
  print(all_vars)
  print(all_var_name_pairs)
  
  df_list <- 
    purrr::map(.x = base::names(well_plate_list), .f = function(wp_name){

      df <- 
        dplyr::select(input_df, dplyr::starts_with("well"), dplyr::all_of(all_vars)) %>% 
        dplyr::filter(well_plate_name == {{wp_name}}) %>% 
        dplyr::rename(!!!all_var_name_pairs)
      
      return(df)
      
    }) %>% 
    purrr::set_names(nm = base::names(well_plate_list))
  
  all_data_list <- 
    list(df_list, well_plate_list, base::seq_along(df_list), base::names(df_list))
  
  # join well plate information and create final cell id
  df <- purrr::pmap_dfr(.l = all_data_list, .f = hlpr_assemble_df)
  
  cell_lines <- df$cell_line %>% base::unique() %>% base::sort()
  conditions <- df$condition %>% base::unique() %>% base::sort()
  cl_conditions <- df$cl_condition %>% base::unique() %>% base::sort()
  
  final_df <-
    dplyr::mutate(
      .data = df, 
      cell_line = base::factor(x = cell_line, levels = cell_lines),
      condition = base::factor(x = condition, levels = conditions),
      cl_condition = base::factor(x = cl_condition, levels = cl_conditions)
    )
  
  phases <- object@set_up$phases
  
  measurements <- object@set_up$measurement_string
  
  conditions <- 
    stringi::stri_split(str = base::unique(final_df$condition), regex = "->")
  
  phase_starts <-
    purrr::map_dbl(.x = phases, .f = ~ base::which(x = measurements == .x))
  
  # if more than one phase exists return list of data.frames named by phase
  if(base::length(phase_starts) > 1){
    
    phase_endings <- 
      phase_starts[2:base::length(phase_starts)] %>% 
      base::append(x = ., values = base::max(final_df$frame_num)) %>% 
      purrr::set_names(nm = base::names(phases))
    
    phase_info <-
      purrr::pmap(
        .l = list(x = phase_starts, y = phase_endings, z = base::seq_along(phases)),
        .f = function(x, y, z){ list("start" = x, "end" = y, "index" = z)}
      ) %>% 
      purrr::set_names(nm = base::names(phases))
    
    track_list <- 
      purrr::map(.x = phase_info, .f = function(phase){
        
        phase_index <- phase$index
        phase_start <- phase$start
        phase_end <- phase$end
        
        phase_pattern <- stringr::str_c("^", phase_index, "\\.",sep = "")
        
        df <-
          dplyr::filter(final_df, frame_num >= {{phase_start}} & frame_num < {{phase_end}})
        
        condition_split <- 
          stringr::str_split_fixed(
            string = df$condition,
            pattern = " -> ", 
            n = base::length(phase_endings)
          ) 
        
        condition_vec <- 
          stringr::str_remove_all(
            string = base::as.character(condition_split[,phase_index]),
            pattern = phase_pattern
          )
        
        df$condition <- condition_vec
        df$cl_condition <- stringr::str_c(df$cell_line, df$condition, sep = " & ")
        
        base::return(df)
        
      }) %>% 
      magrittr::set_names(value = base::names(phase_endings))
    
  } else { # if only one phase return list of one data.frame named "first"
    
    final_df$phase = "first"
    
    track_list <- base::list("first" = final_df)
    
  }
  
  base::return(track_list)
  
}


join_well_plate_data_and_input_one_time_imaging_shiny <- function(input_df, 
                                                                  well_plate_list, 
                                                                  module_vars, 
                                                                  ed_vars,
                                                                  amils,
                                                                  object){
  
  ed_vars_vec <- purrr::flatten_chr(ed_vars)
  
  all_vars <- c(ed_vars_vec, module_vars, "well_roi")
  
  all_var_name_pairs <- 
    get_all_var_name_pairs(amils = amils)
  
  df_list <- 
    purrr::map(.x = base::names(well_plate_list), .f = function(wp_name){
      
      wp_var <- ed_vars$wp
      roi_var <- ed_vars$roi
      well_var <- ed_vars$well
      
      df <- 
        dplyr::select(input_df, dplyr::all_of(all_vars)) %>% 
        dplyr::filter(!!rlang::sym(wp_var) == {{wp_name}}) %>% 
        dplyr::select(-dplyr::all_of(x = c(roi_var, well_var))) %>% 
        dplyr::rename(!!!all_var_name_pairs)
      
      return(df)
      
    }) %>% 
    purrr::set_names(nm = base::names(well_plate_list))
  
  all_data_list <- 
    list(df_list, well_plate_list, base::seq_along(df_list), base::names(df_list))
  
  df <- purrr::pmap_dfr(.l = all_data_list, .f = hlpr_assemble_df)
  
  cell_lines <- df$cell_line %>% base::unique() %>% base::sort()
  conditions <- df$condition %>% base::unique() %>% base::sort()
  cl_conditions <- df$cl_condition %>% base::unique() %>% base::sort()
  
  final_df <-
    dplyr::mutate(
      .data = df, 
      cell_line = base::factor(x = cell_line, levels = cell_lines), 
      condition = base::factor(x = condition, levels = conditions), 
      cl_condition = base::factor(x = cl_condition, levels = cl_conditions)
    )
  
  stat_list <- base::list("only" = final_df)
  
  base::return(stat_list)
  
}




get_all_var_name_pairs <- function(amils){
  
  id_name_pairs <- 
    purrr::map(
      .x = amils$identifier$variables, 
      .f = ~ .x[["name_in_example"]]
    ) %>% 
    purrr::flatten_chr() %>% 
    purrr::set_names(nm = base::names(amils$identifier$variables))
  
  module_name_pairs <- 
    purrr::map(
      .x = amils$analysis_modules, 
      .f = function(module){
        
        name_pairs <- 
          purrr::map_chr(
            .x = module$variables, 
            .f = ~ .x[["name_in_example"]]
          ) %>%
            purrr::set_names(nm = base::names(module$variables)) %>% 
            purrr::discard(.p = ~ .x == "not selected") 
        
        return(name_pairs)
        
      }
    ) %>% 
    purrr::flatten_chr()
  
  all_var_name_pairs <- c(id_name_pairs, module_name_pairs)
  
  return(all_var_name_pairs)
  
}


