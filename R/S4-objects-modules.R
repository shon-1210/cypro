#' @include S4-objects-variables.R


# screening modules -------------------------------------------------------

module_identification <- 
  methods::new(
    Class = "AnalysisModule", 
    active = TRUE,
    descr = analysis_module_descr$identification,
    descr_short = analysis_module_descr_short$identification,
    name_in_app = "Cell Identification", 
    name_in_cypro = "identification",
    variables_optional = list(
      well_plate = var_well_plate, 
      well = var_well,
      roi = var_roi, 
      well_roi = var_well_roi
    ),
    variables_required = list(
      cell_id = var_cell_id
    ) 
    
  )

module_localisation <- 
  methods::new(
    Class = "AnalysisModule",
    active = TRUE,
    descr = analysis_module_descr$localisation,
    descr_short = analysis_module_descr_short$localisation,
    name_in_app = "Localisation", 
    name_in_cypro = "localisation", 
    variables_computable = list(), 
    variables_required = list(
      x_coords = var_x_coords, 
      y_coords = var_y_coords
    )
  )



# time lapse modules ------------------------------------------------------


module_identification_timelapse <- 
  methods::new(
    Class = "AnalysisModuleTimeLapse", 
    active = TRUE, # must be used
    check_fns = list(
      cell_id_frame_num = check_module_cell_id_frame_num
    ),
    descr = stringr::str_c(analysis_module_descr$identification, " ", analysis_module_descr$identification_timelapse), 
    descr_short = stringr::str_c(analysis_module_descr_short$identification, " ", analysis_module_descr_short$identification_timelapse),
    name_in_app = "Cell Identification (time lapse)", 
    name_in_cypro = "identification_timelapse", 
    variables_optional = list(
      well_plate = var_well_plate, 
      well = var_well, 
      roi = var_roi, 
      well_roi = var_well_roi
    ),
    variables_required = list(
      cell_id = var_cell_id, 
      frame_num = var_frame_num
    )
  )

module_migration_localisation <- 
  methods::new(
    Class = "AnalysisModuleTimeLapse", 
    active = TRUE,
    descr = analysis_module_descr$migration_localisation,
    descr_short = analysis_module_descr_short$migration_localisation,
    name_in_app = "Localisation & Migration",
    name_in_cypro = "migration_localisation",
    variables_computable = list(
      aflp = var_aflp, 
      afo = var_afo,
      dflp = var_dflp,
      dfo = var_dfo, 
      speed = var_speed
    ),
    variables_required = list(
      x_coords = var_x_coords,
      y_coords = var_y_coords
    ),
    variables_summary = list(
      total_dist = var_total_dist, 
      mgr_eff = var_mgr_eff
    )
  )



# all modules  ------------------------------------------------------------

cypro_modules <- 
  list(
    screening = list(
      "identification" = module_identification,
      "localisation" = module_localisation
    ),
    time_lapse = list(
      "identification_timelapse" = module_identification_timelapse,
      "migration_localisation" = module_migration_localisation
    )
  )

cypro_module_names <- purrr::map(.x = cypro_modules, .f = ~ base::names(.x))


