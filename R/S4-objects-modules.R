


# time lapse modules ------------------------------------------------------

module_migration <- 
  methods::new(
    Class = "AnalysisModuleTimeLapse", 
    descr = "Quantifies locomotion of cells in time lapse experiments.", 
    name_in_app = "Migration",
    name_in_cypro = "migration",
    variables_computed = list(
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
    variables_summarized = list(
      total_dist = var_total_dist, 
      mgr_eff = var_mgr_eff
    )
  )
