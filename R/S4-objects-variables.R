#' @include compute.R logical-tests.R r-objects.R check.R
#' 
NULL



# computable variables ----------------------------------------------------

var_afo <- 
  methods::new(
    Class = "ComputableVariable", 
    check_candidate = is_numeric_candidate,
    check_var = check_var_angle,
    compute_with = function(){}, 
    descr_requirements = req_numeric_var, 
    descr_variable = "Angles in degree (between 0° and 359°) between position of first frame and from current one.", 
    name_in_app = "Angle from origin", 
    name_in_cypro = "afo", 
    name_in_example = NA_character_
  )

var_aflp <- 
  methods::new(
    Class = "ComputableVariable", 
    check_candidate = is_numeric_candidate,
    check_var = check_var_angle,
    compute_with = function(){}, 
    descr_requirements = req_numeric_var, 
    descr_variable = "Angles in degree (between 0° and 359°) between position of previous frame and from current one.", 
    name_in_app = "Angle from last point", 
    name_in_cypro = "aflp",
    name_in_example = NA_character_
  )

var_dflp <- 
  methods::new(
    Class = "ComputableVariable", 
    check_candidate = is_numeric_candidate,
    check_var = check_var_numeric,
    compute_with = compute_var_dflp, 
    descr_requirements = req_numeric_var, 
    descr_variable = "The distance a cell has traveled during the timespan between the last frame and the current one.", 
    name_in_app = "Distance from last point", 
    name_in_cypro = "dflp",
    name_in_example = NA_character_
  )

var_dfo <- 
  methods::new(
    Class = "ComputableVariable", 
    check_candidate = is_numeric_candidate,
    check_var = check_var_numeric,
    compute_with = compute_var_dfo, 
    descr_requirements = req_numeric_var, 
    descr_variable = "The distance a cell has traveled during the timespan betweent the first frame and the current one.", 
    name_in_app = "Distance from origin", 
    name_in_cypro = "dfo",
    name_in_example = NA_character_
  )

var_speed <- 
  methods::new(
    Class = "ComputableVariable", 
    check_candidate = is_numeric_candidate,
    check_var = check_var_numeric,
    compute_with = compute_var_speed, 
    descr_requirements = req_numeric_var, 
    descr_variable = glue::glue(
      "The distance from the last point divided by the time elapsed ",
      "between the acquisition of two subsequent frames."
    ) %>% base::as.character(), 
    name_in_app = "Speed", 
    name_in_cypro = "speed",
    name_in_example = NA_character_
  )

# -----

# required variables ------------------------------------------------------

# identifier variables
var_cell_id <- 
  methods::new(
    Class = "RequiredVariable", 
    check_candidate = is_grouping_candidate, 
    check_var = check_var_cell_id,
    descr_requirements = "Numeric or character as long as every cell has a unique value.",
    descr_variable = "Variable that identifies cells throughout all frames by number or by a character ID.", 
    name_in_app = "Cell ID", 
    name_in_cypro = "cell_id",
    name_in_example = NA_character_
  )

var_frame_num <- 
  methods::new(
    Class = "RequiredVariable", 
    check_candidate = is_grouping_candidate, 
    check_var = check_var_frame_num, # additionally checks if content of cell id and frame number uniquely identify each observation
    descr_requirements = "Numeric or convertable to numeric.", 
    descr_variable = "Variable that identifies each frame in an image stack by number.", 
    name_in_app = "Frame number", 
    name_in_cypro = "frame_num",
    name_in_example = NA_character_
  )

var_well_plate <- 
  methods::new(
    Class = "OptionalVariable",
    check_candidate = is_grouping_candidate, 
    check_var = check_var_well_plate, 
    descr_requirements = req_grouping_var, 
    descr_variable = "Variable that contains the well plate belonging of the cell.", 
    name_in_app = "Well plate", 
    name_in_cypro = "well_plate",
    name_in_example = NA_character_
    )

var_well <- 
  methods::new(
    Class = "OptionalVariable", 
    check_candidate = is_grouping_candidate, 
    check_var = check_var_well, 
    descr_requirements = req_grouping_var, 
    descr_variable = variable_content_descr$well,
    name_in_app = "Well", 
    name_in_cypro = "well", 
    name_in_example = NA_character_
  )

var_well_roi <- 
  methods::new(
    Class = "OptionalVariable", 
    check_candidate = is_grouping_candidate, 
    check_var = check_var_well_roi,
    descr_requirements = req_grouping_var, 
    descr_variable = variable_content_descr$well_roi,
    name_in_app = "Well-ROI",
    name_in_cypro = "well_roi",
    name_in_example = NA_character_
  )

var_roi <- 
  methods::new(
    Class = "OptionalVariable", 
    check_candidate = is_grouping_candidate, 
    check_var = check_var_roi, 
    descr_requirements = req_grouping_var, 
    descr_variable = variable_content_descr$roi,
    name_in_app = "Region of interest (ROI)", 
    name_in_cypro = "roi",
    name_in_example = NA_character_
  )

# data variables

var_x_coords <- 
  methods::new(
    Class = "RequiredVariable",
    check_candidate = is_numeric_candidate,
    check_var = check_var_numeric, 
    descr_requirements = req_numeric_var,
    descr_variable = "Refers to the x-coordinates of a cell.",
    name_in_app = "x-coordinates", 
    name_in_cypro = "x_coords",
    name_in_example = NA_character_
    )

var_y_coords <- 
  methods::new(
    Class = "RequiredVariable",
    check_candidate = is_numeric_candidate,
    check_var = check_var_numeric,
    descr_requirements = req_numeric_var,
    descr_variable = "Refers to the y-coordinates of a cell.",
    name_in_app = "y-coordinates", 
    name_in_cypro = "y_coords",
    name_in_example = NA_character_
  )

# -----


# summarizable variables --------------------------------------------------

var_mgr_eff <- 
  methods::new(
    Class = "SummarizableVariable",
    name_in_app = "Migration efficiency",
    name_in_cypro = "mgr_eff", 
    summarize_with = compute_var_mgr_eff
  )

var_total_dist <- 
  methods::new(
    Class = "SummarizableVariable", 
    name_in_app = "Total distance",
    name_in_cypro = "total_dist", 
    summarize_with = compute_var_total_dist 
  )

# -----





# r-objects ---------------------------------------------------------------

cypro_variables <- 
  list(
    optional = list(
      well_plate = var_well_plate, 
      well = var_well, 
      well_roi = var_well_roi, 
      roi = var_roi
    ),
    required = list(
      cell_id = var_cell_id, 
      frame_num = var_frame_num, 
      x_coords = var_x_coords, 
      y_coords = var_y_coords
    ),
    computable = list(
      aflp = var_aflp, 
      afo = var_afo, 
      dflp = var_dflp,
      dfo = var_dfo, 
      speed = var_speed
    ),
    summarizable = list(
      mgr_eff = var_mgr_eff, 
      total_dist = var_total_dist
    )
  )

cypro_variable_names <- 
  purrr::map(.x = cypro_variables, .f = ~ purrr::map(.x = ., .f = ~ .x@name_in_app)) %>% 
  purrr::flatten() 



