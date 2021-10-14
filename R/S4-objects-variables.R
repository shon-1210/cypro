#' @include compute-family.R logical-tests.R r-objects.R
#' 
NULL



# computable variables ----------------------------------------------------

var_afo <- 
  methods::new(
    Class = "ComputableVariable", 
    check_candidate = is_numeric_candidate,
    compute_with = function(){}, 
    descr_requirements = req_numeric_var, 
    descr_variable = "Angles in degree (between 0° and 359°) between position of first frame and from current one.", 
    name_in_app = "Angle from origin", 
    name_in_cypro = "afo"
  )

var_aflp <- 
  methods::new(
    Class = "ComputableVariable", 
    check_candidate = is_numeric_candidate,
    compute_with = function(){}, 
    descr_requirements = req_numeric_var, 
    descr_variable = "Angles in degree (between 0° and 359°) between position of previous frame and from current one.", 
    name_in_app = "Angle from last point", 
    name_in_cypro = "aflp"
  )

var_dflp <- 
  methods::new(
    Class = "ComputableVariable", 
    check_candidate = is_numeric_candidate,
    compute_with = compute_var_dflp, 
    descr_requirements = req_numeric_var, 
    descr_variable = "The distance a cell has traveled during the timespan betweent the last frame and the current one.", 
    name_in_app = "Distance from last point", 
    name_in_cypro = "dflp"
  )

var_dfo <- 
  methods::new(
    Class = "ComputableVariable", 
    check_candidate = is_numeric_candidate,
    compute_with = compute_var_dfo, 
    descr_requirements = req_numeric_var, 
    descr_variable = "The distance a cell has traveled during the timespan betweent the first frame and the current one.", 
    name_in_app = "Distance from origin", 
    name_in_cypro = "dfo"
  )

var_speed <- 
  methods::new(
    Class = "ComputableVariable", 
    check_candidate = is_numeric_candidate,
    compute_with = compute_var_speed, 
    descr_requirements = req_numeric_var, 
    descr_variable = glue::glue(
      "The distance from the last point divided by the time elapsed ",
      "between the acquisition of two subsequent frames."
    ) %>% base::as.character(), 
    name_in_app = "Distance from origin", 
    name_in_cypro = "dfo"
  )

# -----

# required variables ------------------------------------------------------

var_cell_id <- 
  methods::new(
    Class = "RequiredVariable", 
    check_candidate = is_grouping_candidate, 
    descr_requirements = "Numeric or character as long as every cell has a unique value.",
    descr_variable = "Variable that identifies cells throughout all frames by number or by a character ID.", 
    name_in_app = "Cell ID", 
    name_in_cypro = "cell_id"
  )

var_frame_num <- 
  methods::new(
    Class = "RequiredVariable", 
    check_candidate = is_grouping_candidate, 
    descr_requirements = "Numeric or convertable to numeric.", 
    descr_variable = "Variable that identifies each frame in an image stack by number.", 
    name_in_app = "frame_num", 
    name_in_cypro = "Frame number"
  )

var_x_coords <- 
  methods::new(
    Class = "RequiredVariable",
    check_candidate = is_numeric_candidate,
    descr_requirements = req_numeric_var,
    descr_variable = "Refers to the x-coordinates of a cell.",
    name_in_app = "x-coordinates", 
    name_in_cypro = "x_coords"
    )

var_y_coords <- 
  methods::new(
    Class = "RequiredVariable",
    check_candidate = is_numeric_candidate,
    descr_requirements = req_numeric_var,
    descr_variable = "Refers to the y-coordinates of a cell.",
    name_in_app = "y-coordinates", 
    name_in_cypro = "y_coords"
  )

# -----


# summarizable variables --------------------------------------------------

var_mgr_eff <- 
  methods::new(
    Class = "SummarizableVariable",
    name_in_cypro = "mgr_eff", 
    summarize_with = compute_var_mgr_eff
  )

var_total_dist <- 
  methods::new(
    Class = "SummarizableVariable", 
    name_in_cypro = "total_dist", 
    summarize_with = compute_var_total_dist 
  )

# -----


