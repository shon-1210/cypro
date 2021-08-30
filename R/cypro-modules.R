
#' @include compute-family.R
#' 
NULL

# module blueprint --------------------------------------------------------

# each known variable in cypro has it's own list of information & tests
# a variable can appear in different modules


# module variable requirements --------------------------------------------

req_numeric_var <- "Must be numeric or convertable to numeric. (Must not contain character values.)"

req_grouping_var <- "Must be of type character or be convertable to type character. (Must not contain decimal numbers.)"

# module variable tests ---------------------------------------------------

# candidate check (for picker outputs) return TRUE or FALSE
is_grouping_candidate <- function(var){
  
  var <- var[!base::is.na(var)]
  
  if(base::is.character(var) | base::is.factor(var)){
    
    res <- TRUE
    
    # if not character of factor -> numeric
  } else if(!base::any(stringr::str_detect(var, pattern = "\\."))){ # . in var -> double
    
    res <- TRUE
    
  } else {
    
    res <- FALSE
    
  }
  
  return(res)
  
}

is_numeric_candidate <- function(var){
  
  if(base::is.numeric(var)){
    
    res <- TRUE
    
  } else if(base::is.character(var)){
    
    n_na <- base::is.na(var) %>% base::sum()
    
    var_numeric <- base::suppressWarnings({ base::as.numeric(var) })
    
    n_na_new <- base::is.na(var_numeric) %>% base::sum()
    
    if(n_na_new > n_na){
      
      res <- FALSE
      
    } else {
      
      res <- TRUE
      
    }
    
  } else {
    
    res <- FALSE
    
  }
  
  return(res)
  
}

# content check (attempts to convert if possible), returns:
# variable if valid 
# variable + warning in case of NA introduction
# error in case of only NAs after conversion or if var is NULL
check_and_convert_grouping_var <- function(var, ref, in_shiny = FALSE){
  
  n_nas <- base::is.na(var) %>% base::sum()
  
  l_var <- base::length(var)
  
  if(base::is.null(var)){
    
    msg <- glue::glue( "Variable '{ref}' is missing." )
    
    confuns::give_feedback(
      msg = msg, 
      fdb.fn = "stop", 
      in.shiny = in_shiny, 
      with.time = FALSE
    )
    
  } else if(!base::is.character(var)){
    
    var <- base::as.character(var)
    
    n_nas_new <- base::is.na(var) %>% base::sum()
    
    if(n_nas_new == l_var){
      
      msg <- 
        glue::glue(
          "Attempt to convert variable '{ref}' to type 'character' resulted in only NAs. ",
          "Variable is invalid."
          )
      
      confuns::give_feedback(
        msg = msg, 
        fdb.fn = "stop", 
        in.shiny = in_shiny, 
        with.time = FALSE
      )
      
    } else if(n_nas < n_nas_new){
      
      new_nas <- n_nas_new - n_nas
      
      msg <- glue::glue("Converting variable '{ref}' to character resulted in {new_nas} NAs.")
      
      confuns::give_feedback(
        msg = msg, 
        fdb.fn = "warning", 
        in.shiny = in_shiny, 
        with.time = FALSE
      )
      
    }
    
  }
  
  return(var)
  
}

check_and_convert_numeric_var <- function(var, ref, in_shiny = FALSE){
  
  n_nas <- base::is.na(var) %>% base::sum()
  
  l_var <- base::length(var)
  
  if(base::is.null(var)){
    
    msg <- glue::glue( "Variable '{ref}' is missing." )
    
    confuns::give_feedback(
      msg = msg, 
      fdb.fn = "stop", 
      in.shiny = in_shiny, 
      with.time = FALSE
    )
    
  } else if(!base::is.numeric(var)){
    
    if(base::is.factor(var)){
      
      # convert to character first in case of unordered levels
      var <- base::as.character(var)
      
    }
    
    var <- base::as.numeric(var)
    
    n_nas_new <- base::is.na(var) %>% base::sum()
    
    if(n_nas_new == l_var){
      
       msg <-
         glue::glue(
           "Attempt to convert variable '{ref}' to type 'numeric' resulted in only NAs. ",
           "Variable is invalid."
           )
       
       confuns::give_feedback(
         msg = msg,
         fdb.fn = "stop",
         in.shiny = in_shiny,
         with.time = FALSE
         )
       
      
    } else if(n_nas < n_nas_new){
      
      new_nas <- n_nas_new - n_nas
      
      msg <- 
        glue::glue("Converting variable '{ref}' to numeric resulted in {new_nas} NAs")
      
      confuns::give_feedback(
        msg = msg,
        fdb.fn = "warning",
        in.shiny = in_shiny,
        with.time = FALSE
        )
      
    }
    
  }
  
  return(var)
  
}


# variable type descriptions ----------------------------------------------

general_grouping_variable_info <- 
  list(
    descr = "Variables that contain categorical data that can be used to group observations.",
    requirements = req_grouping_var, 
    name_in_app = "Grouping Data",
    name_in_cypro = "grouping_variables",
    name_in_example = base::character(0), # can contain several names,
    check_candidate = is_grouping_candidate,
    check_content = check_and_convert_grouping_var
  )

general_numeric_variable_info <- 
  list(
    descr = "Variables that contain numeric data that can be used for analysis.",
    requirements = req_numeric_var, 
    name_in_app = "Numeric Data",
    name_in_cypro = "numeric_variables",
    name_in_example = base::character(0), # can contain several names,
    check_candidate = is_numeric_candidate,
    check_content = check_and_convert_numeric_var
  )



# identifier variables ----------------------------------------------------

var_cell_id <- 
  list(
    descr = "Variable that identifies cells throughout all images by number or by a character ID.", 
    requirements = "Numeric or character as long as every cell has a unique value.", 
    name_in_app = "Cell ID",
    name_in_cypro = "cell_id", 
    name_in_example = base:::character(1), 
    relevance = "needed",
    check_candidate = is_grouping_candidate,
    check_content = function(var, ref, in_shiny, ...){
      
      n_nas <- base::is.na(var) %>% base::sum()
      
      l_var <- base::length(var)
      
      n_ids <- dplyr::n_distinct(var)
      
      if(l_var > n_ids && n_nas == 0){
        
        msg <- 
          glue::glue(
            "There are more rows in the data.frame than there are unique ids in variable '{ref}'. ",
            "Invalid variable assignment for Cell ID."
          )
        
        confuns::give_feedback(
          msg = msg,
          fdb.fn = "stop",
          in.shiny = in_shiny
          )
        
      } else {
        
        var <-
          check_and_convert_grouping_var(
            var = var,
            ref = ref,
            in_shiny = in_shiny
            )
        
        res <- list(var) %>% purrr::set_names(nm = ref)
        
        return(res)
        
      }
      
    }
  )


var_frame_num <- 
  list(
    descr = "Variable that identifies each frame in an image stack by number.", 
    requirements = "Numeric or convertable to numeric.", 
    name_in_app = "Frame number",
    name_in_cypro = "frame_num", 
    name_in_example = base:::character(1),
    relevance = "needed",
    check_candidate = is_grouping_candidate,
    check_content = function(var, ref, ...){
      
      # check in check_var_frame_cell_id() function together with cell_id
      return(var)
      
    }
  )


check_and_convert_vars_frame_num_and_cell_id <- function(var_frame_num,
                                                         ref_frame_num,
                                                         var_cell_id,
                                                         ref_cell_id,
                                                         in_shiny){
  
  var_frame_num <- 
    check_and_convert_numeric_var(
      var = var_frame_num,
      ref = ref_frame_num,
      in_shiny = in_shiny
      )
  
  var_cell_id <- 
    check_and_convert_grouping_var(
      var = var_cell_id,
      ref = ref_cell_id,
      in_shiny = in_shiny
      )
  
  identifier_df <-
    base::data.frame(cell_id = var_cell_id, frame = var_frame_num) %>% 
    magrittr::set_colnames(value = c(ref_cell_id, ref_frame_num))
  
  n_obs <- base::nrow(identifier_df)
  
  n_unique_obs <- 
    dplyr::distinct(identifier_df) %>% 
    base::nrow()
  
  if(n_obs > n_unique_obs){
    
    msg  <- 
      glue::glue(
        "Frame-variable {ref_frame_num} and Cell ID-variable {ref_cell_id} do not", 
        "uniquely identify each observation. Invalid variable assignment."
        )
    
    confuns::give_feedback(
      msg = msg,
      fdb.fn = "stop",
      in.shiny = in_shiny,
      with.time = FALSE
      )
    
  } else {
    
    var_list <- base::as.list(identifier_df)
    
    return(var_list)
    
  }
  
}

# module variables  -------------------------------------------------------

# needed (variables that are the core of the modules they belong to and that can not be computed)
var_parent_id <- 
  list(
    descr = "The ID of a cells parent cell in the previous frame.", 
    requirements = req_grouping_var, 
    name_in_app = "Parent ID",
    name_in_cypro = "parent_id", 
    name_in_example = base::character(1), 
    relevance = "needed",
    check_candidate = is_grouping_candidate,
    check_content = check_and_convert_grouping_var
  )

var_parent_id_ref <- 
  list(
    descr = "The variable that 'Parent ID' refers to. This does not necessarily have to be 'Cell ID'.", 
    requirements = req_grouping_var, 
    name_in_app = "Parent ID reference",
    name_in_cypro = "parent_id_ref", 
    name_in_example = base::character(1), 
    relevance = "needed",
    check_candidate = is_grouping_candidate,
    check_content = check_and_convert_grouping_var
  )

var_x_coords <- 
  list(
    descr = "Refers to the x-coordinates of a cell.",
    requirements = req_numeric_var,
    name_in_app = "x-coordinates",
    name_in_cypro = "x_coords", 
    name_in_example = base:::character(1), 
    relevance = "needed",
    check_candidate = is_numeric_candidate,
    check_content = check_and_convert_numeric_var
  )


var_y_coords <- 
  list(
    descr = "Refers to the y-coordinates of a cell.",
    name_in_app = "y-coordinates",
    requirements = req_numeric_var,
    name_in_cypro = "y_coords", 
    name_in_example = base:::character(1), 
    relevance = "needed",
    check_candidate = is_numeric_candidate, 
    check_content = check_and_convert_numeric_var
  )


# computable (variable of time lapse- and one time imaging modules that are computed via dplyr::mutate())

var_afo <- 
  list(
    descr = "Angles in degree (between 0° and 359°) between position of first frame and from current one.", 
    requirements = req_numeric_var, 
    name_in_app = "Angle from origin",
    name_in_cypro = "afo", 
    name_in_example = base::character(1), 
    relevance = "computable",
    check_candidate = is_numeric_candidate,
    check_content = check_and_convert_numeric_var, 
    compute_with = compute_var_afo,
    compute_with_args = NULL
  )

var_aflp <- 
  list(
    descr = "Angles in degree (between 0° and 359°) between position of previous frame and from current one.", 
    requirements = req_numeric_var, 
    name_in_app = "Angle from last point",
    name_in_cypro = "aflp", 
    name_in_example = base::character(1), 
    relevance = "computable",
    check_candidate = is_numeric_candidate,
    check_content = check_and_convert_numeric_var, 
    compute_with = compute_var_aflp,
    compute_with_args = NULL
  )

var_dflp <- 
  list(
    descr = "The distance a cell has travelled from last frame to current one.", 
    requirements = req_numeric_var, 
    name_in_app = "Distance from last point",
    name_in_cypro = "dflp", 
    name_in_example = base::character(1), 
    relevance = "computable",
    check_candidate = is_numeric_candidate,
    check_content = check_and_convert_numeric_var, 
    compute_with = compute_var_dflp,
    compute_with_args = NULL
  )

var_dfo <- 
  list(
    descr = "The distance a cell has travelled from first frame to current one. In case of multi-phase experiment design 
    this variable is always computed from x- and y-coordinates.", 
    requirements = req_numeric_var, 
    name_in_app = "Distance from origin",
    name_in_cypro = "dfo", 
    name_in_example = base::character(1), 
    relevance = "computable",
    check_candidate = is_numeric_candidate,
    check_content = check_and_convert_numeric_var,
    compute_with = compute_var_dfo,
    compute_with_args = c("phase")
  )

var_speed <- 
  list(
    descr = glue::glue(
      "The distance from the last point divided by the time elapsed ",
      "between the acquisition of two subsequent frames."
      ), 
    requirements = req_numeric_var,
    name_in_app = "Instantaneous speed",
    name_in_cypro = "speed", 
    name_in_example = base::character(1), 
    relevance = "computable",
    check_candidate = is_numeric_candidate,
    check_content = check_and_convert_numeric_var,
    compute_with = compute_var_speed, 
    compute_with_args = NULL
  )

# 'summarizable' (variables of time lapse modules that are computed by dplyr::summarize())
#  these variables are stored in the stat data.frames

var_mgr_eff <- 
  list(
    descr = c(
      "A score for how efficient a cell has migrated.",
      "Sets the total distance in relation to the distance between origin and end point."
      ),
    name_in_app = "Migration efficiency",
    name_in_cypro = "mgr_eff",
    relevance = "computable_summary",
    summarize_with = compute_var_mgr_eff,
    summarize_with_args = list(),
    summarize_with_stat_vars = "total_dist", # names of already smrd variables that need to be specified in the summary function
    summarize_with_track_vars = c("x_coords", "y_coords") # names of tracks variables that need to be specified in the summary function
    
  )

var_total_dist <-
  list(
    descr = "The total distance a cell has travelled.",
    name_in_app = "Total distance travelled",
    name_in_cypro = "total_dist",
    relevance = "computable_summary",
    summarize_with = compute_var_total_dist
  )



# analysis modules --------------------------------------------------------

empty_analysis_module <- 
  list(
    exp_type = character(1), 
    helper_text = character(1),
    name = character(1),
    pretty_name = character(1),
    variables = list(), 
    variables_to_summarize = list(),
    computation_order = character(0),
    summary_order = character(0),
    replace_na_first_frame = numeric(1)
  )

# one_time_imaging

module_localisation <- 
  list(
    exp_type = "one_time_imaging",
    helper_text = "Uses x- and y-coordinates for cell localisation.",
    name = "localisation",
    pretty_name = "Localisation",
    variables = list(
      x_coords = var_x_coords, 
      y_coords = var_y_coords
    )
  )


# time_lapse 

module_migration <- 
  list(
    exp_type = "time_lapse",
    helper_text = "Quantifies locomotion of cells in time lapse experiments.",
    name = "migration",
    pretty_name = "Migration",
    variables = list(
      x_coords = var_x_coords,
      y_coords = var_y_coords,
      dfo = var_dfo,
      dflp = var_dflp,
      speed = var_speed,
      afo = var_afo, 
      aflp = var_aflp
    ), 
    replace_na_first_frame = 0,
    variables_to_summarize = list(
      total_dist = var_total_dist, 
      mgr_eff = var_mgr_eff
    ),
    computation_order = c("dfo", "dflp", "speed", "afo", "aflp"), 
    summary_order = c("total_dist", "mgr_eff")
  )

module_mitosis <- 
  list(
    exp_type = "time_lapse",
    helper_text = "Analyzes cell proliferation and lineage development in time lapse experiments.",
    name = "mitosis",
    pretty_name = "Mitosis",
    variables = list(
      parent_id = var_parent_id,
      parent_id_ref = var_parent_id_ref
    ),
    variables_to_summarize = list(),
    computation_order = character(0),
    summary_order = character(0)
  )



# data loading reading modules --------------------------------------------

# additional variables
module_additional_data_variables <- 
  list(
    exp_type = "both",
    helper_text = "none needed",
    pretty_name = "none needed",
    name = "additional_data_variables",
    variables = list(
      grouping_variable_info = general_grouping_variable_info,
      numeric_variable_info = general_numeric_variable_info
    )
  )


# identification module
module_identification_one_time_imaging <- 
  list(
    exp_type = "one_time_imaging", # does not appear in shiny app 
    helper_text = "none needed", 
    pretty_name = "none_needed", 
    name = "identification_one_time_imaging", 
    variables = list(
      cell_id = var_cell_id
    )
  )

module_identification_time_lapse <- 
  list(
    exp_type = "time_lapse", 
    helper_text = "none needed", # does not appear in shiny app 
    pretty_name = "none_needed",
    name = "identification_time_lapse",
    variables = list(
      cell_id = var_cell_id, 
      frame_num = var_frame_num
    )
  )

# module collection -------------------------------------------------------

cypro_modules <- 
  list(
    localisation = module_localisation,
    migration = module_migration
  )

all_module_names <- base::names(cypro_modules)

time_lapse_module_names <- 
  purrr::keep(cypro_modules, .p = ~ .x$exp_type %in% c("time_lapse", "both")) %>% 
  base::names()

one_time_imaging_module_names <- 
  purrr::keep(cypro_modules, .p = ~ .x$exp_type %in% c("one_time_imaging", "both")) %>% 
  base::names()

