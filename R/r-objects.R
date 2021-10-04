




# Well plate information --------------------------------------------------

valid_well_plates <- c("2x3 (6)", "3x4 (12)", "4x6 (24)", "6x8 (48)", "8x12 (96)")

well_plate_info <- 
  data.frame(
    type = valid_well_plates, 
    rows = c(2,3,4,6,8), 
    cols = c(3,4,6,8,12)
  )

# -----

# Regular expressions -----------------------------------------------------

well_regex <- "[A-Z]{1}\\d{1,2}"
well_roi_regex <- "[A-Z]{1}\\d{1,2}_\\d{1}"
file_regex <- "[A-Z]{1}\\d{1,2}_\\d{1}\\.(csv|xls|xlsx)$"

# -----

# Column names ------------------------------------------------------------

non_data_track_variables <- c("frame_added", "frame_itvl", "frame_num", "frame_time",
                              "x_coords", "y_coords")

original_ct_variables <- c("y-coordinate [pixel]", "x-coordinate [pixel]", "Frame number", "Cell ID",
                           "Distance from origin", "Distance from last point", "Instantaneous speed",
                           "Angle from origin", "Angle from last point")

short_ct_variables <- c("well_roi", "condition", "cell_line", "cell_id", "x_coords",
                        "y_coords", "frame", "dfo", "dflp", "speed", "afo", "aflp")

# -----

# Miscellaneuos -----------------------------------------------------------

analysis_methods <- list(
  dim_red = c("pca", "tsne", "umap"), 
  clustering = c("hclust", "kmeans", "pam")
)

app_title <- "Cypro"

ambiguity_colors <- c("Clear" = "#1CE35B", "Ambiguous" = "#E02424", "Dismissed" = "lightgrey")

cdata_slots <- c("cluster", "meta", "stats", "tracks", "well_plate")

colors_grey <- c("unknown" = "lightgrey",
                 "unknown & unknown" = "lightgrey",
                 "Dismissed" = "lightgrey")

colors_unnamed <- c("#D4E8CF", "#EBAAAA", "#EBBCD6", "#A0E8CB", "#DEDEAB", "#B6A8E0", "#9FBA8E",
                    "#EBD1B0", "#BBC1F0", "#F2C2C2", "#AD9A9A", "#F2A9F5", "#E5CCF0", "#FFFC96",
                    "#CCFFFC", "#A0E8FA", "#C7B6FC", "#C5FCCC", "#EDFCB0", "#C8FAF0")

colors_information_status = c("Complete" = "forestgreen", 
                              "Incomplete" = "yellow", 
                              "Missing" = "red", 
                              "Discarded" = "lightgrey")

current_version <- list(major = 0, minor = 3, patch = 0)

data_status_levels <- c("Complete", "Incomplete", "Missing")

debug_ct <- FALSE

descr_variables <- c("cell_id", "cell_line", "condition")

default_list <-
  list(
    clrp = "milo",
    clrsp = "viridis",
    color_aes = "color",
    method_aggl = "ward.D",
    method_corr = "pearson",
    method_dist = "euclidean",
    method_kmeans = "Hartigan-Wong",
    method_outlier = "iqr",
    method_pam = "euclidean",
    phase = "first", 
    pt_alpha = 0.9, 
    pt_clr = "black",
    pt_clrp = "milo", 
    pt_clrsp = "viridis",
    pt_fill = "black",
    pt_shape = 19,
    pt_size = 3, 
    smooth_alpha = 0.9,
    smooth_clr = "blue",
    smooth_method = "lm",
    smooth_se = FALSE,
    smooth_size = 1,
    verbose = TRUE, 
    well_plate = character(1), 
    with_cluster = TRUE, 
    with_meta = TRUE, 
    with_well_plate = TRUE
  )

default_character_values <- c("method_aggl", "method_corr", "method_dist", "method_kmeans", 
                       "method_pam", "phase", "pt_clr", "pt_clrp", "pt_clrsp", "pt_fill", 
                       "well_plate")

default_logical_values <- c("make_pretty", "with_cluster", "with_meta", "with_well_plate")

default_numeric_values <- c("pt_alpha", "pt_size")






filetypes <- c("csv$", "xls$", "xlsx$")

helper_content <- list(
  
  # designExperiment()
  
  total_number_of_images = c(
    "'Total Number of Images' refers to the number of times the imaging device
    made a picture (Cell Tracker refers to that as 'Frame Number'",
    "", 
    "For instance, if you denote the 25 as the total number of images cypro 
    assumes that each file contains 25 rows of data for each cell id. Additional
    rows will be discarded. Missing rows will be imputed."),
  
  interval = c("'Interval' refers to the time that has passed between each image."),
  
  interval_unit = c("'Interval Unit' specifies the unit of the numeric input of 'Interval'."),
  
  number_of_phases = c(
    "If the experiment is split into several phases you can specify the number of phases here.",
    "",
    "For instance:",
    "", 
    "If treatment started right from the beginning there is only one phase and thus no need to further specify anything.",
    "",
    "If cells have been imaged for twelve hours every hour then the oxygen 
    supply was decreased for the next twelve hours. There are two phases. In case
    of more than one phase you can denote the starting point of subsequent phases 
    below."
  ),
  
  rois_per_well = 
    c("E.g. if your data contains the files A1_1.csv, A1_2.csv and A1_3.csv
       there are 3 images/image stacks per well.",
      "",
      "Denoting the correct number is important in so far as it limits the number 
      of files that is looked for later on."
      ),

  
  # loadDataFile()
  well_plate_var = c(
    "Choose the variable that contains information about the well plate name.", 
    "If the experiment design contains only one well plate. You can specify 'none'."
  ),
  
  well_var = c(
    "Choose the variable that contains information about the well."
  ), 
  
  roi_var = c(
    "Choose the variable that contains information about the region of interest (roi)."
  ),
  
  # loadData()
  assign_folder = c(
  "In the left lower corner you see a select option that contains the names of the
  well plates you have set up in the module opened via 'designExperiment()'. 
  In order for cypro to read in the data every well plate needs to be assigned to
  a directory in which cypro finds the files to read in.",
  "",
  "Clicking on 'Assign Folder: Browse' opens a window that gives you access to the
  folders of your device. Select the one that contains the files of the respective
  well plate or that again contains (sub-)folders in which the files are stored.
  (If you want cypro to look in subfolders, too, make sure that 'Include Subfolders' is enabled.)",
  "",
  "After you have assigned the folder by closing the window you will see a well
  plate plot appear right above the button 'Assign Folder: Browse'. The wells will
  be colored according to the number of files that have been found in the folder you
  specified.",
  "",
  "Green/Complete means that all files have been found.",
  "",
  "Yellow/Incomplete means that only some files of the well have been found.
  (e.g. you set 'Images per Well' to 3 which made cypro expect files
  'A1_1.csv', 'A1_2.csv' and 'A1_3.csv'. However only 'A1_1.csv' and 'A1_3.csv'
  have been found).",
  "",
  "Missing/Red means that no files of that well have been found at all.",
  "",
  "Blue/Ambiguous might appear if you stored the files in subfolders of the
  assigned folder and some folders contain files with equal names due to typos
  while naming the files. (e.g. ~/Ctrl/A1_1.csv, ~/Treatment/A1_1.csv)",
  "",
  "If you realized that you assigned the wrong folde or that some files were
  named incorrectly you can fix what needs to be fixed and assign the directory
  again by clicking on 'Assign Folder: Browse'."), 
  
  if_ambiguous = c(
  "In case of ambiguous file naming you can denote which filetype should be used.",
  "",
  "If the chosen filetype is not among the found files the file is considered
  to be missing."
  ),
  
  well_plate_status = c(
  "This table provides summary information about the file availability for all well
  plates. It updates every time you assign a folder to a well plate. 'Number of
  Files' sums up the number of files found and 'Expected Number of Files' sets that
  in relation to the number of files expected according to number of covered areas
  per well and the number of wells for which you specified cell line and conditions.",
  "", 
  "Missing or incomplete wells do not prevent you from reading in all other files found -
  no need to design the experiment again if you realize that some files are missing.
  Just click on 'Load Data' and the missing files will be ignored. In case of ambiguous
  wells you need to rename or delete the files that are doubled before loading the data."),
  
  load_files_and_proceed = c(
  "After clicking on 'Load Data' you should see a progress bar for every well plate
  giving information about the reading progress. Once all files are read in you obtain
  information about any errors that occured while reading the files.",
  "",
  "In case of no errors you can simply click on 'Save & Proceed' and afterwards on
  'Return Cypro Object'. If the box hints at errors occured you can try and fix the
  cause for these errors and then click on 'Load Data' again which will repeat the
  reading process. On the other hand you can ignore the errors and click on
  'Save & Proceed' anyway which makes cypro ignore the files that were not suffessfully
  read in and continue with the rest."
  )
  
  
  
)

image_processing_softwares <- 
  c("Cell Profiler" = "cell_profiler", 
    "Cell Tracker" = "cell_tracker", 
    "ImageJ" = "imagej"
    )

imp_filter_criteria <- c("total_meas", "skipped_meas", "first_meas", "last_meas")

interval_options <- c("weeks", "days", "hours", "minutes", "seconds", "miliseconds")

invalid_groups <- c("cell_id", "well_plate_name", "well_plate_index", "well", "well_roi")

legend_titles <- c("ambiguity_status" = "Ambiguity Status", 
                   "cl_condition" = "Cell Line & Condition", 
                   "condition" = "Condition", 
                   "cell_line" = "Cell Line")

meta_variables <- c("cell_id", "well_plate_name", "well_plate_index", "well", "well_roi")

new_slots <- c("analysis", "compatibility", "default", "information", "name", "well_plates", "version")

numeric_stat_vars <- c("total_dist", "max_dist_fo", "avg_dist_fo", "max_dist_flp",
                       "avg_dist_flp", "max_speed", "avg_speed", "mgr_eff")

not_splitted <- c("No treatment", "From beginning")

object_class <- "cypro"
base::attr(object_class, which = "package") <- "cypro"



protected_vars <- c("cell_id", "cell_line", "condition",
                    "well_plate_name", "well_plate_index", "well",  "well_roi", 
                    "x_coords", "y_coords", 
                    "frame_itvl", "frame_num", "frame_time", 
                    "imputed")

protected_vars_modules <- 
  purrr::map(.x = cypro_modules, .f = function(module){
    
    var_names <- 
      c(base::names(module$variables),
        base::names(module$variables_to_summarize)
      )
    
    return(var_names)
    
  }) %>% 
  purrr::flatten_chr() %>% 
  base::unique()

protected_vars_all <- 
  base::unique(c(protected_vars, protected_vars_modules))


set_up_funs <- list(experiment_design = "designExperiment()", 
                    load_data = "loadData()", 
                    quality_check = "checkDataQuality()", 
                    process_data = "processData()")

shiny_bar_positions <- c("Stacked" = "stack", "Dodged" = "dodge", "Filled" = "fill")

shiny_discrete_vars <- c("Cell Line and Condition" = "cl_condition", 
                         "Cell Line" = "cell_line", 
                         "Condition" = "condition")

stat_funs <- list("max" = base::max,
                  "mean" = base::mean,
                  "median" = stats::median,
                  "min" = base::min,
                  "sd" = stats::sd,
                  "var" = stats::var)

status_colors <- c("Missing" = "#B31010",
                   "Incomplete" = "#FFD700",
                   "Complete" = "darkgreen", 
                   "Dismissed" = "lightgrey",
                   "Ambiguous" = "#4C12E0")

storage_slots <- c("directory", "valid_directories", "missing_files")

testable_plottypes <- c("boxplot", "violinplot")

variable_relevance_descr <- 
  list(
    "needed" = glue::glue(
      "This variable has to be provided by the input data ",
      "if you want to use the modules it is part of."
      ),
    "computable" = glue::glue(
      "This variable does not have to be part of the input data. ",
      "If it is not, cypro computes it based an the variables ",
      "needed by this module."
      )
  )

well_plate_vars <- c("well_plate_name", "well_plate_index", "well",  "well_roi")



# -----


# Pretty names ------------------------------------------------------------

pretty_exp_types <- 
  list("one_time_imaging" = "One Time Imaging", 
       "time_lapse" = "Time Lapse", 
       "time_lapse_smrd" = "Time Lapse (summarized)"
       )

pretty_grouping_variables_list <-
  list("Cell Line &\n Condition" = "cl_condition", 
       "Condition" = "condition", 
       "Cell Line" = "cell_line")

pretty_grouping_variables_vec <-
  purrr::imap_chr(.x = pretty_grouping_variables_list, 
                  .f = ~ .x)

pretty_linetypes <- c("Solid" = "solid", 
                      "Twodash" = "twodash", 
                      "Longdash" = "longdash", 
                      "Dotted" = "dotted", 
                      "Dotdash" = "dotdash")

pretty_plottypes <- c("Violinplot" = "violinplot", 
                      "Ridgeplot" = "ridgeplot", 
                      "Densityplot" = "density",
                      "Boxplot" = "boxplot"
                      )

pretty_phases <- c("Before treatment" = "before_tmt",
                   "After first treatment" = "first_tmt", 
                   "Entire timespan" = "all")

pretty_stattests <- c("T-test" = "t.test", 
                      "Wilcox" = "wilcox.test", 
                      "ANOVA" = "anova", 
                      "Kruskal" = "kruskal.test")

pretty_stattests_pairwise <- c("None" = "none", pretty_stattests[1:2])
pretty_stattests_groupwise <- c("None" = "none", pretty_stattests[3:4])

pretty_stat_variables_list <-
  list("Total Distance" = "total_dist" ,
       "Max. distance from origin" = "max_dist_fo", 
       "Avg. distance from origin" = "avg_dist_fo",
       "Max. distance from last point" = "max_dist_flp", 
       "Avg. distance from last point" = "avg_dist_flp", 
       "Max. speed" = "max_speed", 
       "Avg. speed" = "avg_speed", 
       "Migration efficiancy" = "mgr_eff")

pretty_stat_variables_vec <-
  purrr::imap_chr(.x = pretty_stat_variables_list, 
                  .f = ~ .x) 

pretty_wp_variables_list <- 
  list("WP Name" = "well_plate_name", 
       "WP Index" = "well_plate_index", 
       "Well" = "well", 
       "Well-Image" = "well_roi")

pretty_wp_variables_vec <- 
  purrr::imap_chr(.x = pretty_wp_variables_list, 
                  .f = ~ .x)


pretty_names_vec <- 
  c(pretty_stat_variables_vec,
    pretty_grouping_variables_vec, 
    pretty_wp_variables_vec)

pretty_names_list <- 
  c(pretty_stat_variables_list,
    pretty_grouping_variables_list, 
    pretty_wp_variables_list)

# -----



# Feedback lists ----------------------------------------------------------

ct_warnings <- list(
  
  # renaming
  "how_to_name_input" = "Input needs to be named like this: 'new_group_name' = 'old_group_name'",

  
  # statistical tests
  "stat_test_requirements" = "In order to perform statistical tests please choose 'boxplot' or 'violinplot' as input for argument 'plot_type' and specify only one variable as input for argument 'variables'."
  
  
)




# -----

