

# old-cypro-object -------------------------------------------------------


### old

cypro <- setClass(Class = "cypro", 
                  slots = list(
                    analysis = "list",
                    compatibility = "list",
                    cdata = "list",
                    vdata = "list",
                    default = "list",
                    information = "list",
                    modules = "list",
                    name = "character",
                    qcheck = "list",
                    set_up = "list",
                    well_plates = "list", 
                    variable_sets = "list",
                    version = "list"
                  )
)

# -----



# analysis modules --------------------------------------------------------

#' @title The AnalysisModule Class
#' 
#' @description An analysis module in \code{cypro} represents a biological/bioinformatic aspect
#' around which can be programmed using a fixed set of data variables that are known to \code{cypro}.
#' See details for more information and examples. 
#' 
#' @slot active logical. Denotes if a module is used or not. This is decided via the variable assignment
#' in \code{assignVariables()} and/or \code{assignVariablesManually()}. An inactive module (@@active = FALSE)
#' is ignored by all functions regarding all modules. E.g. \code{getVariableAssignment()}.
#' @slot check_fns list. A list of functions that check the data variables of an input 
#' data.frame that have been assigned to variables known by \code{cypro} against their 
#' requirements. This differs from the variable specific functions of the slot @@check_var
#' of each data variable in so far as that the functions from slot @@check_fns can refer to several 
#' data variables at the same time. E.g. \emph{cell_id} and \emph{frame_num} combined must identify 
#' each row of the input data.frame as a cell at a given point of time. A '@@check_fns-function' does that. 
#' To make sure that the variable assigned to \emph{frame_num} is numeric is accomplished by 
#' the function stored in slot @@check_var of its corresponding \code{AssignableVariable} object. 
#' @slot data list. A named list that contains specific data, computations or analysis
#' that does not fit into the regular \code{Cdata}-class.
#' @slot descr character. Text that describes what the analysis module is about.
#' @slot descr_short character. Short sentence that summarizes what the module is about.
#' @slot name_in_app character. Pretty name to be used as reference in application.
#' @slot name_in_cypro character. Short name to be used as reference throughout
#' \code{cypro}. Should follow underscore-naming-convention (e.g. \emph{x_coords} instead of 
#' \emph{xCoords} or \emph{x-coords})
#' @slot variables_computable list. Named list of computable variables represented by the S4-Class
#' \code{ComputableVariable}.  Names of list correspond to the respective slot @@name_in_cypro.
#' Variables must be ordered according to the order in which they are supposed to be computed.
#' @slot variables_optional list. Named list of optional variables represented by the S4-class 
#' \code{OptionalVariable}. 
#' @slot variables_required list. Named list of required variables represented by the S4-Class
#' \code{RequiredVariable}.  Names of list correspond to the respective slot @@name_in_cypro.
#' 
#' @details Example: Cellular migration 
#' Cellular migration is a commonly explored aspect in time lapse image analysis.  
#' 
#' @seealso \code{AssignableVariable}, 

AnalysisModule <- setClass(Class = "AnalysisModule", 
                           slots = list(
                             active = "logical",
                             check_fns = "list",
                             data = "list",
                             descr = "character",
                             descr_short = "character",
                             name_in_app = "character",
                             name_in_cypro = "character",
                             variables_computable = "list",
                             variables_optional = "list",
                             variables_required = "list")
                           )


#' @title The AnalysisModuleTimeLapse Class
#' 
#' @description Subclass of S4-Class \code{AnalysisModule}. Specific class for modules
#' that can only be used in time lapse experiments.
#'
#' @slot variables_summary list. Named list of summary variables represented by the 
#' S4-class \code{SummaryVariable}. Names of list correspond to the respective
#' slot @@name_in_cypro. Variable must be ordered according to the order in which they
#' are supposed to be summarized. E.g. as the migration efficiency (\emph{mgr_eff}) requires
#' the variable total distance (\emph{total_dist}) the total distance must be listed 
#' before migration efficiency.
#' 
#' @export
#'
AnalysisModuleTimeLapse <- setClass(Class = "AnalysisModuleTimeLapse", 
                                    slots = list(
                                      variables_summary = "list"
                                    ), 
                                    contains = "AnalysisModule")

# -----




# Cdata + -----------------------------------------------------------------

#' @title The Cdata Class (Cell Data)
#' 
#' @description The parent S4-class of every \code{Cypro}-objects \code{@@cdata}
#' slot. Contains the experiment data in form of data.frames. All data.frames 
#' are functionally connected by their common data variable \emph{cell_id} with which 
#' they can be joined via \code{dplyr::left_join()}. 
#' @slot scaled matrix. Contains scaled data. In case of screening experiments it's
#' the data from slot @@features. In case of time lapse experiments it's the scaled data
#' from slot @@features_stats.
#' @slot well_plate data.frame. Contains data variables that give 
#' information about a cells well plate belonging: \emph{well_plate_name, 
#' well_plate_index, well} and \emph{well_roi}.
#' 
#' @seealso CdataScreening, CdataTimeLapse, CdataTimeLapseMP
#'
Cdata <- setClass(Class = "Cdata", 
                  slots = list(
                    well_plate = "data.frame"
                  ))

#' @title The Cdata Class (Cell Data - Screening Experiments)
#' 
#' @description S4 object that contains the slots needed to represent the 
#' data obtained by screening experiments (one time imaging).
#'
#' @slot cluster data.frame. Contains data variables that group cells
#' according to clustering results. These cluster variables can either 
#' be added from cypro intern clustering via \code{add*Algorithm*ClusterVariables()} 
#' or from cypro extern clustering via \code{addClusterVariables()}. 
#' @slot features data.frame. Contains the cellular features (quantified by e.g. CellProfiler or ImageJ) 
#' in form of numeric data variables. 
#' @slot meta data.frame. Contains data variables that group cells according 
#' to the experiment design: \emph{cell_line} and \emph{condition}. Additional 
#' grouping variables can be added via \code{addMetaVariables()}. 
#' @slot scaled data.frame. Contains scaled data. In case of screening experiments it's
#' the data from slot @@features.
#'
CdataScreening <- setClass(Class = "CdataScreening", 
                           slots = list(
                             cluster = "data.frame",
                             features = "data.frame",
                             meta = "data.frame",
                             scaled = "data.frame"
                           ), 
                           contains = "Cdata")

#' @title The Cdata Class (Cell Data - Time Lapse Experiments)
#' 
#' @description S4 object that contains the slots needed to represent the 
#' data obtained by screening experiments (one time imaging).
#' 
#' @slot cluster data.frame. Contains data variables that group cells
#' according to clustering results. These cluster variables can either 
#' be added from cypro intern clustering via \code{add*Algorithm*ClusterVariables()} 
#' or from cypro extern clustering via \code{addClusterVariables()}. 
#' @slot meta data.frame. Contains data variables that group cells according 
#' to the experiment design: \emph{cell_line} and \emph{condition}. Additional 
#' grouping variables can be added via \code{addMetaVariables()}. 
#' @slot features_stats data.frame. Contains data variables that represent 
#' summarized, cellular profiles of the time lapse data of slot @@features_tracks.
#' @slot features_tracks data.frame. Contains the cellular features (quantified by e.g. CellProfiler or ImageJ) 
#' in form of numeric data variables. In addition to the \emph{cell_id}-variable three 
#' variables (\emph{frame_num, frame_time} and \emph{frame_itvl} indicate the point of time during which
#' the observation was made. The variable \emph{frame_added} indicates whether the
#' observation was actually missing due to imaging artifacts and was added artificially by imputation. 
#' @slot scaled data.frame. Contains scaled data. In case of time lapse experiments it's
#' the data from slot @@features_stats.
#' 

CdataTimeLapse <- setClass(Class = "CdataTimeLapse", 
                           slots = list(
                             cluster = "data.frame", 
                             meta = "data.frame",
                             features_stats = "data.frame",
                             features_tracks = "data.frame",
                             scaled = "data.frame"
                             
                           ),
                           contains = "Cdata")


#' @title The Cdata Class (Cell Data - Time Lapse Experiments - Multiple Phases)
#' 
#' @description S4 object that contains the slots needed to represent the 
#' data obtained by screening experiments (multiple phase time lapse experiments).
#' All slots are lists named according to the ordinal number of the phase (\emph{first, 
#' second, third...}).
#' 
#' @slot cluster list. Contains data.frames as described in slot @@cluster of 
#' S4-class \code{CdataTimeLapse} for every phase of the experiment. 
#' @slot meta list. Contains data.frames as described in slot @@meta of S4-class 
#' \code{CdataTimeLapse} for every phase of the experiment. 
#' @slot features_stats list. Contains data.frames as described in slot @@features_stats
#' of S4-class \code{CdataTimeLapse} for every phase of the experiment.
#' @slot features_tracks list. Contains data.frames as described in slot @@features_tracks
#' of S4-class \code{CdataTimeLapse} for every phase of the experiment.
#' @slot scaled list. Contains scaled data for every phase. In case of time lapse experiments it's
#' the data from slot @@features_stats.
#' 
CdataTimeLapseMP <- setClass(Class = "CdataTimeLapseMP", 
                             slots = list(
                               cluster = "list",
                               meta = "list",
                               features_stats = "list", 
                               features_tracks = "list",
                               scaled = "list"
                             ),
                             contains = "Cdata")

# -----



# DataVariable + ------------------------------------------------------------

#' @title The DataVariable Class
#' 
#' @description The S4-class \code{DataVariable} is an object oriented approach to 
#' solve the problem of different image analysis software following different
#' naming conventions for data variables that are used and known across platforms.
#' See details for more. 
#'
#' @slot name_in_app character. Pretty name to be used as reference in application.
#' @slot name_in_cypro character. Short name to be used as reference throughout
#' \code{cypro}. Should follow underscore-naming-convention (e.g. \emph{x_coords} instead of 
#' \emph{xCoords} or \emph{x-coords})
#' 
#' @details The 'Variable-Ecosystem' of \code{cypro} attempts to solve the problem of different
#' image analysis software following different naming conventions. The reasoning behind this goes 
#' as follows: Columns in data tables that refer to certain data variables such as x- and y-coordinates,
#' frame number, cell perimeter, etc. are often named inconsistent across image analysis software which 
#' complicates writing module specific code.
#' E.g. cellular migration is commonly visualized by rose plots (see function \code{plotRosePlot()}).
#' This function refers to x- and y-coordinates. If the input data, however, can refer to coordinates in different ways like 
#' \emph{x}, \emph{x_coordinates}, \emph{x-coordinates}, the code becomes unreliable.  
#' Unspecific data variables visualized by boxplots or density plots can follow arbitrary 
#' naming conventions. However, variables that are used repeatedly in \code{cypro}-intern code must follow a
#' consistent system to assure reliable code. 
#' 
#' Therefore, to ensure flexibility regarding the data input, data variables of the input data tables
#' are assigned to the variables \code{cypro} knows by assigning their names. This is done interactively 
#' in \code{assignVariables()} where an example file of the input is loaded. Then the individually named
#' variables/columns of the input data are assigned to variables \code{cypro} knows. This assignment is 
#' considered at all data loading steps and variables/columns of data fiels are immediately renamed while beeing read into 
#' \code{cypro}. The assignment is stored and can be obtained via \code{getVariableAssignment()}.
#' 
#' @seealso \code{AssignableVariable}, \code{RequiredVariable}, \code{ComputableVariable},
#' \code{SummaryVariable}
#' 
#' @export
#'
DataVariable <- setClass(Class = "DataVariable", 
                         slots = list(
                           name_in_app = "character",
                           name_in_cypro = "character"
                         )
)

#' @title The AssignableVariable Class
#' 
#' @description An assignable variable is defined as a variable that can be part of
#' the input data. See details for more information.
#' 
#' @slot check_candidate function. A function that takes a vector as input and
#' returns a single TRUE if the vector meets the requirements described in 
#' slot @@descr_requirements or a single FALSE if not. This only refers to superficial things 
#' like class and types. 
#' 
#' The function must have only one argument, namely \code{var}.
#'
#' @slot check_var function. A more sophisticated function that takes several things 
#' into consideration to check if the content of the assigned variable fits the requirements.
#' It takes a data.frame, extracts the assigned variable, checks the variable's content, 
#' adjusts it if necessary/possible and returns the variable or an informative message about
#' the content's flaws.
#' 
#' See details of the \code{check_var_*()}-function documentation for more information.
#'   
#' @slot descr_requirements character. Text that describes the class requirements (e.g. 
#' numeric, character).
#' @slot descr_variable character. Text that describes what the data variable represents. 
#' @slot name_in_example character. Name the variable carried in the example 
#' data set uploaded in \code{assignVariables()}.
#' @slot valid logical. Indicates if the content of the assigned variable meets the 
#' variable specific requirements and is valid. 
#' 
#' @details 
#' To x- and y-coordinates can be referred to in many ways like \emph{x-coordinates,
#' x_coordinates, x_coords, x, etc.}. In \code{cypro} one refers to x-coordinates
#' via \emph{x_coords}. If the data set uploaded refers to x-coordinates
#' with \emph{x} instead of \emph{x_coords} problems would arise during all functions that need 
#' the information regarding x- (and -y) coordinates since it is unclear which variable
#' should be used as the names are not identical. Therefore, the user denotes 
#' the variable \emph{x} as the variable containing the x-coordinate information in 
#' \code{assignVariables()}. Afterwards, while being loaded every data file
#' is checked for the variable \emph{x}, which is then renamed to \code{x_coords} and added
#' to the cell data of the \code{Cypro} object.
#' 
#' All data variables that have a specific meaning and are recurrently used in \code{cypro} are
#' handled this way. Further (S4-Classes) exist that differentiate between three types of
#' assignable data variables: \code{RequiredVariable}, \code{OptionalVariable}, \code{ComputableVariable}.
#'  
#' @seealso \code{RequiredVariable}, \code{ComputableVariable}, \code{OptionalVariable}, \code{SummaryVariable} 
#' 
AssignableVariable <- setClass(Class = "AssignableVariable", 
                               slots = list(
                                 check_candidate = "function", 
                                 check_var = "function",
                                 descr_requirements = "character",
                                 descr_variable = "character", 
                                 name_in_example = "character",
                                 valid = "logical"),
                               contains = "DataVariable"
                               )


#' @title The RequiredVariable Class
#' 
#' @description Subclass of \code{DataVariable}. See details for more information.
#' 
#' @details A variable is considered to be required
#' if it can not be computed or summarized and thus represents the core of an 
#' analysis module. An analysis module can only be used if all of it's required variables
#' exist in the input data. In case of the migration analysis the variables \emph{x_coords}
#' and \emph{y_coords} must be part of the input data as they can not be computed based
#' on other variables.
#' 
#' @seealso \code{AssignableVariable}, \code{ComputableVariable}, \code{SummaryVariable}
#'
#' @export

RequiredVariable <- setClass(Class = "RequiredVariable", 
                             slots = list(), 
                             contains = c("DataVariable", "AssignableVariable")
                             )


#' @title The OptionalVariable Class
#' 
#' @description Subclass of \code{DataVariable}. Similar architecture to 
#' \code{RequiredVariable} but it differs from this class regarding what it represents, 
#' hence, the additional class. See details for more information.
#' 
#' @details A variable is considered optional if it is not required for a module to 
#' work and if it can neither be computed using required variables. This applies for the variables 
#' that assign a cell to their localisation on the well plate: \emph{well_plate}, 
#' \emph{well}, \emph{well_roi}. These variables can be part of the input data files
#' but do not have to be as there are other options to add this information to the 
#' \code{Cypro} object. 
#' 
#' @seealso \code{AssignableVariable}, \code{RequiredVariable}, \code{SummaryVariable}, 
#' \code{loadDataFiles()}, \code{loadData()}. 
#' 
OptionalVariable <- setClass(Class = "OptionalVariable", 
                             slots = list(), 
                             contains = c("DataVariable", "AssignableVariable")
                             )



#' @title The ComputableVariable Class
#' 
#' @description Subclass of \code{DataVariable}. See details for more information.
#'
#' @slot compute_with function. A function that takes the input data as the first 
#' argument \code{df} and the \code{Cypro} object as the second argument and uses 
#' the information of both to compute the variable. The function must return only 
#' the input data.frame now containing the variable that has just been computed.
#' 
#' @details Computable variables do not have to be part of the input data in order
#' to use the analysis modules they are part of. As long as the required variables of
#' the analysis module exist in the input data they can be computed using the function 
#' of slot @@compute_with. 
#' 
#' E.g. the data variable distance from last point (\emph{dflp}) is part of the migration analysis module. 
#' It describes the distance a cell has traveled during the time that passed between 
#' two frames. The output of some image analysis software (e.g. CellTracker) contain 
#' this by default. Others do not. As the distance from last point can be computed via
#' x- and y-coordinates it is a computable variable.
#' 
#' @seealso \code{AssignableVariable}, \code{RequiredVariable}, \code{SummaryVariable}
#'
ComputableVariable <- setClass(Class = "ComputableVariable", 
                               slots = list(
                                 compute_with = "function", 
                                 compute_with_args = "character"
                               ), 
                               contains = c("DataVariable", "AssignableVariable")
                               )

#' @title The SummaryVariable Class
#' 
#' @description Subclass of \code{DataVariable}. See details for more information.
#' 
#' @slot summarize_with function. A function that takes the input data as the first 
#' argument \code{df}, the summarized data as the second argument \code{stat_df}
#' and the \code{Cypro} object as the third argument. It uses 
#' the information of both to compute the variable. The function must only return the 
#' stat data.frame now containing the additional variable that has just been summarized.
#' 
#' @details Summary variables represent variables of time lapse experiments that can be 
#' computed by summarizing other variables.
#'  
#' E.g. the total distance a cell has traveled throughout the imaging process is part of the
#' migration analysis module and referred to in \code{cypro} as \emph{total_dist}.
#' It can be summarized by adding up all distances a cell has 
#' traveled from frame to frame. (The so called distance from last point is referred to as 
#' \emph{dflp} in \code{cypro} and is a computable variable of the migration analysis module).  
#' 
#' @seealso \code{AssignableVariable}, \code{RequiredVariable}, \code{ComputableVariable}
#'
SummaryVariable <- setClass(Class = "SummaryVariable", 
                                 slots = list(
                                   name_in_cypro = "character",
                                   summarize_with = "function"
                                 ), 
                                 contains = "DataVariable"
                                 )

# -----




# ExperimentDesign --------------------------------------------------------

#' @title The ExperimentDesign Class 
#' 
#' @description S4-class that contains the information around the 
#' experiment design.
#'
#' @slot experiment character. The name of the \code{Cypro} object it is part of.
#' @slot example_df data.frame. The example spreadsheet uploaded during \code{assignVariables()}.
#' @slot example_dir character. The file directory of the loaded example spreadsheet.
#' @slot ias character. The image analysis software used to quantify the images. 
#' @slot variables_grouping character. Contains variable names that were denoted as
#' additional grouping variables. All variables denoted like this are converted to factors.
#' @slot variables_numeric character. Contains variable names that were denoted as
#' additional numeric variables. All variables denoted like this are converted to numeric.
#' @slot well_plates list. Contains as many S4-objects of class \code{WellPlate}
#' as defined during the interactive session of \code{designExperiment()}.
#' 
#' @seealso \code{ExperimentDesignScreening}, \code{ExperimentDesignTimeLapse}, 
#' \code{ExperimentDesignTimeLapseMP}
#' 

ExperimentDesign <- setClass(Class = "ExperimentDesign", 
                             slots = list(
                               example_df = "data.frame",
                               example_dir = "character",
                               experiment = "character",
                               ias = "character",
                               variables_grouping = "character",
                               variables_numeric = "character", 
                               well_plates = "list"
                             )
                             )


#' @title The ExperimentDesignScreening Class 
#' 
#' @description S4-class that contains the information around the 
#' experiment design in a \code{CyproScreening} object.
#' 
#' @details Inherits from class \code{ExperimentDesign}. Does not contain any 
#' additional information.
#' 
#' @seealso \code{ExperimentDesign}, \code{ExperimentDesignTimeLapse}, 
#' \code{ExperimentDesignTimeLapseMP}
#'
ExperimentDesignSreening <- setClass(Class = "ExperimentDesignScreening", 
                                     slots = list(), 
                                     contains = "ExperimentDesign"
                                     )

#' @title The ExperimentDesignTimeLapse Class 
#' 
#' @description S4-class that contains the information around the 
#' experiment design in a \code{CyproTimeLapse} object.
#' 
#' @details Inherits from class \code{ExperimentDesign} and contains additional
#' information about the imaging process.
#'
#' @slot n_frames numeric. Only relevant in case of time lapse experiments. The total
#' number of images that have been made for every well-region-of-interest (well-roi).
#' @slot interval numeric. Only relevant in case of time lapse experiments. 
#' The interval in between which the images were made. 
#' @slot interval_unit character. Only relevant in case of time lapse experiments. 
#' The unit of the numeric value specified in slot @@interval. One of 
#' \emph{'days', 'hours', 'minutes', 'seconds'}.
#' 
#' @seealso \code{ExperimentDesign}, \code{ExperimentDesignScreening}, 
#' \code{ExperimentDesignTimeLapseMP}
#'
ExperimentDesignTimeLapse <- setClass(Class = "ExperimentDesignTimeLapse", 
                                      slots = list(
                                        interval = "numeric", 
                                        interval_unit = "character",
                                        n_frames = "numeric"
                                      ), 
                                      contains = "ExperimentDesign"
                                      )

#' @title The ExperimentDesignTimeLapseMP Class 
#' 
#' @description S4-class that contains the information around the 
#' experiment design in a \code{CyproTimeLapseMP} object. 
#' 
#' @details Inherits from class \code{ExperimentDesignTimeLapse} and contains
#' additional information about the phase spans.
#'
#' @slot phases list. A list of numeric vectors. 
#' 
#' @seealso \code{ExperimentDesignScreening}, \code{ExperimentDesignTimeLapse}, 
#' \code{ExperimentDesignTimeLapseMP}
#'
ExperimentDesignTimeLapseMP <- setClass(Class = "ExperimentDesignTimeLapseMP", 
                                        slots = list(
                                          phases = "list"
                                        ), 
                                        contains = "ExperimentDesignTimeLapse"
                                        )



# -----


# DataFile --------------------------------------------------------------


#' @title The DataFile Class
#' 
#' @description S4-class that contains the information about every file loaded. 
#'
#' @slot added logical. TRUE if all variables of slot @@content were valid and 
#' the data has been integrated in the \code{Cypro} object slot @@cdata.
#' @slot content list. Every file is loaded and every variable is checked for its
#' content according to what has been assigned during \code{AssignVariables()}. 
#' Slot @@content contains the output of all check functions of slot @@check_var.
#' @slot directory character. The directory from which the file was loaded.
#' @slot valid logical. TRUE if no variable of slot @@content is a value of class 
#' \code{glue} - meaning that all @@check_var functions returned a valid variable
#' and no error/feedback message.
#' @slot well_plate character. The well plate belonging.
#'
#' @export
#'
DataFile <- setClass(Class = "DataFile", 
                       slots = list(
                         content = "list",
                         file_status = "character",
                         directory = "character",
                         loading_modality = "character",
                         name = "character",
                         transferred = "logical",
                         valid = "logical", 
                         well_plate = "character"
                       ))


# -----

# Progress ----------------------------------------------------------------

#' @title The Progress Class
#' 
#' @description S4-class that keeps track of the necessary function
#' calls that have to be made in order to use a \code{Cypro}-object.
#'
#' @slot experiment character. The name of the \code{Cypro} object it is part of.
#' @slot designExperiment logical. 
#' @slot assignVariables logical. 
#' @slot loadData logical. 
#' @slot processData logical.
#'
Progress <- setClass(Class = "Progress", 
                     slots = list(
                       experiment = "character",
                       designExperiment = "logical", 
                       assignVariables = "logical", 
                       loadData = "logical", 
                       processData = "logical"
                     ))


# -----


# WellPlate ---------------------------------------------------------------

#' @title The WellPlate Class
#' 
#' @description S4-classs that abstracts a well plate. It additionally carries
#' information about if and how the related data has been loaded.
#' 
#' @slot directory character. The directory assigned to the well plate during \code{loadData()}.
#' Is either a directory leading to a folder that contains the files to read in or a directory leading 
#' to a single file. Depends on the loading modality.
#' @slot experiment character. The name of the \code{Cypro} object it is part of.
#' @slot files list. List of lists named according to all files the directory of @@slot folder
#' contains. Each slot contains an object of class \code{LoadedFile}. 
#' @slot index numeric. The ordinal well plate number. E.g. if @@index = 2, it was the second
#' well plate that has been designed.
#' @slot name character. The well plate name.
#' @slot set_up data.frame. A data.frame carrying and additional S3-class \code{wp_design_df} or 
#' \code{wp_design_mp_df} in case of multiple phase experiments.
#' @slot type character. The well plate type. Currently one of
#' \emph{'2x3 (6)', '3x4 (12)', '4x6 (24)', '6x8 (48)','8x12 (96)'}.
#'
WellPlate <- setClass(Class = "WellPlate", 
                      slots = list(
                        directory = "character",
                        experiment = "character",
                        files = "list",
                        filetypes = "character",
                        layout = "data.frame", 
                        loading_modality = "character",
                        index = "numeric",
                        name = "character",
                        recursive = "logical",
                        type = "character"
                      ))


# -----


# Cypro + -----------------------------------------------------------------

#' @title The Cypro Class
#' 
#' @description The \code{Cypro} object is a representation of high-content-screening (HCS)
#' and time-lapse-imaging experiment for R. In both cases images from cells have been 
#' made and the features of the captured cells have been quantified by image analyzing 
#' software such as \emph{CellProfer, CellTracker, ImageJ}. 
#' 
#' The \code{Cypro}-class is only the parent of the actual \code{Cypro}-classes that are 
#' used, namely \code{CyproScreening}, \code{CyproTimeLapse} and \code{CyproTimeLapseMP}.
#' 
#' @slot analysis list. A list of three slots containing results of statistical and machine-learning
#' techniques used on the numeric data of slot @@cdata. Currently implemented analysis pipelines
#' occupy the slots \emph{clustering}, \emph{correlation} and \emph{dim_red}.
#' @slot commands list. A list of commands run on this \code{Cypro}-object. 
#' @slot compatibility list. A list that can be used as a short term deposit for anything that isn't implemented
#' in \code{cypro}.
#' @slot default list. Contains the object specific default input for recurring arguments. Can 
#' be safely modified via \code{adjustDefaultInstructions()}.
#' @slot directory character. Directory under which the \code{Cypro}-object is stored by default 
#' using \code{saveCyproObject()}.
#' @slot experiment character. The name of the experiment. 
#' @slot experiment_design ExperimentDesign. An S4-class of class \code{ExperimentDesign}.
#' @slot feature_sets list. Each slot contains a character vector of names of numeric variables forming 
#' a \emph{feature_set}. Based on these features clustering and dimensional reduction can be 
#' performed. This allows to store several clustering results based on different features in one and the 
#' same \code{Cypro} object.
#' @slot information list. Miscellaneous information around the object.
#' @slot modules list. Each slot is again a list representing one of the modules that has been denoted 
#' during \code{assignVariables()}. In addition to the variable denotation it can contain 
#' module specific data, computation or analysis results. 
#' @slot progress Progress. S4-class of class \code{Progress.}
#' @slot quality_checks list. Contains results of quality check related results such as outlier detection.
#' @slot subsets list. Contains information of each subsetting process the \code{Cypro}-object
#' has gone through. See functions prefixed with \code{subsetBy*()}.
#' @slot version list. Three slots named \emph{patch, minor} and \emph{major} that keep 
#' track of the version of the object and the latest version of the cypro package.
#' 
#' @seealso \code{CyproScreening}-class, \code{CyproTimeLapse}-class, \code{CyproTimeLapseMP}-class
#'
Cypro <- setClass(Class = "Cypro", 
                  slots = list(
                    analysis = "list",
                    commands = "list",
                    compatibility = "list",
                    directory = "character",
                    default = "list",
                    experiment = "character",
                    experiment_design = "ExperimentDesign",
                    feature_sets = "list",
                    image_directories = "data.frame",
                    information = "list",
                    modules = "list",
                    progress = "Progress", 
                    quality_checks = "list",
                    subsets = "list",
                    version = "list"
                  )
)


#' @title The CyproScreening Class
#' 
#' @description The abstraction of high content screening experiments in which cells were imaged
#' only one time in contrast to time lapse experiments where cells are imaged several times in defined
#' intervals to track them. 
#' 
#' In addition to slot @@cdata it contains all slots of class \code{Cypro}.
#' 
#' @slot cdata Object of class \code{CdataScreening}. 
#' 
#' @seealso \code{Cypro}-class, \code{CdataScreening}-class
#' 
#' @export
#' 
CyproScreening <- setClass(Class = "CyproScreening", 
                           slots = list(
                             cdata = "CdataScreening", 
                             experiment_design = "ExperimentDesignScreening"
                           ), 
                           contains = "Cypro"
)

#' @title The CyproTimeLapse Class
#' 
#' @description The abstraction of time lapse imaging experiments in which cells were
#' imaged several times in defined intervals to track them. In contrast to the class
#' \code{CyproTimeLapseMP} where the suffix MP stands for multiple phases the condition 
#' in which cells existed did not change over the course of the experiment. Meaning that 
#' they were treated right before the imaging started.
#' 
#' In addition to slot @@cdata it contains all slots of class \code{Cypro}.
#' 
#' @slot cdata Object of class \code{CdataTimeLapse}. 
#' 
#' @seealso \code{Cypro}-class, \code{CdataTimeLapse}-class
#' 
#' @export
#' 
CyproTimeLapse <- setClass(Class = "CyproTimeLapse", 
                           slots = list(
                             cdata = "CdataTimeLapse", 
                             experiment_design = "ExperimentDesignTimeLapse"
                           ),
                           contains = "Cypro"
)

#' @title The CyproTimeLapseMP Class
#' 
#' @description The abstraction of time lapse imaging experiments in which cells were
#' imaged several times in defined intervals to track them. The suffix MP stands for multiple
#' phases. This describes a special kind of experiment design in which the condition of the cells
#' changes over time. E.g cells were treated with compound x right before the imaging started
#' and after 10 hours compound y was added.  
#' 
#' In addition to slot @@cdata it contains all slots of class \code{Cypro}.
#' 
#' @slot cdata Object of class \code{CdataTimeLapseMP}. 
#' 
#' @seealso \code{Cypro}-class, \code{CdataTimeLapseMP}-class
#' 
#' @export
#' 
CyproTimeLapseMP <- setClass(Class = "CyproTimeLapseMP", 
                             slots = list(
                               cdata = "CdataTimeLapseMP", 
                               experiment_design = "ExperimentDesignTimeLapseMP"
                             ),
                             contains = "Cypro"
)

# -----




# show method
setMethod(f = "show", signature = "cypro", definition = function(object){
  
  check_object(object)
  
  summary_list <- list()
  
  summary_list$class <- "An object of class 'cypro'.\n\n"
  
  summary_list$object_name <-stringr::str_c("Name: ", object@name)
  
  summary_list$exp_type <- stringr::str_c("\nType: ", pretty_exp_types[object@set_up$experiment_type])
  
  summary_list$n_cells <- stringr::str_c("\nNumber of Cells: ", nCells(object))
  
  # conditions --------------------------------------------------------------
  
  if(multiplePhases(object)){
    
    summary_list$conditions <- 
      purrr::map_chr(
        .x = getPhases(object), 
        .f = function(phase){
          
          all_conditions <- getConditions(object, phase = phase)
          
          text <- 
            stringr::str_c(
              "\n ", confuns::make_capital_letters(phase), " phase: '", 
              glue::glue_collapse(all_conditions, sep = "', '", last = "' and '"), 
              "'"
            )
          
          base::return(text)
          
        }
      ) %>% 
      stringr::str_c(collapse = "") %>% 
      stringr::str_c("\nConditions:", .)
    
    
  } else {
    
    summary_list$conditions <- 
      getConditions(object) %>% 
      glue::glue_collapse(sep = "', '", last = "' and '") %>% 
      stringr::str_c("\nConditions: ", "'", . , "'") 
    
  }
  
  # cell lines  -------------------------------------------------------------
  
  summary_list$cell_lines <- 
    getCellLines(object) %>% 
    glue::glue_collapse(sep = "', '", last = "' and '") %>% 
    stringr::str_c("\nCell Lines: ", "'", . , "'")   
  
  
  # well plates -------------------------------------------------------------
  
  summary_list$well_plates <- 
    getWellPlateNames(object) %>% 
    glue::glue_collapse(sep = "', '", last = "' and '") %>% 
    stringr::str_c("\nWell Plates: ", "'", . , "'")   
  
  
  # variable sets -----------------------------------------------------------
  
  if(base::identical(object@variable_sets, base::list())){
    
    summary_list$variable_sets <- "\nNo variables sets have been defined yet."
    
  } else {
    
    summary_list$variables_sets <- 
      getVariableSetNames(object) %>% 
      glue::glue_collapse(sep = "', '", last = "' and '") %>% 
      stringr::str_c("\nVariable Sets: '", ., "'") 
    
  }
  
  # print output ------------------------------------------------------------
  
  purrr::flatten_chr(summary_list) %>% 
    stringr::str_c(collapse = "") %>% 
    base::writeLines()
  
})

setMethod(f = "show", signature = "Cypro", definition = function(object){
  
  print("write that")
  
})



# Subset + ----------------------------------------------------------------



#' @title The CyproSubset Class
#' 
#' @description Subsetting allows to conveniently split data sets by certain
#' characteristics such as cell lines, conditions, cluster etc. or for specific
#' cell ids. All outlier removal functions of \code{cypro} base on this system
#' and it might be additionally useful if you want apply some machine learning 
#' algorithms such as clustering and correlation on only a subset of cells.
#' 
#' The \code{CyproSubset}-class contains information about the subsetting the 
#' \code{Cypro} object it is part of has undergone. This allows to trace 
#' all subsetting steps back to original data set with which downstream analysis
#' began.
#' 
#' object after the subsetting.
#' @slot new_name character. The content of slot @@experiment of the new \code{Cypro}
#' object.
#' @slot nth integer. 
#' @slot parent_name character. The content of slot @@experiment of the \code{Cypro}
#' object that underwent subsetting.
#' @slot reasoning character. Character string of length one that describes in 
#' a human readable way the reasoning behind the subsetting.
#' 
#' @seealso \code{CyproSubsetByCellID},\code{CyproSubsetByGroup}, \code{CyproSubsetByNumber}
#' 
#' @export
#' 
CyproSubset <- setClass(Class = "CyproSubset", 
                        slots = list(
                          new_name = "character",
                          nth = "integer",
                          parent_name = "character",
                          reasoning = "character"
                        )
                        )


#' @title The CyproSubsetByCellID Class
#' 
#' @description Every subsetting in \code{Cypro} happens by cell id. The 
#' \code{CyproSubsetByCellID} class contains information about 
#' the cell IDs that were kept and that were discarded. 
#' 
#' @slot ids_discarded character. The cell IDs that were discarded.
#' @slot ids_remaining character. The cell IDs that remained in the \code{Cypro}
#' 
#' @details Inherits from class \code{CyproSubset}.
#' 
#' @section Subset mechanism:
#' 
#' Cells with cell IDs that were not part of the input IDs are discarded.
#' 
CyproSubsetByCellID <- setClass(Class = "CyproSubsetByCellID", 
                                slots = list(
                                  ids_discarded = "character",
                                  ids_remaining = "character"
                                ), 
                                contains = "CyproSubset"
                                )


#' @title The CyproSubsetByGroup Class
#' 
#' @description Subsetting by group allows to only denote the groups
#' from a grouping variable that are supposed to be kept. The cell IDs
#' are then filtered and subsetting by cell ID takes place. 
#' 
#' The class \code{CyproSubsetByGroup} contains the denoted information 
#' that lead to the cell IDs that were used to subset the \code{Cypro}
#' object eventually. 
#' 
#' @slot grouping_variable character. The grouping variable that contains
#' the groups that are supposed to be kept.
#' @slot groups character. The groups whose cells are supposed to be kept.
#' 
#' @details Inherits from classes \code{CyproSubset} and \code{CyproSubsetByCellID}.
#' 
#' @section Subset mechanism:
#' 
#' The cell IDs that belong to the groups denoted are gathered. The object is 
#' then subsetted by the mechanism described in the documentation for S4-class
#' \code{CyproSubsetByCellID}.
#' 
CyproSubsetByGroup <- setClass(Class = "CyproSubsetByGroup", 
                               slots = list(
                                 grouping_variable = "character", 
                                 groups = "character"
                               ),
                               contains = c("CyproSubset", "CyproSubsetByCellID")
                               )


#' @title The CyproSubsetByNumber Class
#' 
#' @description The class \code{CyproSubsetByGroup} contains the denoted information 
#' that lead to the cell IDs that were used to subset the \code{Cypro}
#' object eventually. See section 'Subset mechanism' for more information about the options.
#' 
#' @slot across character. The grouping variables across which 
#' to reduce the cell number. 
#' @slot n_by_group numeric. The number of cells that are selected by 
#' group in case of option \emph{groupwise}.
#' @slot n_total numeric. The number of cells the \code{Cypro} object 
#' is supposed to contain after the subsetting in case of either option \emph{total_fixed}
#' or option \emph{total_weighted}.
#' @slot weighted logical. Decides if the proportion of cells in each group is 
#' maintained (weighted = TRUE, option \emph{total_weighted}) or is equalized
#' (weighted = FALSE, option \emph{total_fixed}).
#' @slot option character. The option according to which the subsettign took place.
#' 
#' @details Inherits from classes \code{CyproSubset} and \code{CyproSubsetByCellID}.
#' 
#' @section Subset mechanism:  
#' 
#' Subsetting by number does not subset by anything 
#' specific but simply reduces the number of cells in the object by random
#' selection across certain grouping variables. There are three ways to subset by number:
#' 
#'  \itemize{
#'   \item{\emph{groupwise:}}{ A fixed number of cells is picked from every group. The 
#'   resulting total number of cells in the new \code{Cypro} object is the sum of groups
#'   of the chosen grouping variable multiplied by the denoted number.}
#'   \item{\emph{total_fixed:}}{ The number of cells in the whole data set is reduced 
#'   to a fixed number. In the new \code{Cypro} object very group of the grouping variable
#'   across which the data is subsetted contains the same number of cells.}
#'   \item{\emph{total_weighted:}}{ The number of cells in the whole data set is reduced 
#'   to a fixed number. In the new \code{Cypro} object the original proportion of cells in
#'   the groups of the grouping variable across which the object is subsetted stays the same.}
#'   } 
#' 
CyproSubsetByNumber <- setClass(Class = "CyproSubsetByNumber", 
                                slots = list(
                                  across = "character",
                                  n_by_group = "numeric", 
                                  n_total = "numeric",
                                  weighted = "logical",
                                  option = "character",
                                  seed = "numeric"
                                ), 
                                contains = c("CyproSubset", "CyproSubsetByCellID")
                                )




# -----

