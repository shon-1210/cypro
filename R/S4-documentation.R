


# cypro-object -------------------------------------------------------


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


### new

#' @title Cypro Cell Data 
#' 
#' @description The parent S4-object of every \code{Cypro}-objects \code{@@cdata}
#' slot. Contains the experiment data in form of data.frames. All data.frames 
#' are functionally connected by their common data variable \emph{cell_id} with which 
#' they can be joined via \code{dplyr::left_join()}. 
#'
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

#' @title Cypro Cell Data - Screening Experiments
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
#'
CdataScreening <- setClass(Class = "CdataScreening", 
                           slots = list(
                             cluster = "data.frame",
                             features = "data.frame",
                             meta = "data.frame"
                           ), 
                           contains = "Cdata")

#' @title Cypro Cell Data - Time Lapse Experiments
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
#' 

CdataTimeLapse <- setClass(Class = "CdataTimeLapse", 
                           slots = list(
                             cluster = "data.frame", 
                             features_stats = "data.frame",
                             features_tracks = "data.frame",
                             meta = "data.frame"
                             
                           ),
                           contains = "Cdata")


#' @title Cypro Cell Data - Time Lapse Experiments - Multiple Phases
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
#' 
CdataTimeLapseMP <- setClass(Class = "CdataTimeLapseMP", 
                             slots = list(
                               cluster = "list",
                               meta = "list",
                               features_stats = "list", 
                               features_tracks = "list"
                             ),
                             contains = "Cdata")

### Experiment Design 

#' @title Experiment Design 
#' 
#' @description S4-objet that contains the information around the 
#' experiment design. Of vital importance for all data loading functions. 
#'
#' @slot frames numeric. Only relevant in case of time lapse experiments. The total
#' number of images that have been made for every well-region-of-interest (well-roi).
#' @slot interval numeric. Only relevant in case of time lapse experiments. 
#' The interval in between which the images were made. 
#' @slot interval_unit character. Only relevant in case of time lapse experiments. 
#' The unit of the numeric value specified in slot @@interval. One of 
#' \emph{'days', 'hours', 'minutes', 'seconds'}.
#' @slot type character. The experiment type. One of \emph{'screening', 'time_lapse',
#' 'time_lapse_mp'}.
#' @slot well_plates list. Contains as many S4-objects of class \code{WellPlate}
#' as defined during the interactive session of \code{designExperiment()}.
#' 
#' @details E.g. @@frames = 25, @@interval = 1, @@interval_unit = 'hours' means 
#' that during the experiment every well region of interest was captured 25 times. 
#' The read in data should therefore contain 25 rows for every observed cell - assuming 
#' that no cells emerged due to mitosis or went missing due to imaging artifacts.
#'
ExperimentDesign <- setClass(Class = "ExperimentDesign", 
                             slots = list(
                               interval = "numeric", 
                               interval_unit = "character",
                               frames = "numeric", 
                               type = "character",
                               well_plates = "list"
                             ))


#' @title Progress of Cypro Object
#' 
#' @description S4-object that keeps track of the necessary function
#' calls that have to be made in order to use a \code{Cypro}-object.
#'
#' @slot designExperiment logical. 
#' @slot assignVariables logical. 
#' @slot loadData logical. 
#'
Progress <- setClass(Class = "Progress", 
                     slots = list(
                       designExperiment = "logical", 
                       assignVariables = "logical", 
                       loadData = "logical"
                     ))


#' @title Cypro Well Plate Design
#' 
#' @description S4-objects that abstract the design of every well plate. 
#' It additionally carries information about if and how the related data has been 
#' loaded.
#'
#' @slot experiment character. The name of the \code{Cypro}-object it is part of.
#' @slot files character. All spreadsheet-files directories that have been loaded 
#' for that well plate.
#' @slot folder character. Only relevant in case of usage of \code{loadFilesByWellRoi()}.
#' Contains the directory to the folder from which the files have been loaded.
#' @slot loaded_by character. The function that has been used to load data for this 
#' well plate. 
#' @slot index numeric. The ordinal well plate number. E.g. if @@index = 2, it was the second
#' well plate that has been designed.
#' @slot name character. The well plate name.
#' @slot set_up data.frame. A data.frame carrying and additonal S3-class \code{wp_design_df} or 
#' \code{wp_design_mp_df} in case of multiple phase experiments.
#' @slot type character. The well plate type. Currently one of
#' \emph{'2x3 (6)', '3x4 (12)', '4x6 (24)', '6x8 (48)','8x12 (96)'}.
#'
WellPlate <- setClass(Class = "WellPlate", 
                      slots = list(
                        experiment = "character",
                        files = "character",
                        folder = "character",
                        loaded_by = "character",
                        index = "numeric",
                        name = "character",
                        set_up = "data.frame", 
                        type = "character"
                      ))

### new cypro classes


#' @title The Cypro Class
#' 
#' @description The \code{Cypro}-object is a representation of high-content-screening (HCS)
#' and time-lapse-imaging experiment for R. In both cases images from cells have been 
#' made and the features of the captured cells have been quantified by image analyzing 
#' software such as \emph{CellProfer, CellTracker, ImageJ}. 
#' 
#' For details on slot @@cdata see documentation for S4-classes \code{Cdata}.
#' 
#' @slot analysis list. A list of three slots containing results of statistical and machine-learning
#' techniques used on the numeric data of slot @@cdata. Currently implemented analysis pipelines
#' occupy the slots \emph{clustering}, \emph{correlation} and \emph{dim_red}.
#' @slot commands list. A list of commands run on this \code{Cypro}-object. 
#' @slot compatibility list. A list that can be used as a short term deposit for anything that isn't implemented
#' in \code{cypro}.
#' @slot default list. Contains the object specific default input for recurring arguments. Can 
#' be savely modified via \code{adjustDefaultInstructions()}.
#' @slot design ExperimentDesign. An S4-object of class \code{ExperimentDesign}.
#' @slot feature_sets list. Each slot contains a character vector of names of numeric variables forming 
#' a \emph{feature_set}. Based on these features clustering and dimensional reduction can be 
#' performed. This allows to store several clustering results based on different features in one and the 
#' same \code{Cypro}-object.
#' @slot information list. Misellaneous information around the object.
#' @slot modules list. Each slot is again a list representing one of the modules that has been denoted 
#' during \code{assignVariables()}. In addition to the variable denotation it can contain 
#' module specific data, computation or analysis results. 
#' @slot name character. The name of the experiment.
#' @slot progress Progress. S4-object of class \code{Progress.}
#' @slot qcheck list. Contains results of quality check related results such as outlier detection.
#' @slot storage character. Directory under which the \code{Cypro}-object is stored by default 
#' using \code{saveCyproObject()}.
#' @slot subsets list. Contains information of each subsetting process the \code{Cypro}-object
#' has gone through. See functions prefixed with \code{subsetBy*()}.
#' @slot version list. Three slots named \emph{patch, minor} and \emph{major} that keep 
#' track of the version of the object and the latest version of the cypro package.
#'
Cypro <- setClass(Class = "Cypro", 
                  slots = list(
                    analysis = "list",
                    commands = "list",
                    compatibility = "list",
                    default = "list",
                    design = "ExperimentDesign",
                    feature_sets = "list",
                    information = "list",
                    modules = "list",
                    name = "character",
                    progress = "Progress", 
                    qcheck = "list",
                    storage = "character",
                    subsets = "list",
                    version = "list"
                  )
)


#' @rdname Cypro
#' @export
CyproScreening <- setClass(Class = "CyproScreening", 
                           slots = list(
                             cdata = "CdataScreening"
                           ), 
                           contains = "Cypro"
)

#' @rdname Cypro
#' @export
CyproTimeLapse <- setClass(Class = "CyproTimeLapse", 
                           slots = list(
                             cdata = "CdataTimeLapse"
                           ),
                           contains = "Cypro"
)

#' @rdname Cypro
#' @export
CyproTimeLapseMP <- setClass(Class = "CyproTimeLapseMP", 
                             slots = list(
                               cdata = "CdataTimeLapseMP"
                             ),
                             contains = "Cypro"
)


#' @title The Variable Class
#' 
#' @description The S4-class \code{DataVariable} is an object oriented approach to 
#' solve the problem of different image analysis software following different
#' naming conventions. 
#' 
#' @slot name_in_cypro character. Short name to be used as reference throughout
#' \code{cypro}. Should follow underscore-naming-convention (e.g. \emph{x_coords} instead of 
#' \emph{xCoords} or \emph{x-coords})
#' 
#' @details The 'Variable-Ecosystem' of \code{cypro} attempts to solve the problem of different
#' image analysis software following different naming conventions. The reasoning behind this goes 
#' as follows: Columns that refer to certain data variables such as x- and y-coordinates, frame number, cell perimeter, 
#' etc. complicate writing specific code. E.g. cellular migration is 
#' commonly visualized by rose plots (see function \code{plotRosePlot()}). This function refers to 
#' x- and y-coordinates. If the input data, however, can refer to coordinates in different ways like 
#' \emph{x}, \emph{x_coordinates}, \emph{x-coordinates}, the code becomes unreliable. While 
#' unspecific data variables visualized by boxplots or density plots can follow arbitrary 
#' naming conventions variables that are used repeatedly in \code{cypro}-intern code must follow a
#' consistent system to ensure flexibility regarding the data input while maintaining reliable code. 
#' 
#' @seealso \code{AssignableVariable}, \code{RequiredVariable}, \code{ComputableVariable},
#' \code{SummarizableVariable}
#' 
#' @export
#'
DataVariable <- setClass(Class = "DataVariable", 
                         slots = list(
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
#' slot @@description or a single FALSE if not. This only refers to superficial things 
#' like class and types. 
#' 
#' The function must have only one argument, namely \code{var}.
#'
#' @slot check_content function. A more sophisticated function that takes several things 
#' into consideration to check if the content of the variable fits the modules requirements. 
#' @slot descr_requirements character. Text that describes the class requirements (e.g. 
#' numeric, character).
#' @slot descr_variable character. Text that describes what the variable stands for. 
#' @slot name_in_app character. Pretty name to be used as reference in application.
#' @slot name_in_example character. Name the variable carried in the example 
#' data set uploaded in \code{assignVariables()}.

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
#' is checked for the variable \emph{x}, which es then renamed to \code{x_coords} and added
#' to the cell data of the \code{Cypro} object.
#' 
#' All variables that have a specific meaning and are recurrently used in \code{cypro} are
#' handled this way. Two further (S4-Classes) exist that differentiate between two types of
#' assignable data variables: \code{RequiredVariable}, \code{ComputableVariable}.
#'  
#' @seealso \code{RequiredVariable}, \code{ComputableVariable}, \code{SummarizableVariable} 
#' 
AssignableVariable <- setClass(Class = "AssignableVariable", 
                               slots = list(
                                 check_candidate = "function", 
                                 check_content = "function",
                                 descr_requirements = "character",
                                 descr_variable = "character", 
                                 name_in_app = "character",
                                 name_in_example = "character"),
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
#' @seealso \code{AssignableVariable}, \code{ComputableVariable}, \code{SummarizableVariable}
#'
#' @export

RequiredVariable <- setClass(Class = "RequiredVariable", 
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
#' the input data.frame with the variable that has just been computed.
#' 
#' @details Computable variables do not have to be part of the input data in order
#' to use their associated analysis modules. As long as the required variables of
#' the analysis module exist in the input data they can be computed using the function 
#' of slot @@compute_with. 
#' 
#' E.g. the variable distance from last point (\emph{dflp}) is part of the migration analysis module. 
#' It describes the distance a cell has traveled during the time that passed between 
#' two frames. The output of some image analysis software (e.g. CellTracker) contain 
#' this by default. Others do not. As the distance from last point can be computed via
#' x- and y-coordinates it is a computable variable.
#' 
#' @seealso \code{AssignableVariable}, \code{RequiredVariable}, \code{SummarizableVariable}
#'
ComputableVariable <- setClass(Class = "ComputableVariable", 
                               slots = list(
                                 compute_with = "function", 
                                 compute_with_args = "character"
                               ), 
                               contains = c("DataVariable", "AssignableVariable")
                               )

#' @title The SummarizableVariable Class
#' 
#' @description Subclass of \code{DataVariable}. See details for more information.
#' 
#' @slot summarize_with function. A function that takes the input data as the first 
#' argument \code{df}, the summarized data as the second argument \code{stat_df}
#' and the \code{Cypro} object as the third argument. It uses 
#' the information of both to compute the variable. The function must only return the 
#' stat data.frame with the additional variable that has just been summarized.
#' 
#' @details Summarizable variables represent variables of time lapse experiments that can be 
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
SummarizableVariable <- setClass(Class = "SummarizableVariable", 
                                 slots = list(
                                   name_in_cypro = "character",
                                   summarize_with = "function"
                                 ), 
                                 contains = "DataVariable"
                                 )



#' @title The AnalysisModule Class
#' 
#' @description An analysis module in \code{cypro} represents a biological aspect
#' of cells that can be described and explored using a fixed set of data variables. 
#' See details for more information and examples. 
#' 
#' @slot descr character. Text that describes what the analysis module is about.
#' @slot name_in_app character. Pretty name to be used as reference in application.
#' @slot name_in_cypro character. character. Short name to be used as reference throughout
#' \code{cypro}. Should follow underscore-naming-convention (e.g. \emph{x_coords} instead of 
#' \emph{xCoords} or \emph{x-coords})
#' @slot variables_computed list. Named list of computable variables represented by the S4-Class
#' \code{ComputableVariable}.  Names of list correspond to the respective slot @@name_in_cypro.
#' Variable must be ordered according to the order in which they are supposed to be computed.
#' @slot variables_required list. Named list of required variables represented by the S4-Class
#' \code{RequiredVariable}.  Names of list correspond to the respective slot @@name_in_cypro.
#' 
#' @details Example: Cellular migration 
#' Cellular migration is a commonly explored aspect in time lapse image analysis.  

AnalysisModule <- setClass(Class = "AnalysisModule", 
                           slots = list(
                             descr = "character",
                             name_in_app = "character",
                             name_in_cypro = "character",
                             variables_computed = "list",
                             variables_required = "list"
                           ))


#' @title The AnalysisModuleTimeLapse Class
#' 
#' @description Subclass of S4-Class \code{AnalysisModule}. Specific class for modules
#' that can only be used in time lapse experiments.
#'
#' @slot variables_summarized list. Named list of summarizable variables represented by the 
#' S4-class \code{SummarizableVariables}. Names of list correspond to the repsective
#' slot @@name_in_cypro. Variable must be ordered according to the order in which they
#' are supposed to be summarized. E.g. as the migration efficiancy (\emph{mgr_eff}) requires
#' the variable total distance (\emph{total_dist}) the total distance must be listed 
#' before migration efficiency.
#' 
#' @export
#'
AnalysisModuleTimeLapse <- setClass(Class = "AnalysisModuleTimeLapse", 
                                    slots = list(
                                      variables_summarized = "list"
                                    ), 
                                    contains = "AnalysisModule")








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
