


# cypro-object -------------------------------------------------------


### old

cypro <- setClass(Class = "cypro", 
                  slots = c(
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

### new - cdata - parent 



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

### Progress 

#' @title Progress of Cypro Object
#' 
#' @description S4-object that keeps track of the necessary function
#' calls that have to be made in order to use a \code{Cypro}-object.
#'
#' @slot designExperiment logical. 
#' @slot denoteVariables logical. 
#' @slot loadData logical. 
#'
Progress <- setClass(Class = "Progress", 
                     slots = list(
                       designExperiment = "logical", 
                       denoteVariables = "logical", 
                       loadData = "logical"
                     ))

### Well Plate

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
#' during \code{denoteVariables()}. In addition to the variable denotation it can contain 
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
                  slots = c(
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
                           slots = c(
                             cdata = "CdataScreening"
                           ), 
                           contains = c("Cypro")
)

#' @rdname Cypro
#' @export
CyproTimeLapse <- setClass(Class = "CyproTimeLapse", 
                             slots = c(
                               cdata = "CdataTimeLapse"
                             ),
                           contains = c("Cypro")
)

#' @rdname Cypro
#' @export
CyproTimeLapseMP <- setClass(Class = "CyproTimeLapseMP", 
                                slots = c(
                                  cdata = "CdataTimeLapseMP"
                                ),
                             contains = c("Cypro")
)



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
