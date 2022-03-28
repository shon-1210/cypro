### Aufgabe Jan
library(readr)
library(utils)
library(tibble)
library(dplyr)
library(ggplot2)
library(tidyverse)

track_df <- 
  utils::read.csv(file = "~/Desktop/Aufgaben_Jan/tracking_tmz_ly34.csv", header = TRUE, sep = ";") %>% 
  tibble::as_tibble()


inorig_df <- readxl::read_xls(path = "~/Desktop/Aufgaben_Jan/A4_3_Tracks.xls", sheet = 1) 
inorig_df %>% gorup_by(cell_line)

# a) Was genau ist die Observation. Nenn es mir so prägnant wie möglich.

# jede Observation entspricht eine bestimmte Zelle(cell_id), die zu einer bestimmten Zeit fotografiert 
# wurde(frame_num) mit den entsprechenden X- und Y-Koordinaten, ...

# b) Warum ist die Aussage "Observations im data.frame `track_df` sind Zellen falsch bzw. zu ungenau?`
# Zellen bezeichnet in einer Taballe jeweils immer ein Eintrag, eine Observation bezeichnet die gesamte Zeile/Row.
# Somit besteht jede Observation aus mehreren Zellen. Zellen werden mit Koordinatnen, entsprechend ihrer Position
# innerhalb der Tablle angegeben, dies bezieht sich allerdings nciht auf die X- und Y-Koordinaten aus der Tabelle

# c) angenommen die Variable `cell_id` ist nicht vorhanden, und das data.frame sähe so aus ..
dplyr::mutate(
  .data = track_df, 
  cell_id = stringr::str_extract(cell_id, pattern = "\\d") # extrahiert erste "digit" im string
)
# mit einer Variable aus dem df geht das jetzt leider nicht mehr, daher sollte man zur Identifizierung 
# eine Kombination aus mehreren Variablen nehmen: cell_line, well, well_roi, condition, frame_num 
unique(track_df["well_roi"])
unique(track_df["frame_num"]) # zu 24punkten wurden Bilder gemacht
unique(track_df["condition"])
unique(track_df["cell_line"])


track_df["CID_3_WI_A4_1_WP_1",]

track_df <- 
  utils::read.csv(file = "tracking_tmz_ly34.csv") %>% 
  tibble::as_tibble()


track_df <- 
  dplyr::mutate(
    .data = track_df, 
    dplyr::across(.cols = where(is.character) & -cell_id, .fns = as.factor)
  )
track_df



# function #
x_origin <- df_without_angle_vars[df_without_angle_vars$frame_num == 1, "x_coords"]
y_origin <- df_without_angle_vars[df_without_angle_vars$frame_num == 1, "y_coords"]

x_coords <- df_without_angle_vars[df_without_angle_vars$frame_num != 1, "x_coords"]
y_coords <- df_without_angle_vars[df_without_angle_vars$frame_num != 1,]



compute_angle_form_origin <- function(){
  mutate(afo <- round(atan2((y_coords - y_origin), (x_coords - x_origin) * 180 / pi)))
  if (afo < 0) {
    afo <- afo * -1
  }
}


afo <- function(df_without_angle_vars, group_var, summary_var) {
  
  df_without_angle_vars %>%
    group_by(cell_id, fram_num)
  
  if(frame_num == 1) {x_origin = x_coords, y_origin = y_coords}
  if(frame_num != 1) {x_coord = x_coords, y_coord = y_coords}
  
  mutate(afo <- round(atan2((y_coords - y_origin), (x_coords - x_origin) * 180 / pi)))
  if (afo < 0) {
    afo <- afo * -1
  }
}

afo <- function(df_without_angle_vars, col_name){
  require("dplyr")
  df_without_angle_vars %>% 
    group_by_(cell_id, frame_num)
  mutate(afo <- round(atan2((y_coords - y_origin), (x_coords - x_origin) * 180 / pi))
         if(afo < 0) {
           afo <- afo * -1
         }
         return(df_without_angle_vars)
}
###


my_fun <- function(object = df_without_angle_vars, columns = c("x_coords", "y_coords")) {                # Write user-defined function
  df_without_angle_vars %>% 
    group_by_(cell_id) %>% 
    if(frame_num == 1) {
      x_origin == x_coords +
        y_origin == y_coords
    }
  # x_origin <- df_without_angle_vars[df_without_angle_vars$frame_num == 1, c("x_coords", "y_coords")]
  # x_origin <- df_without_angle_vars[ , columns[1]]
  #y_origin <- df_without_angle_vars[ , columns[2]]
  # add_column(afo = if_else(.$frame_num != 1, round(atan2((.$y_coords - y_origin), (.$x_coords - x_origin) * 180 / pi))))
  mutate(afo = round(atan2((y_coords - y_origin), (x_coords - x_origin) * 180 / pi)))
  if(afo < 0) {
    afo <- afo * -1
  }.
  return(df_without_angle_vars)
}

df_without_angle_vars %>%
  group_by(cell_id) %>% 
  mutate(y = (y_coords - lag(y_coords)), x = x_coords - lag(x_coords)) %>% 
  mutate( aflp = atan2(y, x) * 180 / pi) %>% 
  if(aflp< 0) {
    aflp <- aflp * -1
  }



mutate(aflp = round((atan2(y_coords - lag(y_coords), x_coords - lag(y_coords)) * 180 / pi)))
if(aflp < 0) {
  aflp <- aflp * -1
} 

df_without_angle_vars



df_without_angle_vars$frame_num[df_without_angle_vars$frame_num == 1] <- xy_origin

df_without_angle_vars[df_without_angle_vars$frame_num == 1, c("x_coords", "y_coords")]

my_fun(df_without_angle_vars)


df_without_angle_vars %>%  afo()

afo <- function(df, x_origin, y_origin, x_coords, y_coords) {
  
  df %>% dplyr::group_by(cell_id, frame_num) %>% 
    x_origin <- df$
}

# df_with_angle_vars <- 
df_without_angle_vars %>% 
  dplyr::group_by(cell_id) %>% 
  compute_angle_form_origin()
print(df_with_angle_vars)


df_without_angle_vars %>% 
  lapply(df_without_angle_vars, compute_angle_form_origin)




xy_coords <- function() {
  if (df_without_angle_vars$frame_num != 1) {
    x_coords == df_without_angle_vars$x_coords
  }
  if (df_without_angle_vars$frame_num != 1) {
    y_coords == df_without_angle_vars$y_coords
  }
}

xy_origin <- function() {
  if(is.na(dfo) {
    x_origin == x_coords 
  }
  # in df integrieren
  df_without_angle_vars <- dplyr::select(track_df, -afo, -aflp)
  
  
  frame_not_origin <- 2:24
  
  for (x in frame_not_origin) {
    if (df_without_angle_vars$frame_num == 1)
      x_origin == x_coords & y_origin == y_coords
  } else {
    x_coords == x_coords & y_coords == y_coords
  }
  
  
  
  ## zweiter Teil Aufagbe ##
  track_df <- 
    utils::read.csv(file = "~/Desktop/Aufgaben_Jan/tracking_tmz_ly34.csv", header = TRUE, sep = ";") %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(roi = stringr::str_extract(string = well_roi, pattern = "\\d*$") %>% base::as.numeric())
  
  track_df
  # Die Cell ID Variable setzt sich aus einer Nummer für jeder Zelle nach (zB CID_1), der Well_ROI (_A4_1) 
  # und der WP (wellplate (WP_1)) zusammen. Für jede Zelle auf jeder Wellplate in jeder well_roi gibt es dann eine 
  # einzigartige Nummer, die zusammen mit der Frame_num auch für den zeitlichen Verlauf einzigartig ist. 
  
  # Jede Platte hat jeweils nur eine Cellline und nur jeweils eine Condition, daher müssen diese NICHT in der Cell_id 
  # integriert sein. CellProfiler hat zum Zeirpunkt frame_num = 1 jeder Zelle eine Nummer gegeben (1-x). Die Cell_id setzt sich dann aus dieser,
  # der well-roi (beseteht wiederrum aus dem Well und der ROI (1-3) und der Well-plate (hier nur 1).
  
  track_df %>% group_by(cell_line) %>% summarise() %>% as.character()
  
  track_df["CID_60_WI_A7_2_WP_1", ]
  
  # well roi = jeder Bereich auf der Well-plate (zB A1) mit einer ROI(1-3)
  
  
  
  # ein bisschen hilfe ------------------------------------------------------
  
  mutate(
    .data = track_df, 
    afo = compute_angle_form_origin(x_coords, y_coords), 
    aflp = compute_angle_from_last_point(x_coords, y_coords)
  )
  
  first_cell_id <- "CID_1_WI_A4_1_WP_1"
  
  first_cell_id <- unique(track_df$cell_id) %>% head(1)
  
  
  track_df1 <- filter(track_df, cell_id == first_cell_id)
  
  
  #' @title Compute the distance between to points
  #'
  #' @param starting_pos,final_pos Numeric vector of length two. Denotes the two positions 
  #' between which the distance is calculated 
  #'
  #' @return A numeric value. 
  #'
  
  compute_distance <- function(starting_pos, final_pos){
    
    # direction vector
    drvc <- final_pos - starting_pos
    
    # compute effective distance traveled ( = value of direction vector)
    base::sqrt(drvc[1]^2 + drvc[2]^2)
    
  }
  
  #' @title Compute the distances from origin 
  #' 
  #' @description The first values of both input-vectors are taken as 
  #' the origin position. For each subsequent position the respective 
  #' distance to the position of origin is computed. 
  #' 
  #' To be used in \code{dplyr::mutate()} to recalculate the distances 
  #' of origin if the experiment includes time displaced treatment.
  #'
  #' @param x_coords Numeric vector. Refers to x-coordinates of object. 
  #' @param y_coords Numeric vector. Refers to y-coordinates of object.
  #'
  #' @return A numeric vector that corresponds to the respective distances from 
  #' the first position. 
  #' 
  
  compute_distances_from_origin <- function(x_coords, y_coords, object){
    
    n_coords <- base::length(x_coords)
    
    origin <- c(x_coords[1], y_coords[1])
    
    subsequent_positions <- 
      purrr::map2(
        .x = x_coords[2:n_coords],
        .y = y_coords[2:n_coords],
        .f = ~ c(.x, .y)
      )
    
    distances_from_origin <- 
      purrr::map_dbl(
        .x = subsequent_positions,
        .f = ~ compute_distance(starting_pos = origin, final_pos = .x)
      ) %>% 
      base::round(digits = 2) %>% 
      purrr::prepend(values = 0)
    
    return(distances_from_origin)
    
  }
  
  #' @rdname compute_distances_from_origin
  compute_distances_from_last_point <- function(x_coords, y_coords){
    
    n_coords <- base::length(x_coords)
    
    origins <- 
      purrr::map2(
        .x = x_coords[1:(n_coords-1)],
        .y = y_coords[1:(n_coords-1)],
        .f = ~ c(.x, .y)
      )
    
    subsequent_positions <- 
      purrr::map2(
        .x = x_coords[2:n_coords],
        .y = y_coords[2:n_coords],
        .f = ~ c(.x, .y)
      )
    
    distances_from_last_point <- 
      purrr::map2(
        .x = origins, 
        .y = subsequent_positions, 
        .f = ~ compute_distance(starting_pos = .x, final_pos = .y)
      ) %>% 
      purrr::flatten_dbl() %>% 
      purrr::prepend(values = 0)
    
    return(distances_from_last_point)
    
  }
  
  
  track_df_test <- track_df %>% select(-dfo, -dflp)
  
  # dplyr
  ids_one_time <- 
    track_df %>% 
    group_by(cell_id) %>% 
    summarise(count = n()) %>% 
    filter(count <= 1) %>% 
    pull(cell_id)
  
  track_df_test2 <- 
    group_by(track_df_test, cell_id) %>% 
    filter(!cell_id %in% ids_one_time) %>% 
    mutate(
      dfo = compute_distances_from_origin(x_coords = x_coords, y_coords = y_coords), 
      dflp = compute_distances_from_last_point(x_coords = x_coords, y_coords = y_coords)
    )