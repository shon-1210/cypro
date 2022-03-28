
library(tidyverse)

# einlesen 
track_df <- 
  utils::read.csv(file = "david_zimmer/Aufgabe_1/tracking_tmz_ly34.csv") %>% 
  tibble::as_tibble() %>% 
  dplyr::mutate(roi = stringr::str_extract(string = well_roi, pattern = "\\d*$") %>% base::as.numeric())

# in etwa so sieht der original output von Cell Tracker pro Image Stack aus 
orig_df <- readxl::read_xls(path = "david_zimmer/Aufgabe_1/A4_3_Tracks.xls", sheet = 1)

# abkürzung der variablen: 
# afo = angle from origin, dfo = distance from origin, speed = speed, 
# aflp = angle from last point, dflp = distance from last point
# well_roi = well region of interest


# Vorbereitung: -----------------------------------------------------------

# mach dich mit dem datensatz vertraut. mach dir klar
# a) Was genau ist die Observation. Nenn es mir so prägnant wie möglich.
# b) Warum ist die Aussage "Observations im data.frame `track_df` sind Zellen falsch bzw. zu ungenau?`
# c) angenommen die Variable `cell_id` ist nicht vorhanden, und das data.frame sähe so aus ... 

dplyr::mutate(
  .data = track_df, 
  cell_id = stringr::str_extract(cell_id, pattern = "\\d") # extrahiert erste "digit" im string
)

# - ... welche Variablen brauchst du, um jede einzelne observation einzigartig zu identifizieren, 
# sprich, was sind deine identifier/key variablen, die du benötigst?


# Wenn du die Aufgaben für a) b) und c) hast, schreib mir kurz. Dann nicke ich es ab, um sicher zu 
# gehen, dass du den Datensatz verstanden hast und dann gehts weiter. 



# ein bisschen Theorie:
# 1. Aufgabe: mach dir klar, was der Unterschied zwischen character variablen und factor 
# variablen ist 


# 2. Aufgabe: dplyr::mutate() müsstest du kennen. Wie funktioniert dplyr::across() innerhalb
# der funktion dplyr::mutate()?

track_df <- 
  dplyr::mutate(
    .data = track_df, 
    dplyr::across(.cols = where(is.character) & -cell_id, .fns = as.factor)
  )



# ein bisschen Praxis:

# 3. Aufgabe: die Variablen 'afo' (Angle from origin) and 'aflp' (Angle from last point) 
# sind Teil des Outputs von Cell Tracker http://www.celltracker.website/about-celltracker.html
# aber nicht von CellProfiler. Ich will diese Variablen also anhand der x- und y-koordinaten berechnen

# ich brauche zwei Funktionen

compute_angle_from_origin <- function(x_coords, y_coords){
  
  
  
}


compute_angle_form_last_point <- function(x_coords, y_coords){
  
  
  
  
}


# beide sollen wie folgt eingesetzt werden können: 

# data.frame, das die angle-variablen nicht beinhaltet
df_without_angle_vars <- dplyr::select(track_df, -afo, -aflp)


df_with_angle_vars <- 
  df_without_angle_vars %>% 
  #dplyr::group_by(cell_id) # brauchen wir hier group_by(), wenn ja für welche der funktionen? für beide? für eine?
                            # wie funktioniert group_by()
  dplyr::mutate(
    afo = compute_angle_from_origin(x_coords = x_coords, y_coords = y_coords), 
    aflp = compute_angle_from_last_point(x_coords = x_coords, y_coords = y_coords)
  )



# nutze track_df als kontrolle, ob deine funktionen funktionieren











