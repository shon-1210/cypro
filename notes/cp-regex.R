
library(stringr)


string <- choose.dir()

str_extract_all(string, pattern = ".{3}n(?!e-05)")


string <- "E:\\Milo DataBlue\\ImagingRed\\Ugne-05Green-29\\Vessel ID 1260\\Green (6h)"

str_extract_all(string, pattern = "(?<=\\d{4}\\\\)(Red|Green|Blue)")

str_extract_all(string, pattern = "(Red|Green|Blue)")


library(raster)

?stack
