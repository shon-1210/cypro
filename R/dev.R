


dir <- 
  "E:\\Research Data\\Broad Institute\\MCF7 Imaging\\Original Images\\Week1_22123\\Week1_150607_B04_s4_w261B79A05-7534-46F3-8C80-D67A89A3A125.tif"


setImageDirectories <- function(object,
                                folder = NULL,
                                directories = NULL,
                                rgx_well_plate_name = NULL,
                                rgx_well_plate_index = NULL,
                                rgx_well = NULL, 
                                rgx_roi = NULL){
  
  validate_only_one_arg_specified(
    input = list(folder = folder, directories = directories)
  )
  
  validate_only_one_arg_specified(
    input = list(
      rgx_well_plate_name = rgx_well_plate_name,
      rgx_well_plate_index = rgx_well_plate_index
      )
  )
  
}

HALLMARK <- msigdb_download("Homo sapiens", "H", "")
names(HALLMARK) <- clean_genesets(names(HALLMARK))
head(names(HALLMARK))

























