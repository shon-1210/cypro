#' @include get-family.R
#' 
NULL


# for Cypro and Cypro sub -------------------------------------------------

#' @title Basic extractor functions
#'
#' @inherit argument_dummy params
#' 
#' @return The respective slot content.
#' 

setGeneric(name = "getCdata", def = function(object){
  
  standardGeneric(f = "getCdata")
  
})

#' @rdname getCdata
#' @export
setMethod(f = "getCdata", signature = "Cypro", definition = function(object){
  
  check_object(object)
  
  return(object@cdata)
  
})


setGeneric(name = "getExperimentDesign", def = function(object){
  
  standardGeneric(f = "getExperimentDesign")
  
})

#' @rdname getExperimentDesign
#' @export
setMethod(f = "getExperimentDesign", signature = "Cypro", function(object){
  
  object@design
  
})






# for Cdata and Cdata sub  ------------------------------------------------

# not sure yet where and how to document

setMethod(f = "getClusterDf", signature = "Cdata", definition = function(object, ...){
  
  object@cluster
  
})

setMethod(f = "getClusterDf", signature = "CdataTimeLapseMP", definition = function(object, phase, ...){
  
  confuns::check_one_of(
    input = phase, 
    against = base::names(object@cluster)
  )
  
  return(object@cluster[[phase]])
  
})

setMethod(f = "getFeatureDf", signature = "CdataScreening", definition = function(object, ...){
  
  return(object@features)
  
})

setMethod(f = "getMetaDf", signature = "Cdata", definition = function(object, ...){
  
  return(cdata@meta)
  
})

setMethod(f = "getMetaDf", signature = "CdataTimeLapseMP", definition = function(object, phase, ...){
  
  confuns::check_one_of(
    input = phase, 
    against = base::names(object@meta)
  )
  
  return(object@meta[[phase]])
  
})

setMethod(f = "getStatsDf", signature = "CdataTimeLapse", definition = function(object, ...){
  
  return(object@features_stats)
  
})

setMethod(f = "getStatsDf", signature = "CdataTimeLapseMP", definition = function(object, phase, ...){
  
  confuns::check_one_of(
    input = phase, 
    against = base::names(object@features_stats)
  )
  
  return(object@features_stats)
  
})

setMethod(f = "getTracksDf", signature = "CdataTimeLapse", definition = function(object, ...){
  
  return(object@features_tracks)
  
})

setMethod(f = "getTracksDf", signature = "CdataTimeLapseMP", definition = function(object, phase, ...){
  
  confuns::check_one_of(
    input = phase, 
    against = base::names(object@features_tracks)
  )
  
  return(object@features_stats)
  
})




