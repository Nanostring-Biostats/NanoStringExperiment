#' Set quality control flags
#' 
#' Flag samples or features that fail quality control checks
#' 
#' @return NanoStringExperiment object
#' 
#' @export
setGeneric("setQCFlags", signature = "object", 
    function(object, ...) standardGeneric("setQCFlags"))