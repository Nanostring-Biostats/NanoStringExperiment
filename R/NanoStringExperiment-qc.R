#' Set quality control flags
#' 
#' Flag samples or features that fail quality control checks
#' 
#' @param object NanoStringExperiment object
#' @param ... additional parameters to pass to methods
#' 
#' @return NanoStringExperiment object
#' 
#' @export
setGeneric("setQCFlags", signature = "object", 
    function(object, ...) standardGeneric("setQCFlags"))