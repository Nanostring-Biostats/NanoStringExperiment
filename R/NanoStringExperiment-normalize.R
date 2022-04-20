#' Normalize an assay
#' 
#' Normalize assay data based on chosen method
#' 
#' @param object NanoStringExperiment object
#' @param ... additional parameters to pass to methods
#' 
#' @return NanoStringExperiment object
#' 
#' @export
setGeneric("normalize", signature = "object", 
    function(object, ...) standardGeneric("normalize"))