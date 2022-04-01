#' Normalize an assay
#' 
#' Normalize assay data based on chosen method
#' 
#' @return NanoStringExperiment object
#' 
#' @export
setGeneric("normalize", signature = "object", 
    function(object, ...) standardGeneric("normalize"))