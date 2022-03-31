#' Normalize an assay
#' 
#' Normalize assay data based on chosen method
#' 
#' @export
setGeneric("normalize", signature = "object", 
    function(object, ...) standardGeneric("normalize"))