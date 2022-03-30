#' Set quality control flags
#' 
#' Flag samples or features that fail quality control checks
#' 
#' @export
setGeneric("setQCFlags", signature = "object", 
    function(object, ...) standardGeneric("setQCFlags"))