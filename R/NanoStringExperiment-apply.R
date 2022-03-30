# Other 
#munge
#summary
#setQCFlags
#esBy

#' Apply across an assay
#' 
#' Apply function row (feature) or column (sample)-wise
#' to selected assay
#' 
#' @export
setGeneric("assayDataApply", signature = "X", 
    function(X, MARGIN, FUN, ...) standardGeneric("assayDataApply"))

#' Apply across an assay
#' 
#' Apply function row (feature) or column (sample)-wise
#' to selected assay
#' 
#' @export
setGeneric("assayDataApply", signature = "X", 
    function(X, MARGIN, FUN, ..., elt = "exprs") {
        exprsAssay <- assay(X, elt)
        return(apply(exprsAssay, MARGIN, FUN, ...))
    })

#' Apply across default exprs assay
#' 
#' Apply function row (feature) or column (sample)-wise
#' to the default "exprs" assay
#' 
#' @importMethodsFrom Biobase esApply
#' 
#' @export
setMethod("esApply", signature = "NanoStringExperiment",
    function(X, MARGIN, FUN, ...) {
        exprsAssay <- assay(X, "exprs")
        return(apply(exprsAssay, MARGIN, FUN, ...))
    })

