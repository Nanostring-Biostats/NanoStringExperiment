#' Subset to endogenous features
#' 
#' Method to subset to features with CodeClass Endogenous
#' 
#' @export
setGeneric("endogenousSubset", signature = "object", 
    function(object) standardGeneric("endogenousSubset"))

#' Subset to endogenous features
#' 
#' Method to subset to features with CodeClass Endogenous
#' 
#' @export
setMethod("endogenousSubset", "NanoStringExperiment", 
    function(object) subset(object, subset=CodeClass == "Endogenous"))

#' Subset to housekeeping features
#' 
#' Method to subset to features with CodeClass Housekeeping
#' 
#' @export
setGeneric("housekeepingSubset", signature = "object", 
    function(object) standardGeneric("housekeepingSubset"))

#' Subset to housekeeping features
#' 
#' Method to subset to features with CodeClass Housekeeping
#' 
#' @export
setMethod("housekeepingSubset", "NanoStringExperiment", 
    function(object) subset(object, subset=CodeClass == "Housekeeping"))

#' Subset to negative control features
#' 
#' Method to subset to features with CodeClass Negative
#' 
#' @export
setGeneric("negativeControlSubset", signature = "object", 
    function(object) standardGeneric("negativeControlSubset"))

#' Subset to negative control features
#' 
#' Method to subset to features with CodeClass Negative
#' 
#' @export
setMethod("negativeControlSubset", "NanoStringExperiment", 
    function(object) subset(object, subset=CodeClass == "Negative"))

#' Subset to positive control features
#' 
#' Method to subset to features with CodeClass Positive
#' 
#' @export
setGeneric("positiveControlSubset", signature = "object", 
    function(object) standardGeneric("positiveControlSubset"))

#' Subset to positive control features
#' 
#' Method to subset to features with CodeClass Positive
#' 
#' @export
setMethod("positiveControlSubset", "NanoStringExperiment", 
    function(object) subset(object, subset=CodeClass == "Positive"))

#' Subset to control features
#' 
#' Method to subset to features with CodeClass IsControl
#' 
#' @export
setGeneric("controlSubset", signature = "object", 
    function(object) standardGeneric("controlSubset"))

#' Subset to IsControl features
#' 
#' Method to subset to features with CodeClass IsControl
#' 
#' @export
setMethod("controlSubset", "NanoStringExperiment", 
    function(object) subset(object, subset=CodeClass == "IsControl"))

#' Subset to non-IsControl features
#' 
#' Method to subset to features not CodeClass IsControl
#' 
#' @export
setGeneric("nonControlSubset", signature = "object", 
    function(object) standardGeneric("nonControlSubset"))

#' Subset to non-IsControl features
#' 
#' Method to subset to features not CodeClass IsControl
#' 
#' @export
setMethod("nonControlSubset", "NanoStringExperiment", 
    function(object) subset(object, subset=CodeClass != "IsControl"))

#' Subset to signature features
#' 
#' Method to subset to features in signatures slot
#' 
#' @export
setGeneric("signatureSubset", signature = "object", 
    function(object) standardGeneric("signatureSubset"))

#' Subset to signature features
#' 
#' Method to subset to features in signatures slot
#' 
#' @export
setMethod("signatureSubset", "NanoStringExperiment", 
    function(object) {
        genes <- unique(names(unlist(unname(weights(signatures(object))))))
        subset(object, subset=CodeClass %in% genes)
    })
