#' Subset to endogenous features
#' 
#' Method to subset to features with CodeClass Endogenous
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' endogenousSubset(testExp)
#' 
#' @export
setGeneric("endogenousSubset", signature = "object", 
    function(object) standardGeneric("endogenousSubset"))

#' Subset to endogenous features
#' 
#' Method to subset to features with CodeClass Endogenous
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' endogenousSubset(testExp)
#' 
#' @export
setMethod("endogenousSubset", "NanoStringExperiment", 
    function(object) subset(object, subset=CodeClass == "Endogenous"))

#' Subset to housekeeping features
#' 
#' Method to subset to features with CodeClass Housekeeping
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' housekeepingSubset(testExp)
#' 
#' @export
setGeneric("housekeepingSubset", signature = "object", 
    function(object) standardGeneric("housekeepingSubset"))

#' Subset to housekeeping features
#' 
#' Method to subset to features with CodeClass Housekeeping
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' housekeepingSubset(testExp)
#' 
#' @export
setMethod("housekeepingSubset", "NanoStringExperiment", 
    function(object) subset(object, subset=CodeClass == "Housekeeping"))

#' Subset to negative control features
#' 
#' Method to subset to features with CodeClass Negative
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' negativeControlSubset(testExp)
#' 
#' @export
setGeneric("negativeControlSubset", signature = "object", 
    function(object) standardGeneric("negativeControlSubset"))

#' Subset to negative control features
#' 
#' Method to subset to features with CodeClass Negative
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' negativeControlSubset(testExp)
#' 
#' @export
setMethod("negativeControlSubset", "NanoStringExperiment", 
    function(object) subset(object, subset=CodeClass == "Negative"))

#' Subset to positive control features
#' 
#' Method to subset to features with CodeClass Positive
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)

#' positiveControlSubset(testExp)
#' 
#' @export
setGeneric("positiveControlSubset", signature = "object", 
    function(object) standardGeneric("positiveControlSubset"))

#' Subset to positive control features
#' 
#' Method to subset to features with CodeClass Positive
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' positiveControlSubset(testExp)
#' 
#' @export
setMethod("positiveControlSubset", "NanoStringExperiment", 
    function(object) subset(object, subset=CodeClass == "Positive"))

#' Subset to control features
#' 
#' Method to subset to features with CodeClass IsControl
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' controlSubset(testExp)
#' 
#' @export
setGeneric("controlSubset", signature = "object", 
    function(object) standardGeneric("controlSubset"))

#' Subset to IsControl features
#' 
#' Method to subset to features with CodeClass IsControl
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' controlSubset(testExp)
#' 
#' @export
setMethod("controlSubset", "NanoStringExperiment", 
    function(object) subset(object, subset=CodeClass == "IsControl"))

#' Subset to non-IsControl features
#' 
#' Method to subset to features not CodeClass IsControl
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' nonControlSubset(testExp)
#' 
#' @export
setGeneric("nonControlSubset", signature = "object", 
    function(object) standardGeneric("nonControlSubset"))

#' Subset to non-IsControl features
#' 
#' Method to subset to features not CodeClass IsControl
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' nonControlSubset(testExp)
#' 
#' @export
setMethod("nonControlSubset", "NanoStringExperiment", 
    function(object) subset(object, subset=CodeClass != "IsControl"))

#' Subset to signature features
#' 
#' Method to subset to features in signatures slot
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' signatureSubset(testExp)
#' 
#' @export
setGeneric("signatureSubset", signature = "object", 
    function(object) standardGeneric("signatureSubset"))

#' Subset to signature features
#' 
#' Method to subset to features in signatures slot
#' 
#' @importFrom NanoStringNCTools weights
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' signatureSubset(testExp)
#' 
#' @export
setMethod("signatureSubset", "NanoStringExperiment", 
    function(object) {
        genes <- unique(names(unlist(unname(weights(signatures(object))))))
        subset(object, subset=CodeClass %in% genes)
    })
