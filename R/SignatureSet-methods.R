#' @export
setGeneric("signatures", 
    signature = "object", 
    function(object) standardGeneric("signatures"))
setMethod("signatures", 
    "NanoStringExperiment", 
    function(object) object@signatures)

#' @export
setGeneric("signatures<-", 
    signature = c("object", "value"), 
    function(object, value) standardGeneric("signatures<-"))
setReplaceMethod("signatures", c("NanoStringExperiment", "SignatureSet"), function(object, 
    value) {
    object@signatures <- value
    object
})