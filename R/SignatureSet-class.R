#' @export
.SignatureSet <- setClass("SignatureSet", 
    contains = "VersionedBiobase", 
    slots = c(weights = "NumericList", 
        groups = "factor", 
        func = "character", 
        version = "character"), 
    prototype = prototype(
        new("VersionedBiobase", 
            versions = c(SignatureSet = "1.0.0")), 
        weights = NumericList(), 
        groups = factor(), 
        func = character(), 
        version = character()))

#' @export
setMethod("initialize", "SignatureSet", function(.Object, weights = NumericList(), ...) {
    callNextMethod(.Object, weights = as(weights, "NumericList"), groups = factor(names(weights)), 
        func = rep("default", length(weights)), version = "0.0.1", ...)
})
#' @export
setMethod("initialize", "SignatureSet", function(.Object, weights = NumericList(), groups = factor(), 
    ...) {
    callNextMethod(.Object, weights = as(weights, "NumericList"), groups = factor(groups), 
        func = rep("default", length(weights)), version = "0.0.1", ...)
})
#' @export
setMethod("initialize", "SignatureSet", function(.Object, weights = NumericList(), groups = factor(), 
    func = character(), ...) {
    callNextMethod(.Object, weights = as(weights, "NumericList"), groups = factor(groups), 
        func = as(func, "character"), version = "0.0.1", ...)
})
#' @export
setMethod("initialize", "SignatureSet", function(.Object, weights = NumericList(), groups = factor(), 
    func = character(), version = character(), ...) {
    callNextMethod(.Object, weights = as(weights, "NumericList"), groups = factor(groups), 
        func = as(func, "character"), version = as(version, "character"), ...)
})
#' @export
setMethod("show", signature = "SignatureSet", function(object) {
    callNextMethod(object)
    cat("weights: ")
    if (length(weights(object)) == 0L) 
        cat("none\n")
    else cat("use 'weights(object)'")
})
#' @export
SignatureSet <- function(weights = NumericList(), ...) {
    new2("SignatureSet", weights = weights, groups = rep("Group", length(weights)), func = rep("default", 
        length(weights)), ...)
}
#' @export
SignatureSet <- function(weights = NumericList(), groups = factor(), ...) {
    new2("SignatureSet", weights = weights, groups = groups, func = rep("default", length(groups)), 
        ...)
}
#' @export
SignatureSet <- function(weights = NumericList(), groups = factor(), func = character(), 
    ...) {
    new2("SignatureSet", weights = weights, groups = groups, func = func, ...)
}
#' @export
SignatureSet <- function(weights = NumericList(), groups = factor(), func = character(), 
    version = character(), ...) {
    new2("SignatureSet", weights = weights, groups = groups, func = func, version = version, 
        ...)
}
#' @export
setMethod("weights", "SignatureSet", function(object, ...) object@weights)
#' @export
setGeneric("weights<-", signature = c("object", "value"), function(object, value) standardGeneric("weights<-"))
#' @export
setReplaceMethod("weights", c("SignatureSet", "NumericList"), function(object, value) {
    object@weights <- value
    object
})
#' @export
setReplaceMethod("weights", c("SignatureSet", "CompressedNumericList"), function(object, 
    value) {
    object@weights <- as(value, "NumericList")
    object
})
#' @export
setReplaceMethod("weights", c("SignatureSet", "ANY"), function(object, value) {
    object@weights <- as(value, "NumericList")
    object
})
#' @export
setReplaceMethod("weights", c("SignatureSet", "list"), function(object, value) {
    value <- lapply(value, function(x) {
        if (is.matrix(x) && ncol(x) == 1L) 
            structure(x[, 1L, drop = TRUE], names = rownames(x))
        else x
    })
    object@weights <- as(value, "NumericList")
    object
})
#' @export
setReplaceMethod("weights", c("SignatureSet", "NULL"), function(object, value) {
    object@weights <- NumericList()
    object
})
#' @export
setGeneric("groups", signature = c("object"), function(object, ...) standardGeneric("groups"))
#' @export
setMethod("groups", "SignatureSet", function(object, ...) object@groups)
#' @export
setGeneric("groups<-", signature = c("object", "value"), function(object, value) standardGeneric("groups<-"))
setReplaceMethod("groups", c("SignatureSet", "factor"), function(object, value) {
    object@groups <- ifelse(inherits(value, "factor"), as(value, "factor"), factor(value))
    object
})
#' @export
setReplaceMethod("groups", c("SignatureSet", "ANY"), function(object, value) {
    object@groups <- ifelse(inherits(value, "factor"), as(value, "factor"), factor(value))
    object
})
#' @export
setReplaceMethod("groups", c("SignatureSet", "NULL"), function(object, value) {
    object@groups <- factor()
    object
})
#' @export
setGeneric("version", signature = c("object"), function(object, ...) standardGeneric("version"))
#' @export
setMethod("version", "SignatureSet", function(object, ...) object@version)
#' @export
setGeneric("version<-", signature = c("object", "value"), function(object, value) standardGeneric("version<-"))
#' @export
setReplaceMethod("version", c("SignatureSet", "character"), function(object, value) {
    object@version <- value
    object
})
#' @export
setReplaceMethod("version", c("SignatureSet", "ANY"), function(object, value) {
    object@version <- as(value, "character")
    object
})
#' @export
setReplaceMethod("version", c("SignatureSet", "NULL"), function(object, value) {
    object@version <- character()
    object
})
#' @export
setGeneric("setSigFuncs<-", signature = c("object", "value"), function(object, value) standardGeneric("setSigFuncs<-"))
#' @export
setReplaceMethod("setSigFuncs", c("SignatureSet", "character"), function(object, value) {
    object@func <- value
    return(object)
})
#' @export
setGeneric("getSigFuncs", signature = c("object"), function(object, ...) standardGeneric("getSigFuncs"))
#' @export
setMethod("getSigFuncs", "SignatureSet", function(object, ...) object@func)
#' @export
setMethod("length", "SignatureSet", function(x) {
    length(weights(x))
})
#' @export
setMethod("names", "SignatureSet", function(x) {
    names(weights(x))
})
#' @export
setMethod("lengths", "SignatureSet", function(x, use.names = TRUE) {
    vapply(weights(x), function(x) sum(names(x) != "(Intercept)"), FUN.VALUE=numeric(1), USE.NAMES = use.names)
})
validSignatureSet <- function(object) {
    errorMessage <- NULL
    if (!inherits(object@weights, "NumericList")) {
        errTmp <- "Weights are not a NumericList"
        errorMessage <- switch(is.null(errorMessage), errTmp, paste(errorMessage, errTmp, 
            sep = "\n"))
    }
    if (!inherits(object@func, "character")) {
        errTmp <- "Functions are not a character"
        errorMessage <- switch(is.null(errorMessage), errTmp, paste(errorMessage, errTmp, 
            sep = "\n"))
    }
    if (length(object@weights) != length(object@groups)) {
        errTmp <- paste("Unequal weights and groups.  Length weights: ", length(object@weights), 
            ", Length groups: ", length(object@groups), sep = "")
        errorMessage <- switch(is.null(errorMessage), errTmp, paste(errorMessage, errTmp, 
            sep = "\n"))
    }
    if (length(object@weights) != length(object@func)) {
        errTmp <- paste("Unequal weights and func  Length weights: ", length(object@weights), 
            ", Length func: ", length(object@func), sep = "")
        errorMessage <- switch(is.null(errorMessage), errTmp, paste(errorMessage, errTmp, 
            sep = "\n"))
    }
    if (length(object@groups) != length(object@func)) {
        errTmp <- paste("Unequal groups and func  Length groups: ", length(object@groups), 
            ", Length func: ", length(object@func), sep = "")
        errorMessage <- switch(is.null(errorMessage), errTmp, paste(errorMessage, errTmp, 
            sep = "\n"))
    }
    if (length(object@weights) != length(union(names(object@weights), names(object@groups)))) {
        errTmp <- paste("Weight names do not match group member names", sep = "\n")
        errorMessage <- switch(is.null(errorMessage), errTmp, paste(errorMessage, errTmp, 
            sep = "\n"))
    }
    if (length(object@weights) != length(union(names(object@weights), names(object@func)))) {
        errTmp <- paste("Weight names do not match func member names", sep = "\n")
        errorMessage <- switch(is.null(errorMessage), errTmp, paste(errorMessage, errTmp, 
            sep = "\n"))
    }
    if (length(object@weights) != length(union(names(object@groups), names(object@func)))) {
        errTmp <- paste("Groups names do not match func member names", sep = "\n")
        errorMessage <- switch(is.null(errorMessage), errTmp, paste(errorMessage, errTmp, 
            sep = "\n"))
    }
    ifelse(is.null(errorMessage), TRUE, errorMessage)
}
setValidity("SignatureSet", validSignatureSet)
