#' The SignatureSet class
#' 
#' Class definition for a SignatureSet. Holds weights and grouping for 
#' expression signature groups.
#' 
#' @importClassesFrom Biobase VersionedBiobase
#' 
#' @slot \code{weights} named \code{NumericList} defining signatures based 
#'     on linear combinations of genes.
#' @slot \code{groups} factor vector indicating groups in \code{SignatureSet}
#' @slot \code{func} character indicating function to use
#' @slot \code{version} character indicating version to use
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-class
#' 
#' @export
.SignatureSet <- setClass("SignatureSet", 
    contains = "VersionedBiobase", 
    slots = c(
        weights = "NumericList", 
        groups = "factor", 
        func = "character", 
        version = "character"), 
    prototype = prototype(
        new("VersionedBiobase", versions = c(SignatureSet = 
            paste(packageVersion("NanoStringExperiment"), collapse="."))), 
        weights = NumericList(), 
        groups = factor(), 
        func = character(), 
        version = character()))

#' Initialize SignatureSet object
#' 
#' Initialize SignatureSet with weights
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-class
#' 
#' @export
setMethod("initialize", "SignatureSet", 
    function(.Object, weights = NumericList(), ...) {
        callNextMethod(.Object, weights = as(weights, "NumericList"), 
            groups = factor(names(weights)), 
            func = rep("default", length(weights)), version = "0.0.1", ...)
    })

#' Initialize SignatureSet object
#' 
#' Initialize SignatureSet with weights and groups
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-class
#' 
#' @export
setMethod("initialize", "SignatureSet", 
    function(.Object, weights = NumericList(), groups = factor(), ...) {
        callNextMethod(.Object, weights = as(weights, "NumericList"), 
            groups = factor(groups), func = rep("default", length(weights)), 
            version = "0.0.1", ...)
    })

#' Initialize SignatureSet object
#' 
#' Initialize SignatureSet with weights, groups, and func
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-class
#' 
#' @export
setMethod("initialize", "SignatureSet", 
    function(.Object, weights = NumericList(), 
        groups = factor(), func = character(), ...) {
            callNextMethod(.Object, weights = as(weights, "NumericList"), 
                groups = factor(groups), func = as(func, "character"), 
                version = "0.0.1", ...)
    })

#' Initialize SignatureSet object
#' 
#' Initialize SignatureSet with weights, groups, func, and version
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-class
#' 
#' @export
setMethod("initialize", "SignatureSet", 
    function(.Object, weights = NumericList(), groups = factor(), 
        func = character(), version = character(), ...) {
            callNextMethod(.Object, weights = as(weights, "NumericList"), 
                groups = factor(groups), func = as(func, "character"), 
                version = as(version, "character"), ...)
    })

#' Show method
#' 
#' Show method for SignatureSet
#' 
#' @return summary of SignatureSet object
#' 
#' @examples
#' testSig <- SignatureSet()
#' show(testSig)
#' 
#' @rdname SignatureSet-class
#' 
#' @export
setMethod("show", signature = "SignatureSet", 
    function(object) {
        callNextMethod(object)
        cat("weights: ")
        if (length(weights(object)) == 0L) 
            cat("none\n")
        else cat("use 'weights(object)'")
    })

#' Generate SignatureSet object
#' 
#' Generate new SignatureSet with weights
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-class
#' 
#' @export
SignatureSet <- function(weights = NumericList(), ...) {
    new2("SignatureSet", weights = weights, 
        groups = rep("Group", length(weights)), 
        func = rep("default", length(weights)), ...)
}

#' Generate SignatureSet object
#' 
#' Generate new SignatureSet with weights and groups
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-class
#' 
#' @export
SignatureSet <- function(weights = NumericList(), groups = factor(), ...) {
    new2("SignatureSet", weights = weights, groups = groups, 
        func = rep("default", length(groups)), ...)
}

#' Generate SignatureSet object
#' 
#' Generate new SignatureSet with weights, groups, and func
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-class
#' 
#' @export
SignatureSet <- function(weights = NumericList(), groups = factor(), 
    func = character(), ...) {
        new2("SignatureSet", weights = weights, groups = groups, 
            func = func, ...)
}

#' Generate SignatureSet object
#' 
#' Generate new SignatureSet with weights, groups, func, and version
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-class
#' 
#' @export
SignatureSet <- function(weights = NumericList(), groups = factor(), func = character(), 
    version = character(), ...) {
        new2("SignatureSet", weights = weights, groups = groups, 
            func = func, version = version, ...)
}

validSignatureSet <- function(object) {
    errorMessage <- NULL
    if (!inherits(object@weights, "NumericList")) {
        errTmp <- "Weights are not a NumericList"
        errorMessage <- switch(is.null(errorMessage), errTmp, 
            paste(errorMessage, errTmp, sep = "\n"))
    }
    if (!inherits(object@func, "character")) {
        errTmp <- "Functions are not a character"
        errorMessage <- switch(is.null(errorMessage), errTmp, 
            paste(errorMessage, errTmp, sep = "\n"))
    }
    if (length(object@weights) != length(object@groups)) {
        errTmp <- paste("Unequal weights and groups.  Length weights: ", 
            length(object@weights), ", Length groups: ", 
            length(object@groups), sep = "")
        errorMessage <- switch(is.null(errorMessage), errTmp, 
            paste(errorMessage, errTmp, sep = "\n"))
    }
    if (length(object@weights) != length(object@func)) {
        errTmp <- paste("Unequal weights and func  Length weights: ", 
            length(object@weights), ", Length func: ", 
            length(object@func), sep = "")
        errorMessage <- switch(is.null(errorMessage), errTmp, 
            paste(errorMessage, errTmp, sep = "\n"))
    }
    if (length(object@groups) != length(object@func)) {
        errTmp <- paste("Unequal groups and func  Length groups: ", 
            length(object@groups), ", Length func: ", 
            length(object@func), sep = "")
        errorMessage <- switch(is.null(errorMessage), errTmp, 
            paste(errorMessage, errTmp, sep = "\n"))
    }
    if (length(object@weights) != 
        length(union(names(object@weights), names(object@groups)))) {
            errTmp <- paste("Weight names do not match group member names", 
                sep = "\n")
            errorMessage <- switch(is.null(errorMessage), errTmp, 
                paste(errorMessage, errTmp, sep = "\n"))
    }
    if (length(object@weights) != 
        length(union(names(object@weights), names(object@func)))) {
            errTmp <- paste("Weight names do not match func member names", 
                sep = "\n")
            errorMessage <- switch(is.null(errorMessage), errTmp, 
                paste(errorMessage, errTmp, sep = "\n"))
    }
    if (length(object@weights) != 
        length(union(names(object@groups), names(object@func)))) {
            errTmp <- paste("Groups names do not match func member names", 
                sep = "\n")
            errorMessage <- switch(is.null(errorMessage), errTmp, 
                paste(errorMessage, errTmp, sep = "\n"))
    }
    ifelse(is.null(errorMessage), TRUE, errorMessage)
}
setValidity("SignatureSet", validSignatureSet)
