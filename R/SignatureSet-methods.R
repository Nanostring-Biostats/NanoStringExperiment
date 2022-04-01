#' Access weights
#' 
#' Access weights in weights slot
#' 
#' @importMethodsFrom BiocGenerics weights
#' 
#' @return NumericList of weights
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setMethod("weights", "SignatureSet", 
    function(object, ...) object@weights)

#' Replace weights
#' 
#' Replace or assign weights in weights slot
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setGeneric("weights<-", signature = c("object", "value"), 
    function(object, value) standardGeneric("weights<-"))

#' Replace weights
#' 
#' Replace or assign weights in weights slot
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setReplaceMethod("weights", c("SignatureSet", "NumericList"), 
    function(object, value) {
        object@weights <- value
        return(object)
    })

#' Replace weights
#' 
#' Replace or assign weights in weights slot
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setReplaceMethod("weights", c("SignatureSet", "CompressedNumericList"), 
    function(object, 
        value) {
        object@weights <- as(value, "NumericList")
        return(object)
    })

#' Replace weights
#' 
#' Replace or assign weights in weights slot
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setReplaceMethod("weights", c("SignatureSet", "ANY"), 
    function(object, value) {
       object@weights <- as(value, "NumericList")
       return(object)
    })

#' Replace weights
#' 
#' Replace or assign weights in weights slot
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setReplaceMethod("weights", c("SignatureSet", "list"), 
    function(object, value) {
        value <- lapply(value, function(x) {
            if (is.matrix(x) && ncol(x) == 1L) {
                structure(x[, 1L, drop = TRUE], names = rownames(x))
            } else {
                x
            }
        })
        object@weights <- as(value, "NumericList")
        return(object)
    })

#' Replace weights
#' 
#' Replace or assign weights in weights slot
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setReplaceMethod("weights", c("SignatureSet", "NULL"), 
    function(object, value) {
        object@weights <- NumericList()
        return(object)
    })

#' Access groups slot
#' 
#' Access groups factor vector for SignatureSet object
#' 
#' @return factor vector
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setGeneric("groups", signature = c("object"), 
    function(object, ...) standardGeneric("groups"))

#' Access groups slot
#' 
#' Access groups factor vector for SignatureSet object
#' 
#' @return factor vector
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setMethod("groups", "SignatureSet", 
    function(object, ...) object@groups)

#' Replace groups slot
#' 
#' Replace or assign factor vector in groups slot
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setGeneric("groups<-", signature = c("object", "value"), 
    function(object, value) standardGeneric("groups<-"))

#' Replace groups slot
#' 
#' Replace or assign factor vector in groups slot
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setReplaceMethod("groups", c("SignatureSet", "factor"), 
    function(object, value) {
        object@groups <- ifelse(inherits(value, "factor"), 
            as(value, "factor"), factor(value))
        return(object)
    })

#' Replace groups slot
#' 
#' Replace or assign factor vector in groups slot
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setReplaceMethod("groups", c("SignatureSet", "ANY"), 
    function(object, value) {
        object@groups <- ifelse(inherits(value, "factor"), 
            as(value, "factor"), factor(value))
        return(object)
    })

#' Replace groups slot
#' 
#' Replace or assign factor vector in groups slot
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setReplaceMethod("groups", c("SignatureSet", "NULL"), 
    function(object, value) {
        object@groups <- factor()
        return(object)
    })

#' Access func slot
#' 
#' Access signature functions for weights
#' 
#' @return character vector of weight functions
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setGeneric("getSigFuncs", signature = c("object"), 
    function(object, ...) standardGeneric("getSigFuncs"))

#' Access func slot
#' 
#' Access signature functions for weights
#' 
#' @return character vector of weight functions
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setMethod("getSigFuncs", "SignatureSet", 
    function(object, ...) object@func)

#' Replace func slot
#' 
#' Replace signature function for SignatureSet object
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setGeneric("setSigFuncs<-", signature = c("object", "value"), 
    function(object, value) standardGeneric("setSigFuncs<-"))

#' Replace func slot
#' 
#' Replace signature function for SignatureSet object
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setReplaceMethod("setSigFuncs", c("SignatureSet", "character"), 
    function(object, value) {
        object@func <- value
        return(object)
    })

#' Access version slot
#' 
#' Access signature versions for SignatureSet object
#' 
#' @return character string indicating signature versions used
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setGeneric("version", signature = c("object"), 
    function(object, ...) standardGeneric("version"))

#' Access version slot
#' 
#' Access signature versions for SignatureSet object
#' 
#' @return character string indicating signature versions used
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setMethod("version", "SignatureSet", function(object, ...) object@version)

#' Replace version slot
#' 
#' Replace signature versions for SignatureSet object
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setGeneric("version<-", signature = c("object", "value"), 
    function(object, value) standardGeneric("version<-"))

#' Replace version slot
#' 
#' Replace signature versions for SignatureSet object
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setReplaceMethod("version", c("SignatureSet", "character"), 
    function(object, value) {
        object@version <- value
        return(object)
    })

#' Replace version slot
#' 
#' Replace signature versions for SignatureSet object
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setReplaceMethod("version", c("SignatureSet", "ANY"), 
    function(object, value) {
        object@version <- as(value, "character")
        return(object)
    })

#' Replace version slot
#' 
#' Replace signature versions for SignatureSet object
#' 
#' @return SignatureSet object
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setReplaceMethod("version", c("SignatureSet", "NULL"), 
    function(object, value) {
        object@version <- character()
        return(object)
    })

#' Get legnth of signatures
#' 
#' Get the length of weights NumericList corresponding
#' to the length of signatures
#' 
#' @return numeric length
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setMethod("length", "SignatureSet", function(x) {
    length(weights(x))
})

#' Get signature names
#' 
#' Get names of the weights NumericList corresponding
#' to the names of signatures
#' 
#' @return character vector
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setMethod("names", "SignatureSet", function(x) {
    names(weights(x))
})

#' Get number of features within each signature
#' 
#' Get number of features within each weight element in the 
#' weights NumericList corresponding to the number of 
#' features in each signature.
#' 
#' @return character vector
#' 
#' @rdname SignatureSet-methods
#' 
#' @export
setMethod("lengths", "SignatureSet", 
    function(x, use.names = TRUE) {
        vapply(weights(x), function(x) sum(names(x) != "(Intercept)"), 
            FUN.VALUE=numeric(1), USE.NAMES = use.names)
    })