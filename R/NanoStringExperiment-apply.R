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

#' Apply across NanoStringExperiment object by groupings
#' 
#' Group samples or features by variable and apply a function
#' 
#' @export
setGeneric("esBy", signature = "X", 
    function(X, GROUP, FUN, ...) standardGeneric("esBy"))

#' Apply across NanoStringExperiment object by groupings
#' 
#' Group samples or features by variable and apply a function
#' 
#' @export
setMethod("esBy", "NanoStringExperiment", 
    function(X, GROUP, FUN, ..., simplify = TRUE) {
        featureNames <- colnames(rowData(X))
        sampleNames <- colnames(colData(X))
        choices <- c(structure(rep.int("featureData", length(featureNames)), 
            names = featureNames), structure(rep.int("sampleData", 
            length(sampleNames)), names = sampleNames))
        dataIn <- choices[match.arg(GROUP, names(choices))][[1L]]
        if (dataIn == "sampleData") {
            values <- colData(X)[, GROUP]
            names(values) <- dimnames(X)[[2L]]
        } else { 
            values <- rowData(X)[, GROUP]
            names(values) <- dimnames(X)[[1L]]
        }
        keys <- sort(unique(values), na.last = TRUE)
        names(keys) <- as.character(keys)
        byResult <- lapply(keys, function(k) {
            if (is.na(k)) {
                keep <- which(is.na(values))
            } else {
                keep <- which(!is.na(values) & values == k)
            }
            if (dataIn == "featureData") {
                FUN(X[keep, ], ...)
            } else {
                FUN(X[, keep], ...)
            }
        })
        if(simplify) {
            return(simplify2array(byResult))
        } else {
            return(byResult)
        }
    })
