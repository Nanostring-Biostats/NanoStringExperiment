#' Apply across an assay
#' 
#' Apply function row (feature) or column (sample)-wise
#' to selected assay
#' 
#' @param X NanoStringExperiment object
#' @param MARGIN integer to apply across row or column
#' @param FUN function to apply
#' @param ... parameters to pass to FUN
#' 
#' @return assay data matrix
#' 
#' @examples
#' data(exampleNSEData)
#' assayDataApply(testExp, 1, mean)
#' 
#' @rdname assayDataApply
#' 
#' @export
setGeneric("assayDataApply", signature = "X", 
    function(X, MARGIN, FUN, ...) standardGeneric("assayDataApply"))

#' Apply across an assay
#' 
#' Apply function row (feature) or column (sample)-wise
#' to selected assay
#' 
#' @param X NanoStringExperiment object
#' @param MARGIN integer to apply across row or column
#' @param FUN function to apply
#' @param ... parameters to pass to FUN
#' @param elt expression matrix for assay element \code{"exprs"}
#' 
#' @return assay data matrix
#' 
#' @examples
#' data(exampleNSEData)
#' assayDataApply(testExp, 1, mean, elt="exprs")
#' 
#' @rdname assayDataApply
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
#' @param X NanoStringExperiment object
#' @param MARGIN integer to apply across row or column
#' @param FUN function to apply
#' @param ... parameters to pass to FUN
#' 
#' @return assay data matrix
#' 
#' @examples
#' data(exampleNSEData)
#' esApply(testExp, 1, mean)
#' 
#' @rdname esApply
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
#' @param X NanoStringExperiment object
#' @param GROUP colData header to group by
#' @param FUN function to apply
#' @param ... parameters to pass to FUN
#' 
#' @return list or matrix of results
#' 
#' @examples
#' data(exampleNSEData)
#' esBy(testExp, 
#'     GROUP = "cell_line", 
#'     FUN = function(x) { 
#'         assayDataApply(x, MARGIN = 1, FUN = mean, elt = "exprs") 
#'     })
#' 
#' @rdname esBy
#' 
#' @export
setGeneric("esBy", signature = "X", 
    function(X, GROUP, FUN, ...) standardGeneric("esBy"))

#' Apply across NanoStringExperiment object by groupings
#' 
#' Group samples or features by variable and apply a function
#' 
#' @param X NanoStringExperiment object
#' @param GROUP colData header to group by
#' @param FUN function to apply
#' @param ... parameters to pass to FUN
#' @param simplify boolean indicating whether to simplify output
#' 
#' @return list or matrix of results
#' 
#' @examples
#' data(exampleNSEData)
#' esBy(testExp, 
#'     GROUP = "cell_line", 
#'     FUN = function(x) { 
#'         assayDataApply(x, MARGIN = 1, FUN = mean, elt = "exprs") 
#'     })
#' 
#' @rdname esBy
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
