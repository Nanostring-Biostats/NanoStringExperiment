#' Object class version accessor
#' 
#' Access version of NanoStringExperiment package used to generate object
#' 
#' @importMethodsFrom Biobase classVersion
#' 
#' @param object NanoStringExperiment object
#' 
#' @return version of class used to generate object
#' 
#' @examples
#' data(exampleNSEData)
#' classVersion(testExp)
#' 
#' @rdname classVersion
#' 
#' @export
setMethod("classVersion", signature = "NanoStringExperiment",
    function(object) object@.__classVersion__)

#' Access variables used for feature and sample identifiers
#' 
#' Shows variables used for rownames in rowData and colData.
#' 
#' @importMethodsFrom Biobase dimLabels
#' 
#' @param object NanoStringExperiment object
#' 
#' @return list of variables used for dimension labels
#' 
#' @examples
#' data(exampleNSEData)
#' dimLabels(testExp)
#' 
#' @rdname dimLabels
#' 
#' @export
setMethod("dimLabels", signature = "NanoStringExperiment",
    function(object) object@dimLabels)

#' Replace variables used for feature and sample identifiers
#' 
#' Replaces rownames in rowData and colData with specified variable columns.
#' 
#' @importMethodsFrom Biobase dimLabels<-
#' 
#' @param object NanoStringExperiment object
#' @param value list of key headers to use for sample and feature names
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' colData(testExp)[["testNames"]] <- paste0(dimnames(testExp)[[2]], "a")
#' dimLabels(testExp) <- c(dimLabels(testExp)[[1]], "testNames")
#' 
#' @rdname dimLabels
#' 
#' @export
setReplaceMethod("dimLabels", signature = "NanoStringExperiment",
    function(object, value) {
        object@dimLabels <- value
        dimnames(object)[[1L]] <- rowData(object)[, dimLabels(object)[[1L]]]
        dimnames(object)[[2L]] <- colData(object)[, dimLabels(object)[[2L]]]
        return(object)
    })

#' Access feature identifiers
#' 
#' Shows identifiers used for features (rownames) in assay
#' 
#' @importMethodsFrom Biobase featureNames
#' 
#' @param object NanoStringExperiment object
#' 
#' @return list of string feature (row) identifiers
#' 
#' @examples
#' data(exampleNSEData)
#' featureNames(testExp)
#' 
#' @rdname featureNames
#' 
#' @export
setMethod("featureNames", signature = "NanoStringExperiment",
    function(object) dimnames(object)[[1L]])

#' Replace feature identifiers
#' 
#' Replace variables used for rownames in rowData.
#' 
#' @importMethodsFrom Biobase featureNames<-
#' 
#' @param object NanoStringExperiment object
#' @param value list feature names
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' rowData(testExp)[["testNames"]] <- paste0(dimnames(testExp)[[1]], "a")
#' featureNames(testExp) <- rowData(testExp)[["testNames"]]
#' 
#' @rdname featureNames
#' 
#' @export
setReplaceMethod("featureNames", signature = "NanoStringExperiment",
    function(object, value) {
        if (length(dimnames(object)[[1L]]) == length(value)) {
            dimnames(object)[[1L]] <- value
        } else {
            stop("number of items to replace does ",
                "not equal replacement length")
        }
        return(object)
    })

#' Access sample identifiers
#' 
#' Shows variables used for rownames in colData.
#' 
#' @importMethodsFrom Biobase sampleNames
#' 
#' @param object NanoStringExperiment object
#' 
#' @return list of string sample (column) identifiers
#' 
#' @examples
#' data(exampleNSEData)
#' sampleNames(testExp)
#' 
#' @rdname sampleNames
#' 
#' @export
setMethod("sampleNames", signature = "NanoStringExperiment",
    function(object) dimnames(object)[[2L]])

#' Replace sample identifiers
#' 
#' Replace variables used for rownames in colData.
#' 
#' @importMethodsFrom Biobase sampleNames<-
#' 
#' @param object NanoStringExperiment object
#' @param value list sample names
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' colData(testExp)[["testNames"]] <- paste0(dimnames(testExp)[[2]], "a")
#' sampleNames(testExp) <- colData(testExp)[["testNames"]]
#' 
#' @rdname sampleNames
#' 
#' @export
setReplaceMethod("sampleNames", signature = "NanoStringExperiment",
    function(object, value) {
        if (length(dimnames(object)[[2L]]) == length(value)) {
            dimnames(object)[[2L]] <- value
        } else {
            stop("number of items to replace does ",
                "not equal replacement length")
        }
        return(object)
    })

# Assay Data ------------------------------------------------------------------

#' Access assays
#' 
#' It is recommended to use the SummarizedExperiment assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase assayData
#' 
#' @param object NanoStringExperiment object
#' 
#' @return summary of assays in object
#' 
#' @examples
#' data(exampleNSEData)
#' assayData(testExp)
#' 
#' @rdname assayData
#' 
#' @export
setMethod("assayData", signature = "NanoStringExperiment",
    function(object) assays(object))

#' Access assay data
#' 
#' It is recommended to use the SummarizedExperiment assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @param object NanoStringExperiment object
#' @param elt name of assay
#' 
#' @return expression matrix for assay element
#' 
#' @examples
#' data(exampleNSEData)
#' assayDataElement(testExp, "exprs")
#' 
#' @rdname assayData
#' 
#' @export
setGeneric("assayDataElement", signature = "object",
    function(object, elt) standardGeneric("assayDataElement"))

#' Access assay data
#' 
#' It is recommended to use the SummarizedExperiment assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @param object NanoStringExperiment object
#' @param elt name of assay
#' 
#' @return expression matrix for assay element
#' 
#' @examples
#' data(exampleNSEData)
#' assayDataElement(testExp, "exprs")
#' 
#' @rdname assayData
#' 
#' @export
setMethod("assayDataElement", signature = "NanoStringExperiment",
    function(object, elt) {
        if (elt %in% assayNames(object)) {
            assays(object)[[elt]]
        } else {
            stop("'elt' not present in assayData(object)")
        }
    })

#' Replace or add new assay data
#' 
#' It is recommended to use the SummarizedExperiment assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @param object NanoStringExperiment object
#' @param elt name of assay
#' @param ... additional parameters to pass to assayDataElement
#' @param value expression matrix
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' assayDataElement(testExp, "exprsShift") <- 
#'     assayDataElement(testExp, "exprs") + 1
#' 
#' @rdname assayData
#' 
#' @export
setGeneric("assayDataElement<-", 
    signature = c("object", "value"),
    function(object, elt, ..., value) standardGeneric("assayDataElement<-"))

#' Replace or add new assay data
#' 
#' It is recommended to use the SummarizedExperiment assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @param object NanoStringExperiment object
#' @param elt name of assay
#' @param ... additional parameters to pass to assayDataElement
#' @param value expression matrix
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' assayDataElement(testExp, "exprsShift") <- 
#'     assayDataElement(testExp, "exprs") + 1
#' 
#' @rdname assayData
#' 
#' @export
setReplaceMethod("assayDataElement", 
    signature = c("NanoStringExperiment", "ANY"),
    function(object, elt, ..., value) {
        assays(object)[[elt]] <- value
        return(object)
    })

#' Access assay names
#' 
#' It is recommended to use the SummarizedExperiment assayNames instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @param object NanoStringExperiment object
#' @param elt name of assay
#' @param value expression matrix
#' 
#' @return list of assay elements in object
#' 
#' @examples
#' data(exampleNSEData)
#' assayDataElementNames(testExp)
#' 
#' @rdname assayData
#' 
#' @export
setGeneric("assayDataElementNames", signature = "object",
    function(object) standardGeneric("assayDataElementNames"))

#' Access assay names
#' 
#' It is recommended to use the SummarizedExperiment assayNames instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @param object NanoStringExperiment object
#' 
#' @return list of assay elements in object
#' 
#' @examples
#' data(exampleNSEData)
#' assayDataElementNames(testExp)
#' 
#' @rdname assayData
#' 
#' @export
setMethod("assayDataElementNames", signature = "NanoStringExperiment",
    function(object) assayNames(object))

#' Access exprs data
#' 
#' It is recommended to use the SummarizedExperiment assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase exprs
#' 
#' @param object NanoStringExperiment object
#' 
#' @return expression matrix for assay element \code{"exprs"}
#' 
#' @examples
#' data(exampleNSEData)
#' exprs(testExp)
#' 
#' @rdname assayData
#' 
#' @export
setMethod("exprs", signature = "NanoStringExperiment",
    function(object) assays(object)[["exprs"]])

#' Replace exprs data
#' 
#' It is recommended to use the SummarizedExperiment assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase exprs<-
#' 
#' @param object NanoStringExperiment object
#' 
#' @return expression matrix for assay element \code{"exprs"}
#' 
#' @examples
#' data(exampleNSEData)
#' exprs(testExp) <- exprs(testExp) + 1
#' 
#' @rdname assayData
#' 
#' @export
setReplaceMethod("exprs", signature = "NanoStringExperiment",
    function(object, value) {
        assays(object)[["exprs"]] <- value
        return(object)
    })

# Column Metadata (Formerly Sample Data) --------------------------------------
#' Access sample metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @param object NanoStringExperiment object
#' 
#' @return matrix of sample (column) metadata
#' 
#' @examples
#' data(exampleNSEData)
#' sData(testExp)
#' 
#' @rdname sampleMetadata
#' 
#' @export
setGeneric("sData", signature = "object",
    function(object) standardGeneric("sData"))

#' Access sample metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @param object NanoStringExperiment object
#' 
#' @return matrix of sample (column) metadata
#' 
#' @examples
#' data(exampleNSEData)
#' sData(testExp)
#' 
#' @rdname sampleMetadata
#' 
#' @export
setMethod("sData", signature = "NanoStringExperiment",
    function(object) colData(object))

#' Access sample metadata varible names
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @param object NanoStringExperiment object
#' 
#' @return list of sample (column) metadata variables
#' 
#' @examples
#' data(exampleNSEData)
#' svarLabels(testExp)
#' 
#' @rdname sampleMetadata
#' 
#' @export
setGeneric("svarLabels", signature = "object",
    function(object) standardGeneric("svarLabels"))

#' Access sample metadata varible names
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @param object NanoStringExperiment object
#' 
#' @return list of sample (column) metadata variables
#' 
#' @examples
#' data(exampleNSEData)
#' svarLabels(testExp)
#' 
#' @rdname sampleMetadata
#' 
#' @export
setMethod("svarLabels", signature = "NanoStringExperiment",
    function(object) colnames(colData(object)))

#' Access sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase phenoData
#' 
#' @param object NanoStringExperiment object
#' 
#' @return matrix of sample phenotypic metadata
#' 
#' @examples
#' data(exampleNSEData)
#' phenoData(testExp)
#' 
#' @rdname sampleMetadata
#' 
#' @export
setMethod("phenoData", signature = "NanoStringExperiment",
    function(object) {
        ifelse(!"phenotypeCols" %in% names(metadata(object)),
            return(colData(object)),
            return(colData(object)[, metadata(object)[["phenotypeCols"]]]))
    })

#' Replace sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase phenoData<-
#' 
#' @param object NanoStringExperiment object
#' @param value phenotype metadata DataFrame
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' phenoData(testExp) <- phenoData(testExp)[, 1:2]
#' 
#' @rdname sampleMetadata
#' 
#' @export
setReplaceMethod("phenoData", signature = "NanoStringExperiment",
    function(object, value) {
        if(!"phenotypeCols" %in% names(metadata(object))) {
            colData(object) <- value
        } else {
            metadata(object)[["protocolCols"]] <- 
                metadata(object)[["protocolCols"]][
                    which(!metadata(object)[["protocolCols"]] %in% 
                        colnames(value))]
            colData(object) <- cbind(value, 
                colData(object)[, metadata(object)[["protocolCols"]]])
            metadata(object)[["phenotypeCols"]] <- colnames(value)
        }
        return(object)
    })


#' Access sample protocol metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase protocolData
#' 
#' @param object NanoStringExperiment object
#' 
#' @return matrix of sample protocol metadata
#' 
#' @examples
#' data(exampleNSEData)
#' protocolData(testExp)
#' 
#' @rdname sampleMetadata
#' 
#' @export
setMethod("protocolData", signature = "NanoStringExperiment",
    function(object) {
        ifelse(!"protocolCols" %in% names(metadata(object)),
            return(colData(object)),
            return(colData(object)[, metadata(object)[["protocolCols"]]]))
    })

#' Replace sample protocol metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase protocolData<-
#' 
#' @param object NanoStringExperiment object
#' @param value protocol metadata DataFrame
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' protocolData(testExp) <- protocolData(testExp)[, 1:2]
#' 
#' @rdname sampleMetadata
#' 
#' @export
setReplaceMethod("protocolData", signature = "NanoStringExperiment",
    function(object, value) {
        if(!"protocolCols" %in% names(metadata(object))) {
            colData(object) <- value
        } else {
            metadata(object)[["phenotypeCols"]] <- 
                metadata(object)[["phenotypeCols"]][
                    which(!metadata(object)[["phenotypeCols"]] %in% 
                        colnames(value))]
            colData(object) <- 
                cbind(colData(object)[, metadata(object)[["phenotypeCols"]]], 
                    value)
            metadata(object)[["protocolCols"]] <- colnames(value)
        }
        return(object)
    })

#' Access sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase pData
#' 
#' @param object NanoStringExperiment object
#' 
#' @return matrix of sample phenotypic metadata
#' 
#' @examples
#' data(exampleNSEData)
#' pData(testExp)
#' 
#' @rdname sampleMetadata
#' 
#' @export
setMethod("pData", signature = "NanoStringExperiment",
    function(object) {
        phenoData(object)
    })

#' Access sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase pData
#' 
#' @param object metadata DataFrame
#' 
#' @return DataFrame of sample phenotypic metadata
#' 
#' @examples
#' data(exampleNSEData)
#' pData(testExp)
#' 
#' @rdname sampleMetadata
#' 
#' @export
setMethod("pData", signature = "DataFrame",
    function(object) object)

#' Replace sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase pData<-
#' 
#' @param object NanoStringExperiment object
#' @param value phenotype metadata DataFrame
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' pData(testExp) <- pData(testExp)[, 1:2]
#' 
#' @rdname sampleMetadata
#' 
#' @export
setReplaceMethod("pData", signature = "NanoStringExperiment",
    function(object, value) {
        phenoData(object) <- value
    })

#' Access sample phenotypic metadata variables
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase varLabels
#' 
#' @param object NanoStringExperiment object
#' 
#' @return list of sample phenotypic metadata variables
#' 
#' @examples
#' data(exampleNSEData)
#' varLabels(testExp)
#' 
#' @rdname sampleMetadata
#' 
#' @export
setMethod("varLabels", signature = "NanoStringExperiment",
    function(object) {
        ifelse(is.null(metadata(object)[["phenotypeCols"]]), 
            return(colnames(colData(object))),
            return(metadata(object)[["phenotypeCols"]]))
    })

#' Access metadata variables
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase varLabels
#' 
#' @param object metadata DataFrame
#' 
#' @return list of metadata variables
#' 
#' @examples
#' data(exampleNSEData)
#' varLabels(phenoData(testExp))
#' 
#' @rdname sampleMetadata
#' 
#' @export
setMethod("varLabels", signature = "DataFrame",
    function(object) colnames(object))

#' Replace sample phenotypic metadata variables
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase varLabels<-
#' 
#' @param object NanoStringExperiment object
#' @param value replacement variable labels
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' varLabels(testExp) <- paste0(varLabels(testExp), "a")
#' 
#' @rdname sampleMetadata
#' 
#' @export
setReplaceMethod("varLabels", signature = "NanoStringExperiment",
    function(object, value) {
        if(is.null(metadata(object)[["phenotypeCols"]])) {
            phenotypes <- colData(object)
        } else {
            phenotypes <- 
                colData(object)[, metadata(object)[["phenotypeCols"]]]
        }
        if (dim(phenotypes)[2L] == length(value)) {
            colnames(phenotypes) <- value
        } else {
            stop("number of items to replace does ",
                "not equal replacement length")
        }
        phenoData(object) <- phenotypes
        return(object)
    })

# Row Metadata (Formerly Feature Data) ----------------------------------------

#' Access feature metadata
#' 
#' It is recommended to use the SummarizedExperiment rowData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase fData
#' 
#' @param object NanoStringExperiment object
#' 
#' @return matrix of feature (column) metadata
#' 
#' @examples
#' data(exampleNSEData)
#' fData(testExp)
#' 
#' @rdname featureMetadata
#' 
#' @export
setMethod("fData", signature = "NanoStringExperiment",
    function(object) rowData(object))

#' Replace feature metadata
#' 
#' It is recommended to use the SummarizedExperiment rowData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase fData<-
#' 
#' @param object NanoStringExperiment object
#' @param value feature metadata
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' fData(testExp) <- rowData(testExp)[, 1:2]
#' 
#' @rdname featureMetadata
#' 
#' @export
setReplaceMethod("fData", signature = "NanoStringExperiment",
    function(object, value) {
        rowData(object) <- value
        return(object)
    })

#' Access feature metadata variables
#' 
#' It is recommended to use the SummarizedExperiment rowData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase fvarLabels
#' 
#' @param object NanoStringExperiment object
#' 
#' @return list of feature metadata variables
#' 
#' @examples
#' data(exampleNSEData)
#' fvarLabels(testExp)
#' 
#' @rdname featureMetadata
#' 
#' @export
setMethod("fvarLabels", signature = "NanoStringExperiment",
    function(object) colnames(rowData(object)))

#' Replace feature metadata variables
#' 
#' It is recommended to use the SummarizedExperiment rowData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase fvarLabels<-
#' 
#' @param object NanoStringExperiment object
#' @param value replacement feature metadata variables
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' fvarLabels(testExp) <- paste0(fvarLabels(testExp), "a")
#' 
#' @rdname featureMetadata
#' 
#' @export
setReplaceMethod("fvarLabels", signature = "NanoStringExperiment",
    function(object, value) {
        colnames(rowData(object)) <- value
        return(object)
    })

#' Access feature metadata
#' 
#' It is recommended to use the SummarizedExperiment rowData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase featureData
#' 
#' @param object NanoStringExperiment object
#' 
#' @return matrix of feature (row) metadata
#' 
#' @examples
#' data(exampleNSEData)
#' featureData(testExp)
#' 
#' @rdname featureMetadata
#' 
#' @export
setMethod("featureData", signature = "NanoStringExperiment",
    function(object) rowData(object))

#' Replace feature metadata
#' 
#' It is recommended to use the SummarizedExperiment rowData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase featureData<-
#' 
#' @param object NanoStringExperiment object
#' @param value replacement feature metadata
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' featureData(testExp) <- featureData(testExp)[, 1:2]
#' 
#' @rdname featureMetadata
#' 
#' @export
setReplaceMethod("featureData", signature = "NanoStringExperiment",
    function(object, value) {
        rowData(object) <- value
        return(object)
    })

# Experiment Data ---------------------------------------------------------
#' Access experiment metadata
#' 
#' It is recommended to use the SummarizedExperiment metadata instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase experimentData
#' 
#' @param object NanoStringExperiment object
#' 
#' @return list of experiment metadata
#' 
#' @examples
#' data(exampleNSEData)
#' experimentData(testExp)
#' 
#' @rdname experimentMetadata
#' 
#' @export
setMethod("experimentData", signature = "NanoStringExperiment",
    function(object) metadata(object))

#' Replace experiment metadata
#' 
#' It is recommended to use the SummarizedExperiment metadata instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase experimentData<-
#' 
#' @param object NanoStringExperiment object
#' @param value list of experiment parameters
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' experimentData(testExp) <- 
#'     c(experimentData(testExp), list(testName="myExp"))
#' 
#' @rdname experimentMetadata
#' 
#' @export
setReplaceMethod("experimentData", signature = "NanoStringExperiment",
    function(object, value) {
        metadata(object) <- value
        return(object)
    })

#' Access experiment metadata
#' 
#' It is recommended to use the SummarizedExperiment metadata instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase expinfo
#' 
#' @param object list of experiment data
#' 
#' @return list of experiment metadata
#' 
#' @examples
#' data(exampleNSEData)
#' expinfo(experimentData(testExp))
#' 
#' @rdname experimentMetadata
#' 
#' @export
setMethod("expinfo", signature = "list",
    function(object) object)

#' Access experiment metadata
#' 
#' It is recommended to use the SummarizedExperiment metadata instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase otherInfo
#' 
#' @param object list of experiment data
#' 
#' @return list of experiment metadata
#' 
#' @examples
#' data(exampleNSEData)
#' otherInfo(experimentData(testExp))
#' 
#' @rdname experimentMetadata
#' 
#' @export
setMethod("otherInfo", signature = "list",
    function(object) object)

#' Access experiment annotations
#' 
#' Access annotations associated with the experiment
#' 
#' @importMethodsFrom BiocGenerics annotation
#' 
#' @param object NanoStringExperiment object
#' 
#' @return list of annotations used with experiment
#' 
#' @examples
#' data(exampleNSEData)
#' annotation(testExp)
#' 
#' @rdname annotation
#' 
#' @export
setMethod("annotation", signature="NanoStringExperiment",
    function(object) object@annotation)

#' Access design formula
#' 
#' Access formula to be used for design in analyses
#' 
#' @param object NanoStringExperiment object
#' 
#' @return formula used for design (model) matrix
#' 
#' @examples
#' data(exampleNSEData)
#' design(testExp)
#' 
#' @export
setGeneric("design", signature = "object",
    function(object) standardGeneric("design"))

#' Access design formula
#' 
#' Access formula to be used for design in analyses
#' 
#' @param object NanoStringExperiment object
#' 
#' @return formula used for design (model) matrix
#' 
#' @examples
#' data(exampleNSEData)
#' design(testExp)
#' 
#' @rdname design
#' 
#' @export
setMethod("design", "NanoStringExperiment", 
    function(object) object@design)

#' Replace design formula
#' 
#' Replace or assign formula to be used for design in analyses
#' 
#' @param object NanoStringExperiment object
#' @param value formula for design matrix or NULL
#' 
#' @return NanoStringExperiment object
#' 
#' @importMethodsFrom BiocGenerics design<-
#' 
#' @examples
#' data(exampleNSEData)
#' design(testExp) <- stats::as.formula(~ `cell_line`)
#' 
#' @rdname design
#' 
#' @export
setReplaceMethod("design", c("NanoStringExperiment", "formula"),
    function(object, value) {
        object@design <- value
        return(object)
    })

#' Replace design formula
#' 
#' Replace or assign formula to be used for design in analyses
#' 
#' @importFrom stats as.formula
#' 
#' @param object NanoStringExperiment object
#' @param value formula for design matrix or NULL
#' 
#' @return NanoStringExperiment object
#' 
#' @importMethodsFrom BiocGenerics design<-
#' 
#' @examples
#' data(exampleNSEData)
#' design(testExp) <- ~ `cell_line`
#' 
#' @rdname design
#' 
#' @export
setReplaceMethod("design", c("NanoStringExperiment", "ANY"),
    function(object, value) {
        object@design <- stats::as.formula(value)
        return(object)
    })

#' Replace design formula
#' 
#' Replace or assign formula to be used for design in analyses
#' 
#' @param object NanoStringExperiment object
#' @param value formula for design matrix or NULL
#' 
#' @return NanoStringExperiment object
#' 
#' @importMethodsFrom BiocGenerics design<-
#' 
#' @examples
#' data(exampleNSEData)
#' design(testExp) <- NULL
#' 
#' @rdname design
#' 
#' @export
setReplaceMethod("design", c("NanoStringExperiment", "NULL"),
    function(object, value) {
        object@design <- NULL
        return(object)
    })

#' Access signatures slot
#' 
#' Access signatures slot to get \code{SignatureSet} object
#' 
#' @param object NanoStringExperiment object
#' 
#' @return SignatureSet object
#' 
#' @examples
#' data(exampleNSEData)
#' signatures(testExp)
#' 
#' @export
setGeneric("signatures", signature = "object", 
    function(object) standardGeneric("signatures"))

#' Access signatures slot
#' 
#' Access signatures slot to get \code{SignatureSet} object
#' 
#' @param object NanoStringExperiment object
#' 
#' @return SignatureSet object
#' 
#' @examples
#' data(exampleNSEData)
#' signatures(testExp)
#' 
#' @export
setMethod("signatures", "NanoStringExperiment", 
    function(object) object@signatures)

#' Replace signatures
#' 
#' Replace signatures slot with updated \code{SignatureSet}
#' 
#' @importClassesFrom NanoStringNCTools SignatureSet
#' 
#' @param object NanoStringExperiment object
#' @param value SignatureSet object
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' signatures(testExp) <- NanoStringNCTools::SignatureSet()
#' 
#' @rdname signatures
#' 
#' @export
setGeneric("signatures<-", signature = c("object", "value"), 
    function(object, value) standardGeneric("signatures<-"))

#' Replace signatures
#' 
#' Replace signatures slot with updated \code{SignatureSet}
#' 
#' @importClassesFrom NanoStringNCTools SignatureSet
#' 
#' @param object NanoStringExperiment object
#' @param value SignatureSet object
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' signatures(testExp) <- NanoStringNCTools::SignatureSet()
#' 
#' @rdname signatures
#' 
#' @export
setReplaceMethod("signatures", c("NanoStringExperiment", "SignatureSet"), 
    function(object, value) {
        object@signatures <- value
        return(object)
    })

