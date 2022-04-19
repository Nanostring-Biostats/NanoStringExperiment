#' Object class version accessor
#' 
#' Access version of NanoStringExperiment package used to generate object
#' 
#' @return version of class used to generate object
#' 
#' @importMethodsFrom Biobase classVersion
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return list of variables used for dimension labels
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return list of string feature (row) identifiers
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' rowData(testExp)[["testNames"]] <- paste0(dimnames(testExp)[[1]], "a")
#' featureNames(testExp) <- rowData(testExp)[["testNames"]]
#' 
#' @rdname featureNames
#' 
#' @export
setReplaceMethod("featureNames", signature = "NanoStringExperiment",
    function(object, value) {
        print(dim(object))
        dimnames(object)[[1L]] <- value
        return(object)
    })

#' Access sample identifiers
#' 
#' Shows variables used for rownames in colData.
#' 
#' @importMethodsFrom Biobase sampleNames
#' 
#' @return list of string sample (column) identifiers
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' colData(testExp)[["testNames"]] <- paste0(dimnames(testExp)[[2]], "a")
#' sampleNames(testExp) <- colData(testExp)[["testNames"]]
#' 
#' @rdname sampleNames
#' 
#' @export
setReplaceMethod("sampleNames", signature = "NanoStringExperiment",
    function(object, value) {
        dimnames(object)[[2L]] <- value
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
#' @return summary of assays in object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return expression matrix for assay element
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return expression matrix for assay element
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return list of assay elements in object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return list of assay elements in object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return expression matrix for assay element \code{"exprs"}
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return matrix of sample (column) metadata
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return matrix of sample (column) metadata
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return list of sample (column) metadata variables
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return list of sample (column) metadata variables
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return matrix of sample phenotypic metadata
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' phenoData(testExp)
#' 
#' @rdname sampleMetadata
#' 
#' @export
setMethod("phenoData", signature = "NanoStringExperiment",
    function(object) {
        ifelse(!"phenotypeCols" %in% names(metadata(object)),
            return(colData(object)),
            return(colData(object)[, metadata(object)$phenotypeCols]))
    })

#' Replace sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase phenoData<-
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
            colData(object) <- value
            metadata(object)$phenotypeCols <- colnames(value)
            metadata(object)$protocolCols <- 
                metadata(object)$protocolCols[!metadata(object)$protocolCols 
                    %in% metadata(object)$phenotypeCols]
        }
        return(object)
    })

#' Replace sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase phenoData<-
#' 
#' @return DataFrame of sample phenotypic metadata
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' phenoData(testExp) <- phenoData(testExp)[, 1:2]
#' 
#' @rdname sampleMetadata
#' 
#' @export
setReplaceMethod("phenoData", signature = "DataFrame",
    function(object, value) {
        object <- value
        return(object)
    })

#' Replace sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase phenoData<-
#' 
#' @return DataFrame of sample phenotypic metadata
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' phenoData(testExp) <- phenoData(testExp)[, 1:2]
#' 
#' @rdname sampleMetadata
#' 
#' @export
setReplaceMethod("phenoData", signature = "vector",
    function(object, value) {
        object <- value
        return(object)
    })


#' Access sample protocol metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase protocolData
#' 
#' @return matrix of sample protocol metadata
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' protocolData(testExp)
#' 
#' @rdname sampleMetadata
#' 
#' @export
setMethod("protocolData", signature = "NanoStringExperiment",
    function(object) {
        ifelse(!"protocolCols" %in% names(metadata(object)),
            return(colData(object)),
            return(colData(object)[, metadata(object)$protocolCols]))
    })

#' Replace sample protocol metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase protocolData<-
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
            colData(object) <- value
            metadata(object)$protocolCols <- colnames(value)
            metadata(object)$phenotypeCols <- 
                metadata(object)$phenotypeCols[!metadata(object)$phenotypeCols 
                    %in% metadata(object)$protocolCols]
        }
        return(object)
    })

#' Replace sample protocol metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase protocolData<-
#' 
#' @return DataFrame of sample protocol metadata
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' protocolData(testExp) <- protocolData(testExp)[, 1:2]
#' 
#' @rdname sampleMetadata
#' 
#' @export
setReplaceMethod("protocolData", signature = "DataFrame",
    function(object, value) {
        object <- value
        return(object)
    })

#' Replace sample protocol metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase protocolData<-
#' 
#' @return DataFrame of sample protocol metadata
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' protocolData(testExp) <- protocolData(testExp)[, 1:2]
#' 
#' @rdname sampleMetadata
#' 
#' @export
setReplaceMethod("protocolData", signature = "vector",
    function(object, value) {
        object <- value
        return(object)
    })

#' Access sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase pData
#' 
#' @return matrix of sample phenotypic metadata
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' pData(testExp)
#' 
#' @rdname sampleMetadata
#' 
#' @export
setMethod("pData", signature = "NanoStringExperiment",
    function(object) {
        ifelse(!"phenotypeCols" %in% names(metadata(object)),
            return(colData(object)),
            return(colData(object)[, metadata(object)$phenotypeCols]))
    })

#' Access sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase pData
#' 
#' @return DataFrame of sample phenotypic metadata
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' pData(testExp) <- pData(testExp)[, 1:2]
#' 
#' @rdname sampleMetadata
#' 
#' @export
setReplaceMethod("pData", signature = "NanoStringExperiment",
    function(object, value) {
        if (!"phenotypeCols" %in% names(metadata(object))) {
            colData(object) <- value
        } else {
            colData(object) <- value
            metadata(object)$phenotypeCols <- colnames(value)
            metadata(object)$protocolCols <- 
                metadata(object)$protocolCols[!metadata(object)$protocolCols 
                    %in% metadata(object)$phenotypeCols]
        }
        return(object)
    })

#' Replace sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase pData<-
#' 
#' @return DataFrame of sample phenotypic metadata
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' pData(testExp) <- pData(testExp)[, 1:2]
#' 
#' @rdname sampleMetadata
#' 
#' @export
setReplaceMethod("pData", signature = "DataFrame",
    function(object, value) {
        object <- value
        return(object)
    })

#' Replace sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase pData<-
#' 
#' @return DataFrame of sample phenotypic metadata
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' pData(testExp) <- pData(testExp)[, 1:2]
#' 
#' @rdname sampleMetadata
#' 
#' @export
setReplaceMethod("pData", signature = "vector",
    function(object, value) {
        object <- value
        return(object)
    })

#' Access sample phenotypic metadata variables
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase varLabels
#' 
#' @return list of sample phenotypic metadata variables
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return list of metadata variables
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' varLabels(colData(testExp))
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
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' varLabels(testExp) <- paste0(varLabels(testExp), "a")
#' 
#' @rdname sampleMetadata
#' 
#' @export
setReplaceMethod("varLabels", signature = "NanoStringExperiment",
    function(object, value) {
        if(is.null(metadata(object)[["phenotypeCols"]])) {
            if (length(colnames(colData(object))) == length(value)) {
                colnames(colData(object)) <- value
            } else {
                stop("number of items to replace does ",
                    "not equal replacement length")
            }
        } else {
            if (length(colnames(colData(object))[colnames(colData(object)) %in%
                metadata(object)$phenotypeCols]) == length(value)) {
                colnames(colData(object))[colnames(colData(object)) %in% 
                    metadata(object)$phenotypeCols] <- value
                metadata(object)$phenotypeCols <- value
                metadata(object)$protocolCols <- 
                    metadata(object)$protocolCols[
                        !metadata(object)$protocolCols %in% 
                            metadata(object)$phenotypeCols]
            } else {
                stop("number of items to replace does ",
                    "not equal replacement length")
            }
        }
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
#' @return matrix of feature (column) metadata
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return list of feature metadata variables
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return matrix of feature (row) metadata
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return list of experiment metadata
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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

#' Access experiment annotations
#' 
#' Access annotations associated with the experiment
#' 
#' @importMethodsFrom BiocGenerics annotation
#' 
#' @return list of annotations used with experiment
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return formula used for design (model) matrix
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' design(testExp)
#' 
#' @export
setGeneric("design", signature = "object",
    function(object) standardGeneric("design"))

#' Access design formula
#' 
#' Access formula to be used for design in analyses
#' 
#' @return formula used for design (model) matrix
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return NanoStringExperiment object
#' 
#' @importMethodsFrom BiocGenerics design<-
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return NanoStringExperiment object
#' 
#' @importMethodsFrom BiocGenerics design<-
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return NanoStringExperiment object
#' 
#' @importMethodsFrom BiocGenerics design<-
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return SignatureSet object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' signatures(testExp)
#' 
#' @export
setGeneric("signatures", signature = "object", 
    function(object) standardGeneric("signatures"))

#' Access signatures slot
#' 
#' Access signatures slot to get \code{SignatureSet} object
#' 
#' @return SignatureSet object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
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
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' signatures(testExp) <- SignatureSet()
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
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' signatures(testExp) <- SignatureSet()
#' 
#' @rdname signatures
#' 
#' @export
setReplaceMethod("signatures", c("NanoStringExperiment", "SignatureSet"), 
    function(object, value) {
        object@signatures <- value
        return(object)
    })

