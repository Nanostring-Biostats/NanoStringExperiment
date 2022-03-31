#' Object class version accessor
#' 
#' Access version of NanoStringExperiment package used to generate object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' classVersion(testExp)
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
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' dimLabels(testExp)
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
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' colData(testExp)[["testNames"]] <- paste0(dimnames(testExp)[[2]], "a")
#' dimLabels(testExp) <- c(dimLabels(testExp)[[1]], "testNames")
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
#' Shows variables used for rownames in rowData.
#' 
#' @importMethodsFrom Biobase featureNames
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
#' @export
setReplaceMethod("featureNames", signature = "NanoStringExperiment",
    function(object, value) {
        dimnames(object)[[1L]] <- value
        return(object)
    })

#' Access sample identifiers
#' 
#' Shows variables used for rownames in colData.
#' 
#' @importMethodsFrom Biobase sampleNames
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
#' @export
setMethod("assayData", signature = "NanoStringExperiment",
    function(object) assays(object))

#' Access assay data
#' 
#' It is recommended to use the SummarizedExperiment assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
setGeneric("assayDataElement", signature = "object",
    function(object, elt) standardGeneric("assayDataElement"))

#' Access assay data
#' 
#' It is recommended to use the SummarizedExperiment assays instead.
#' This is a convenience method for backwards compatibility.
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
#' @export
setGeneric("assayDataElement<-", 
    signature = c("object", "value"),
    function(object, elt, ..., value) standardGeneric("assayDataElement<-"))

#' Replace or add new assay data
#' 
#' It is recommended to use the SummarizedExperiment assays instead.
#' This is a convenience method for backwards compatibility.
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
#' @export
setGeneric("assayDataElementNames", signature = "object",
    function(object) standardGeneric("assayDataElementNames"))

#' Access assay names
#' 
#' It is recommended to use the SummarizedExperiment assayNames instead.
#' This is a convenience method for backwards compatibility.
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
#' @export
setGeneric("sData", signature = "object",
    function(object) standardGeneric("sData"))

#' Access sample metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
setMethod("sData", signature = "NanoStringExperiment",
    function(object) colData(object))

#' Access sample metadata varible names
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
setGeneric("svarLabels", signature = "object",
    function(object) standardGeneric("svarLabels"))

#' Access sample metadata varible names
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
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
#' @export
setReplaceMethod("phenoData", signature = "NanoStringExperiment",
    function(object, value) {
        ifelse(!"phenotypeCols" %in% names(metadata(object)),
            colData(object) <- value,
            colData(object)[, metadata(object)$phenotypeCols] <- value)
        return(object)
    })

#' Replace sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase phenoData<-
#' 
#' @export
setReplaceMethod("phenoData", signature = "DataFrame",
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
#' @export
setReplaceMethod("protocolData", signature = "NanoStringExperiment",
    function(object, value) {
        ifelse(!"protocolCols" %in% names(metadata(object)),
            colData(object) <- value,
            colData(object)[, metadata(object)$protocolCols] <- value)
        return(object)
    })

#' Replace sample protocol metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase protocolData<-
#' 
#' @export
setReplaceMethod("protocolData", signature = "DataFrame",
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
#' @export
setReplaceMethod("pData", signature = "NanoStringExperiment",
    function(object, value) {
        ifelse(!"phenotypeCols" %in% names(metadata(object)),
            colData(object) <- value,
            colData(object)[, metadata(object)$phenotypeCols] <- value)
        return(object)
    })

#' Replace sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase pData<-
#' 
#' @export
setReplaceMethod("pData", signature = "DataFrame",
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
#' @export
setReplaceMethod("varLabels", signature = "NanoStringExperiment",
    function(object, value) {
        if(is.null(metadata(object)[["phenotypeCols"]])) {
            colnames(colData(object)) <- value
        } else {
            colnames(colData(object)[, 
                metadata(object)[["phenotypeCols"]]]) <- value
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
#' @export
setReplaceMethod("featureData", signature = "NanoStringExperiment",
    function(object, value) {
        rowData(object) <- value
        return(object)
    })

#' Replace feature metadata
#' 
#' It is recommended to use the SummarizedExperiment rowData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase featureData<-
#' 
#' @export
setReplaceMethod("featureData", signature = "DataFrame",
    function(object, value) {
        object <- value
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
#' @export
setMethod("annotation", signature="NanoStringExperiment",
    function(object) object@annotation)

#' Access design formula
#' 
#' Access formula to be used for design in analyses
#' 
#' @export
setGeneric("design", signature = "object",
    function(object) standardGeneric("design"))

#' Access design formula
#' 
#' Access formula to be used for design in analyses
#' 
#' @export
setMethod("design", "NanoStringExperiment", 
    function(object) object@design)

#' Replace design formula
#' 
#' Replace or assign formula to be used for design in analyses
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
#' @export
setReplaceMethod("design", c("NanoStringExperiment", "NULL"),
    function(object, value) {
        object@design <- NULL
        return(object)
    })
