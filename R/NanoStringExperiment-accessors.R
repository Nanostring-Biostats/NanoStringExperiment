#' Object class version accessor
#' 
#' Access version of NanoStringExperiment package used to generate object
#' 
#' @export
setMethod("classVersion", signature = "NanoStringExperiment",
    function(object) object@.__classVersion__)


# Assay Data ------------------------------------------------------------------

#' Access assays
#' 
#' It is recommended to use the SummarizedExperiment method assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase assayData
#' 
#' @export
setMethod("assayData", signature = "NanoStringExperiment",
    function(object) assays(object))

#' Access assay data
#' 
#' It is recommended to use the SummarizedExperiment method assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
setGeneric("assayDataElement", signature = "object",
    function(object, elt) standardGeneric("assayDataElement"))

#' Access assay data
#' 
#' It is recommended to use the SummarizedExperiment method assays instead.
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
#' It is recommended to use the SummarizedExperiment method assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
setGeneric("assayDataElement<-", 
    signature = c("object", "value"),
    function(object, elt, ..., value) standardGeneric("assayDataElement<-"))

#' Replace or add new assay data
#' 
#' It is recommended to use the SummarizedExperiment method assays instead.
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
#' It is recommended to use the SummarizedExperiment method assayNames instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
setGeneric("assayDataElementNames", signature = "object",
    function(object) standardGeneric("assayDataElementNames"))

#' Access assay names
#' 
#' It is recommended to use the SummarizedExperiment method assayNames instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
setMethod("assayDataElementNames", signature = "NanoStringExperiment",
    function(object) assayNames(object))

#' Access exprs data
#' 
#' It is recommended to use the SummarizedExperiment method assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase exprs
#' 
#' @export
setMethod("exprs", signature = "NanoStringExperiment",
    function(object) assays(object)[["exprs"]])

#' Replace exprs data
#' 
#' It is recommended to use the SummarizedExperiment method assays instead.
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
#' It is recommended to use the SummarizedExperiment method colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
setGeneric("sData", signature = "object",
           function(object) standardGeneric("sData"))

#' Access sample metadata
#' 
#' It is recommended to use the SummarizedExperiment method colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
setMethod("sData", signature = "NanoStringExperiment",
          function(object) colData(testExp))

#' Access sample metadata varible names
#' 
#' It is recommended to use the SummarizedExperiment method colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
setGeneric("svarLabels", signature = "object",
           function(object) standardGeneric("svarLabels"))

#' Access sample metadata varible names
#' 
#' It is recommended to use the SummarizedExperiment method colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
setMethod("svarLabels", signature = "NanoStringExperiment",
          function(object) colnames(colData(testExp)))

#' Access sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment method colData instead.
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
#' It is recommended to use the SummarizedExperiment method colData instead.
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
#' It is recommended to use the SummarizedExperiment method colData instead.
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
#' It is recommended to use the SummarizedExperiment method colData instead.
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
#' It is recommended to use the SummarizedExperiment method colData instead.
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
#' It is recommended to use the SummarizedExperiment method colData instead.
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
#' It is recommended to use the SummarizedExperiment method colData instead.
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
#' It is recommended to use the SummarizedExperiment method colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase pData
#' 
#' @export
setMethod("pData", signature = "DataFrame",
    function(object) object)

#' Replace sample phenotypic metadata
#' 
#' It is recommended to use the SummarizedExperiment method colData instead.
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
#' It is recommended to use the SummarizedExperiment method colData instead.
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
#' It is recommended to use the SummarizedExperiment method colData instead.
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
#' It is recommended to use the SummarizedExperiment method colData instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @importMethodsFrom Biobase varLabels<-
#' 
#' @export
setReplaceMethod("varLabels", signature = "DataFrame",
    function(object, value) {
        colnames(object) <- value
        return(object)
    })


# Row Metadata (Formerly Feature Data) ----------------------------------------
#fData
#featureData
#featureData()[[]]
#fvarLabels

# Experiment Metadata ---------------------------------------------------------
#experimentData

# Other 
#assayDataApply
#esApply
#munge
#dimLabels
#dimLabels<-
#sampleNames
#sampleNames<-
#featureNames
#featureNames<-
