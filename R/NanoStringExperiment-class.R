setClassUnion("formulaOrNULL", c("formula", "NULL"))
setClassUnion("matrixOrNULL", c("formula", "NULL"))
#setClassUnion("SignatureSetOrNULL", c("SignatureSet", "NULL"))
#setClassUnion("listOrSimpleList", c("list", "SimpleList"))
#setClassUnion("RectangularishOrNULL", c("data.frame", "matrix", "AnnotatedDataFrame", "NULL"))
#setClassUnion("MIAMEOrNULL", c("MIAME", "NULL"))
#setClass("VersionedSummarizedExp", contains = c("VersionedBiobase", "SummarizedExperiment"))

#' The NanoStringExperiment class
#' 
#' Class definition for the base NanoString experiment
#' 
#' 
#' @import SummarizedExperiment
#' @importFrom Biobase AnnotatedDataFrame MIAME
#' 
#' @export
.NanoStringExperiment <- setClass("NanoStringExperiment",
    contains = c("SummarizedExperiment"),
    slots = c(
        assayData = "matrixOrNULL",
        phenoData = "AnnotatedDataFrame",
        protocolData = "AnnotatedDataFrame",
        featureData = "AnnotatedDataFrame",
        experimentData = "MIAME",
        annotation = "character",
        dimLabels = "character",
        signatures = "ANY",
        design = "formulaOrNULL",
        .__classVersion__ = "character"),
    prototype = prototype(
        new("SummarizedExperiment"),
        .__classVersion__ = 
            paste(packageVersion("NanoStringExperiment"), collapse="."),
        signatures = NULL,
        design = NULL)
)

# Show method
#' @export
setMethod("show", signature = "NanoStringExperiment",
function(object) {
    methods::callNextMethod(object)
    cat("dimLabels: ")
    cat(object[["dimLabels"]])
    cat("\nclassVersion: ")
    cat(classVersion(object))
    cat("\nsignature: ")
    if (length(signatures(object)) == 0L) {
        cat("none\n")
    } else {
        cat("use 'signatures(object)'\n")
    }
})

#' @export
setGeneric("NanoStringExperiment",
function(assayData,
         phenoData = Biobase::annotatedDataFrameFrom(assayData, byrow = FALSE),
         protocolData = Biobase::annotatedDataFrameFrom(assayData, byrow = FALSE),
         featureData = Biobase::annotatedDataFrameFrom(assayData, byrow = TRUE),
         experimentData = Biobase::MIAME(),
         annotation=character(),
         dimLabels = character(),
         signatures = NULL,
         design = NULL,
         ...) {
    standardGeneric("NanoStringExperiment")
},
signature = "assayData"
)

#' @export
setMethod("NanoStringExperiment", "missing",
function(assayData,
         phenoData = 
             Biobase::annotatedDataFrameFrom(assayData, byrow = FALSE),
         protocolData = 
             Biobase::annotatedDataFrameFrom(assayData, byrow = FALSE),
         featureData = 
             Biobase::annotatedDataFrameFrom(assayData, byrow = TRUE),
         experimentData = Biobase::MIAME(),
         annotation = character(),
         dimLabels = character(),
         signatures = NULL,
         design = NULL,
         ...) {
    se <- SummarizedExperiment()
    .NanoStringExperiment(se, 
        annotation = annotation,
        dimLabels = dimLabels,
        signatures = signatures,
        design = design)
})

#' @export
setMethod("NanoStringExperiment", "matrix",
function(assayData,
         phenoData = 
             Biobase::annotatedDataFrameFrom(assayData, byrow = FALSE),
         protocolData = 
             Biobase::annotatedDataFrameFrom(assayData, byrow = FALSE),
         featureData = 
             Biobase::annotatedDataFrameFrom(assayData, byrow = TRUE),
         experimentData = Biobase::MIAME(),
         annotation = character(),
         dimLabels = character(),
         signatures = NULL,
         design = NULL,
         ...) {
    allColData <- cbind(phenoData@data, protocolData@data)
    versionCols <- 
        colnames(allColData)[lapply(allColData, class) == "numeric_version"]
    for (vCol in versionCols) {
        allColData[, vCol] <- 
            unlist(lapply(allColData[, vCol], paste, collapse = "."))
    }
    se <- SummarizedExperiment(
        assays = list(exprs = assayData),
        colData = allColData,
        rowData = featureData@data,
        metadata = experimentData@other,
        ...)
    .NanoStringExperiment(se,
        annotation = annotation,
        dimLabels = dimLabels,
        signatures = signatures,
        design = design)
})



