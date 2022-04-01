setClassUnion("formulaOrNULL", c("formula", "NULL"))
setClassUnion("matrixOrNULL", c("formula", "NULL"))

#' The NanoStringExperiment class
#' 
#' Class definition for the base NanoString experiment
#' 
#' @import SummarizedExperiment
#' @importFrom Biobase AnnotatedDataFrame MIAME
#' 
#' @slot \code{assayData} expression matrix or NULL
#' @slot \code{annotation} character list of annotations used
#' @slot \code{dimLabels} character list of dimension labels
#' @slot \code{signatures} optional SignatureSet object or NULL
#' @slot \code{design} formula or NULL
#' 
#' @return NanoStringExperiment object
#' 
#' @rdname NanoStringExperiment-class
#' 
#' @export
.NanoStringExperiment <- setClass("NanoStringExperiment",
    contains = c("SummarizedExperiment"),
    slots = c(
        assayData = "matrixOrNULL",
        annotation = "character",
        dimLabels = "character",
        signatures = "SignatureSet",
        design = "formulaOrNULL",
        .__classVersion__ = "character"),
    prototype = prototype(
        new("SummarizedExperiment"),
        .__classVersion__ = 
            paste(packageVersion("NanoStringExperiment"), collapse="."),
        signatures = NULL,
        design = NULL)
)

#' Show method
#' 
#' Show method for NanoStringExperiment
#' 
#' @return summary of NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' testExp <- readRDS(file.path(datadir, "testExp.rds"))
#' show(testExp)
#' 
#' @rdname NanoStringExperiment-class
#' 
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

#' Generic constructor with assay data
#' 
#' Constructs NanoStringExperiment object based on assay data signature
#' 
#' @importFrom Biobase annotatedDataFrameFrom
#' @importFrom Biobase MIAME
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' NanoStringExperiment(
#'     assayData = readRDS(file.path(datadir, "testAssayData.rds")),
#'     phenoData = readRDS(file.path(datadir, "testPhenoData.rds")),
#'     protocolData = readRDS(file.path(datadir, "testProtocolData.rds")),
#'     featureData = readRDS(file.path(datadir, "testFeatureData.rds")),
#'     experimentData = readRDS(file.path(datadir, "testExperimentData.rds")),
#'     annotation = readRDS(file.path(datadir, "testAnnotation.rds")),
#'     dimLabels = readRDS(file.path(datadir, "testDimLabels.rds")))
#' 
#' @rdname NanoStringExperiment-class
#' 
#' @export
setGeneric("NanoStringExperiment",
function(assayData,
         phenoData = Biobase::annotatedDataFrameFrom(assayData, byrow = FALSE),
         protocolData = 
             Biobase::annotatedDataFrameFrom(assayData, byrow = FALSE),
         featureData = 
             Biobase::annotatedDataFrameFrom(assayData, byrow = TRUE),
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

#' Default constructor
#' 
#' Constructs NanoStringExperiment object if assay data missing
#' 
#' @importFrom Biobase annotatedDataFrameFrom
#' @importFrom Biobase MIAME
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' NanoStringExperiment()
#' 
#' @rdname NanoStringExperiment-class
#' 
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

#' Constructor with assay data
#' 
#' Constructs NanoStringExperiment object if assay data is a matrix
#' 
#' @importFrom Biobase annotatedDataFrameFrom
#' @importFrom Biobase MIAME
#' 
#' @return NanoStringExperiment object
#' 
#' @examples
#' datadir <- system.file("data", package="NanoStringExperiment")
#' NanoStringExperiment(
#'     assayData = readRDS(file.path(datadir, "testAssayData.rds")),
#'     phenoData = readRDS(file.path(datadir, "testPhenoData.rds")),
#'     protocolData = readRDS(file.path(datadir, "testProtocolData.rds")),
#'     featureData = readRDS(file.path(datadir, "testFeatureData.rds")),
#'     experimentData = readRDS(file.path(datadir, "testExperimentData.rds")),
#'     annotation = readRDS(file.path(datadir, "testAnnotation.rds")),
#'     dimLabels = readRDS(file.path(datadir, "testDimLabels.rds")))
#' 
#' @rdname NanoStringExperiment-class
#' 
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
        metadata = experimentData@other)
    .NanoStringExperiment(se,
        annotation = annotation,
        dimLabels = dimLabels,
        signatures = signatures,
        design = design)
})
