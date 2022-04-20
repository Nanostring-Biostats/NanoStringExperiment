setClassUnion("formulaOrNULL", c("formula", "NULL"))
setClassUnion("matrixOrNULL", c("formula", "NULL"))

#' The NanoStringExperiment class
#' 
#' Class definition for the base NanoString experiment
#' 
#' @import SummarizedExperiment
#' @importFrom Biobase AnnotatedDataFrame MIAME
#' @importFrom methods new
#' @importClassesFrom NanoStringNCTools SignatureSet
#' 
#' @slot assayData expression matrix or NULL
#' @slot annotation character list of annotations used
#' @slot dimLabels character list of dimension labels
#' @slot signatures optional SignatureSet object
#' @slot design formula or NULL
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
        signatures = NanoStringNCTools::SignatureSet(),
        design = NULL)
)

#' Show method
#' 
#' Show method for NanoStringExperiment
#' 
#' @return summary of NanoStringExperiment object
#' 
#' @examples
#' data(exampleNSEData)
#' show(testExp)
#' 
#' @importFrom methods callNextMethod
#' 
#' @rdname NanoStringExperiment-class
#' 
#' @export
setMethod("show", signature = "NanoStringExperiment",
function(object) {
    methods::callNextMethod(object)
    cat("dimLabels: ")
    cat(object@dimLabels)
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
#' data(exampleNSEData)
#' NanoStringExperiment(
#'     assayData = testAssayData,
#'     phenoData = testPhenoData,
#'     protocolData = testProtocolData,
#'     featureData = testFeatureData,
#'     experimentData = testExperimentData,
#'     annotation = testAnnotation,
#'     dimLabels = testDimLabels)
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
         signatures = NanoStringNCTools::SignatureSet(),
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
         signatures = NanoStringNCTools::SignatureSet(),
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
#' data(exampleNSEData)
#' NanoStringExperiment(
#'     assayData = testAssayData,
#'     phenoData = testPhenoData,
#'     protocolData = testProtocolData,
#'     featureData = testFeatureData,
#'     experimentData = testExperimentData,
#'     annotation = testAnnotation,
#'     dimLabels = testDimLabels)
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
         signatures = NanoStringNCTools::SignatureSet(),
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
        metadata = c(expinfo(experimentData), otherInfo(experimentData)))
    .NanoStringExperiment(se,
        annotation = annotation,
        dimLabels = dimLabels,
        signatures = signatures,
        design = design)
})
