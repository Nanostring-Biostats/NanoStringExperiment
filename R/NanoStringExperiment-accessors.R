#' Object class version accessor
#' 
#' Access version of NanoStringExperiment package used to generate object
#' 
#' @export
setMethod("classVersion", "NanoStringExperiment",
    function(object) object@.__classVersion__)


# Assay Data ------------------------------------------------------------------

#' Access assays
#' 
#' It is recommended to use the SummarizedExperiment method assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
assayData <- function(object) assays(object)

#' Access assay data
#' 
#' It is recommended to use the SummarizedExperiment method assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
assayDataElement <- function(object, elt) assays(object)[[elt]]

#' Replace or add new assay data
#' 
#' It is recommended to use the SummarizedExperiment method assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
`assayDataElement<-` <- 
    function(object, elt, ..., value) assays(object)[[elt]] <- value

#' Access assay names
#' 
#' It is recommended to use the SummarizedExperiment method assayNames instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
assayDataElementNames <- function(object) assayNames(object)

#' Access exprs data
#' 
#' It is recommended to use the SummarizedExperiment method assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
exprs <- function(object) assays(object)[["exprs"]]

#' Replace exprs data
#' 
#' It is recommended to use the SummarizedExperiment method assays instead.
#' This is a convenience method for backwards compatibility.
#' 
#' @export
`exprs<-` <- function(object, ..., value) assays(object)[["exprs"]] <- value

# Column Data (Formerly Sample Data) ------------------------------------------


# Row Data (Formerly Feature Data) --------------------------------------------