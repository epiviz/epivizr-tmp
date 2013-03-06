#' epiviz interactive device
#' 
#' This class wraps a GenomicRanges object to be plotted in an interactive epiviz device
#' 
#' @section Fields:
#' \describe{
#'  \item{\code{gr}:}{The the \link{GRanges-class} object bound to this track}
#'  \item{\code{type}:}{Display type, currently "regions", "line" or "gene"}
#'  \item{\code{mdCols}:}{vector of elementMetaData column names to use as measurements in plots}
#'  \item{\code{minValue}:}{minimum value for y axis}
#'  \item{\code{maxValue}:}{maximum value for y axis}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{getData}:}{Subset data from the GRanges object within given region}
#' }
#' 
#' @name EpivizDevice-class
#' @rdname EpivizDevice-class
#' 
#' @export
EpivizDevice <- setRefClass("EpivizDevice",
  fields=list(
    gr="GenomicRanges",
    type="character",
    mdCols="ANY",
    minValue="numeric",
    maxValue="numeric"),
  methods=list(
    getData=function() {
      
    }
  )
)

newDevice <- function(gr,
                      type=c("region","line","gene"),
                      mdCols=NULL,
                      minValue=-6,
                      maxValue=6)
{
  type=match.arg(type)
  EpivizDevice$new(gr=gr,
                   type=type,
                   mdCols=mdCols,
                   minValue=minValue,
                   maxValue=maxValue)
}