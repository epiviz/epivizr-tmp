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
    mdCols="ANY",
    minValue="numeric",
    maxValue="numeric"),
  methods=list(
    getData=function(chr, start, end) {
      query=GRanges(seqnames=chr, ranges=IRanges(start=start,end=end),
                    seqinfo=seqinfo(gr))
      subsetByOverlaps(gr, query)
    }
  )
)

EpivizBlockDevice <- setRefClass("EpivizBlockDevice",
  fields=list(dummy="character"),
  contains="EpivizDevice"
)

EpivizBpDevice <- setRefClass("EpivizBpDevice",
  fields=list(
    mdCols="ANY",
    minValue="numeric",
    maxValue="numeric"
  ),
  contains="EpivizDevice"                            
)

EpivizGeneDevice <- setRefClass("EpivizGeneDevice",
  fields=list(
    mdCols="ANY",
    minValue="numeric",
    maxValue="numeric"
  ),
  contains="EpivizDevice"                              
)

.newBlockDevice <- function(gr)
{
  return(EpivizBlockDevice$new(gr=gr))
}

newDevice <- function(gr, type=c("region","line","gene"),...)                      
{
  type=match.arg(type)
  constructFun=switch(type,
                      region=.newBlockDevice,
                      line=.newBpDevice,
                      gene=.newGeneDevice)
  return(constructFun(gr, ...))
}