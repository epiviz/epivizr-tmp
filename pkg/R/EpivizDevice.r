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
    gr="GenomicRanges"
  ),
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
    mdCols="ANY"
  ),
  contains="EpivizDevice"                            
)

EpivizGeneDevice <- setRefClass("EpivizGeneDevice",
  fields=list(
    mdCols="ANY"
  ),
  contains="EpivizDevice"                              
)

.newBlockDevice <- function(gr)
{
  return(EpivizBlockDevice$new(gr=gr))
}

.newBpDevice <- function(gr, mdCols=names(mcols(gr))) {
  if (!all(mdCols %in% names(mcols(gr))))
    stop("mdCols not found in GRanges object")
  
  return(EpivizBpDevice$new(gr=gr,mdCols=mdCols))
}

.newGeneDevice <- function(gr, mdCols=names(mcols(gr))) {
  if (!all(mdCols %in% names(mcols(gr))))
    stop("mdCols not found in GRanges object")
  
  return(EpivizGeneDevice$new(gr=gr,mdCols=mdCols))
}

.typeMap <- list(gene=list(constructor=.newGeneDevice,class="EpivizGeneDevice"),
              bp=list(constructor=.newBpDevice,class="EpivizBpDevice"),
              block=list(constructor=.newBlockDevice,class="EpivizBlockDevice"))

newDevice <- function(gr, type="block",...)                      
{
  if (!type %in% names(.typeMap))
    stop("Unknown device type")
  
  return(.typeMap[[type]]$constructor(gr,...))
}