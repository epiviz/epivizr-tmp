#' epiviz interactive device
#' 
#' This class wraps a GenomicRanges object to be plotted in an interactive epiviz device
#' 
#' @section Fields:
#' \describe{
#'  \item{\code{gr}:}{The the \link{GRanges-class} object bound to this track}
#'  \item{\code{tree}:}{A \link{GIntervalTree-class} object used for querying}
#'  \item{\code{id}:}{The id for this device assigned by the epiviz manager object}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{getData}:}{Subset data from the GRanges object within given region}
#' }
#' 
#' @param ... arguments passed to constructor
#' 
#' @aliases EpivizDevice 
#' @name EpivizDevice-class
#' @rdname EpivizDevice-class
#' 
#' @exportClass EpivizDevice
EpivizDevice <- setRefClass("EpivizDevice",
  fields=list(
    gr="GRanges",
    tree="GIntervalTree",
    id="character",
    columns="ANY"
  ),
  methods=list(
    initialize=function(...) {
      callSuper(...)
      gr <<- sort(gr)
      makeTree()
    }
    makeTree=function() {
      tree <<- GIntervalTree(gr)
    },
    findOverlaps=function(chr, start, end) {
      if (!chr %in% seqlevels(gr))
        return(integer())
      
      query <- GRanges(seqnames=chr,ranges=IRanges(start=start,end=end))
      olaps <- findOverlaps(query, tree, select="all")
      hits <- subjectHits(olaps)
      unique(hits)
    },
    subsetByOverlaps=function(chr, start, end) {
      hits <- .self$findOverlaps(chr,start,end)

      if (length(hits)>0) {
        gr[hits,]
        } else {
          GRanges()
        }
    },
    getData=function(chr, start, end) {
      ogr <- .self$subsetByOverlaps(chr,start,end)
      return(list(start=start(ogr),end=end(ogr)))
    },
    update=function(gr) {
      gr <<- sort(gr)
      makeTree()
      invisible()
    },
    id=function() {
      return(id)
    }
  )
)




