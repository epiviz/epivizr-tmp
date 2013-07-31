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
  contains="VIRTUAL",
  fields=list(
    object="GenomicRanges",
    tree="GIntervalTree",
    mgr="EpivizDeviceMgr",
    id="character",
    columns="ANY",
    ylim="matrix"
  ),
  methods=list(
    initialize=function(object=GRanges(), columns=NULL, ylim=matrix(), ...) {
      object <<- sort(object)
      makeTree()

      if (!is.null(columns)) {
        if (missing(ylim)) {
          ylim <<- sapply(columns, function(x) range(pretty(range(mcols(object)[,x]))))
        } else {
          ylim <<- ylim
        }
      }
      columns <<- columns
      callSuper(...)
    },
    makeTree=function() {
      tree <<- GIntervalTree(as(object, "GRanges"))
    },
    update=function(object) {
      object <<- sort(object)
      makeTree()
      invisible()
    },
    getId=function() {
      return(id)
    },
    setId=function(id) {
      id <<- id
      invisible()
    },
    setMgr=function(mgr) {
      mgr <<- mgr
      invisible()
    }
  )
)

.valid.EpivizDevice.columns <- function(x) {
  if (!is.null(x$columns)) {
    if (any(!(x$columns %in% names(mcols(x$object)))))
      return("'columns' not in 'object' names")
  }
  NULL
}

.valid.EpivizDevice.seqnames <- function(x) {
  if (any(seqlevels(x$object) != seqlevels(x$tree)))
    return("'seqlevels(x$object)' does not match 'seqlevels(x$tree)'")
  NULL
}

.valid.EpivizDevice.length <- function(x) {
  if (length(x$object) != length(x$tree))
    return("'length(x$object)' does not match 'length(x$tree)'")
  NULL
}

.valid.EpivizDevice.ylim <- function(x) {
  if (!is.null(x$columns)) {
    if (!ncol(x$ylim) == length(x$columns))
      return("'ncol(x$ylim)' does not match 'length(x$columns)'")

    if (nrow(x$ylim) != 2L) {
      return("'nrow(x$ylim)' does not equal 2")
    }
  }
  NULL
}

.valid.EpivizDevice <- function(object) {
  c(.valid.EpivizDevice.columns(object),
    .valid.EpivizDevice.seqnames(object),
    .valid.EpivizDevice.length(object),
    .valid.EpivizDevice.ylim(object))
}

setValidity("EpivizDevice", .valid.EpivizDevice)


EpivizDevice$methods(list(
    findOverlaps=function(chr, start, end) {
      if (!chr %in% seqlevels(tree))
        return(integer())
      
      query <- GRanges(seqnames=chr,ranges=IRanges(start=start,end=end))
      olaps <- findOverlaps(query, tree, select="all")
      hits <- subjectHits(olaps)
      unique(hits)
    },
    subsetByOverlaps=function(chr, start, end) {
      hits <- .self$findOverlaps(chr,start,end)

      if (length(hits)>0) {
        object[hits,]
        } else {
          new(class(object))
        }
    },
    getData=function(chr, start, end) {
      sobject <- .self$subsetByOverlaps(chr,start,end)
      return(list(start=start(sobject),end=end(sobject)))
    }
  )
)




