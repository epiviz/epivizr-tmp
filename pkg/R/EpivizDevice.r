#' epiviz interactive device
#' 
#' This class wraps a GenomicRanges object to be plotted in an interactive epiviz device
#' 
#' @section Fields:
#' \describe{
#'  \item{\code{gr}:}{The the \link{GRanges-class} object bound to this track}
#'  \item{\code{tree}:}{A \link{GIntervalTree-class}} object used for querying}
#'  \item{\code{id}:}{The id for this device assigned by the epiviz manager object}
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
    gr="GRanges",
    tree="PartitionedIntervalTree",
    id="character"
  ),
  methods=list(
    makeTree=function() {
      gr <<- sort(gr)
      tree <<- PartitionedIntervalTree(ranges(gr), seqnames(gr))
    },
    subsetGR=function(chr, start, end) {
      if (!chr %in% seqlevels(gr))
        return(GRanges())
      
      query=GRanges(seqnames=chr, ranges=IRanges(start=start,end=end),
                    seqinfo=seqinfo(gr))
      hits=subjectHits(findOverlaps(query, gr, select="all"))
      gr[unique(hits)]
    },
    getData=function(chr, start, end) {
      ogr=.self$subsetGR(chr,start,end)
      return(list(start=start(ogr),end=end(ogr)))
    },
    update=function(gr) {
      gr <<- gr
      makeTree()
      invisible()
    },
    id=function() {
      return(id)
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
  contains="EpivizDevice",
  methods=list(
    getData=function(chr, start, end, cols) {
      nCols=length(cols)
      out=list(min=rep(-6,nCols),
               max=rep(6,nCols),
               data=vector("list",nCols))
      for (i in seq_along(cols)) {
        out$data[[i]]=list(bp=integer(), value=numeric())
      }
      
      ogr=.self$subsetGR(chr,start,end)
      if (length(ogr)<1) {
        return(out)  
      }
      
      bp=start(ogr)
      
      for (i in seq_along(cols)) {
        vals=mcols(ogr)[[cols[i]]]
        rng=range(pretty(range(vals)))
        out$min[i]=rng[1]
        out$max[i]=rng[2]
        out$data[[i]]=list(bp=bp,value=vals)
      }
      return(out)
    },
    update=function(gr=gr,mdCols=NULL) {
      callSuper(gr=gr)
      if (!is.null(mdCols)) {
        if (!all(mdCols %in% mcols(gr))) {
          stop("invalid mdCols specified")
        }
        mdCols <<- mdCols
      }
      invisible()
    }
  )
)

EpivizGeneDevice <- setRefClass("EpivizGeneDevice",
  fields=list(
    mdCols="ANY"
  ),
  contains="EpivizDevice",
  methods=list(
    getData=function(chr, start, end) {
      ogr=.self$subsetGR(chr, start, end)
      out=list(start=start(ogr), end=end(ogr))
    }  
  )              
)

.newBlockDevice <- function(gr, id)
{
  return(EpivizBlockDevice$new(gr=gr, id=id))
}

.newBpDevice <- function(gr, id, mdCols=names(mcols(gr))) {
  if (!all(mdCols %in% names(mcols(gr))))
    stop("mdCols not found in GRanges object")
  
  return(EpivizBpDevice$new(gr=gr,id=id,mdCols=mdCols))
}

.newGeneDevice <- function(gr, id, mdCols=names(mcols(gr))) {
  if (!all(mdCols %in% names(mcols(gr))))
    stop("mdCols not found in GRanges object")
  
  return(EpivizGeneDevice$new(gr=gr, id=id, mdCols=mdCols))
}

.typeMap <- list(gene=list(constructor=.newGeneDevice,class="EpivizGeneDevice"),
              bp=list(constructor=.newBpDevice,class="EpivizBpDevice"),
              block=list(constructor=.newBlockDevice,class="EpivizBlockDevice"))

#' Create a new EpivizDevice object
#' 
#' This is called by the \link{addDevice} method in manager objects. This function should not be called directly.
#' @param gr A \link{GRanges} object to display
#' @param id The id of the device assigned by the manager
#' @param type The type of device to create
#' @param ... Arguments passed to the constructor function
newDevice <- function(gr, id, type="block",...)                      
{
  if (!type %in% names(.typeMap))
    stop("Unknown device type")
  obj <- .typeMap[[type]]$constructor(gr, id, ...)
  obj$makeTree()
  return(obj)
}