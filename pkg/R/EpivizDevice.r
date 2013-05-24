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
    tree="IntervalForest",
    id="character"
  ),
  methods=list(
    makeTree=function() {
      tree <<- IntervalForest(ranges(gr), seqnames(gr))
    },
    subsetGR=function(chr, start, end) {
      if (!chr %in% seqlevels(gr))
        return(GRanges())
      
      hits <- subjectHits(findOverlaps(IRanges(start=start,end=end), tree, partition=Rle(factor(chr))))
      gr[unique(hits)]
    },
    getData=function(chr, start, end) {
      ogr=.self$subsetGR(chr,start,end)
      return(list(start=start(ogr),end=end(ogr)))
    },
    getDataWithValues=function(chr, start, end, cols) {
      nCols=length(cols)
      out=list(min=rep(-6,nCols),
               max=rep(6,nCols),
               data=vector("list",nCols))
      for (i in seq_along(cols)) {
        out$data[[i]]=list(start=integer(), end=integer(), value=numeric())
      }
      
      ogr=.self$subsetGR(chr,start,end)
      if (length(ogr)<1) {
        return(out)  
      }
      
      dataStart=start(ogr)
      dataEnd=end(ogr)
      
      for (i in seq_along(cols)) {
        vals=mcols(ogr)[[cols[i]]]
        if (all(is.na(vals))) {
          next
        }
        naIndex=is.na(vals)
        if (any(naIndex)) {
          out$data[[i]]=list(start=dataStart[!naIndex], end=dataEnd[!naIndex], value=vals[!naIndex])
          vals=vals[!naIndex]
        } else {
          out$data[[i]]=list(start=dataStart,end=dataEnd,value=vals)
        }
        rng=range(pretty(range(vals)))
        out$min[i]=rng[1]
        out$max[i]=rng[2]
      }
      return(out)
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
      tmp <- getDataWithValues(chr, start, end, cols)
      nCols <- length(tmp$data)
      for (i in seq(len=nCols)) {
        names(tmp$data[[i]])[1] <- "bp"
        tmp$data[[i]]$end=NULL
      }
      tmp
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
)

.newBlockDevice <- function(obj, id)
{
  if (!is(obj, "GenomicRanges")) {
    stop("'obj' must be a 'GenomicRanges' object")
  }
  return(EpivizBlockDevice$new(gr=obj, id=id))
}

.newBpDevice <- function(obj, id, mdCols=names(mcols(obj))) {
  if (!is(obj, "GenomicRanges")) {
    stop("'obj' must be a 'GenomicRanges' object")
  }

  if (!all(mdCols %in% names(mcols(obj))))
    stop("mdCols not found in GRanges object")
  
  return(EpivizBpDevice$new(gr=obj,id=id,mdCols=mdCols))
}

.newGeneDevice <- function(obj, id, x, y, ...) {
  if (!is(obj, "ExpressionSet")) {
    stop("'obj' must be of class 'ExpressionSet'")
  } 
  if (annotation(obj) != 'hgu133plus2') {
    stop("only 'hgu133plus2' affy chips supported for now")
  }

  # verify x and y are valid names
  if (any(!(c(x,y) %in% sampleNames(obj)))) {
    stop("'x' or 'y' not found in 'sampleNames'")
  }

  # make GRanges object with approprite info
  probeids = featureNames(obj)
  annoName = paste0(annotation(obj), ".db")
  if (!require(annoName, character.only=TRUE)) {
    stop("package '", annoName, "' is required")
  }

  res = suppressWarnings(select(get(annoName), keys=probeids, cols=c("SYMBOL", "CHR", "CHRLOC", "CHRLOCEND"), keytype="PROBEID"))
  dups = duplicated(res$PROBEID)
  res = res[!dups,]

  isna = is.na(res$CHR) | is.na(res$CHRLOC) | is.na(res$CHRLOCEND)
  res = res[!isna,]

  gr = GRanges(seqnames=paste0("chr",res$CHR),
               strand=ifelse(res$CHRLOC>0, "+","-"),
               ranges=IRanges(start=abs(res$CHRLOC), end=abs(res$CHRLOCEND)))

  mcols(gr)[,x] = exprs(obj)[!isna,x]
  mcols(gr)[,y] = exprs(obj)[!isna,y]

  mcols(gr)[,"SYMBOL"] = res$SYMBOL
  mcols(gr)[,"PROBEID"] = res$PROBEID

  return(EpivizGeneDevice$new(gr=gr, id=id, mdCols=c(x,y)))
}

.typeMap <- list(gene=list(constructor=.newGeneDevice,class="EpivizGeneDevice"),
              bp=list(constructor=.newBpDevice,class="EpivizBpDevice"),
              block=list(constructor=.newBlockDevice,class="EpivizBlockDevice"))

newDevice <- function(obj, id, type="block",...)                      
{
  if (!type %in% names(.typeMap))
    stop("Unknown device type")
  obj <- .typeMap[[type]]$constructor(obj, id, ...)
  obj$makeTree()
  return(obj)
}
