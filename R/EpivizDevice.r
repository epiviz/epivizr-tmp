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
    id="character"
  ),
  methods=list(
    makeTree=function() {
      tree <<- GIntervalTree(gr)
    },
    findOverlaps=function(chr, start, end) {
      if (!chr %in% seqlevels(gr))
        return(integer())
      
      query <- GRanges(seqnames=chr,ranges=IRanges(start=start,end=end))
      olaps <- IRanges::findOverlaps(query, tree, select="all")
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
    getDataWithValues=function(chr, start, end, cols) {
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

EpivizBlockDevice <- setRefClass("EpivizBlockDevice",
  fields=list(dummy="character"),
  contains="EpivizDevice"
)

EpivizBpDevice <- setRefClass("EpivizBpDevice",
  fields=list(
    mdCols="ANY",
    ylim="matrix"
  ),
  contains="EpivizDevice",
  methods=list(
    getData=function(chr, start, end, cols) {
      nCols=length(cols)
      out=list(min=unname(.self$ylim[1,]),
               max=unname(.self$ylim[2,]),
               data=vector("list",nCols))
      for (i in seq_along(cols)) {
        out$data[[i]]=list(bp=integer(), value=numeric())
      }
      
      ogr=.self$subsetByOverlaps(chr,start,end)
      if (length(ogr)<1) {
        return(out)  
      }
      
      bp=start(ogr)
      
      for (i in seq_along(cols)) {
        vals=mcols(ogr)[[cols[i]]]
        if (all(is.na(vals))) {
          next
        }
        naIndex=is.na(vals)
        if (any(naIndex)) {
          out$data[[i]]=list(bp=bp[!naIndex], value=vals[!naIndex])
          vals=vals[!naIndex]
        } else {
          out$data[[i]]=list(bp=bp,value=vals)
        }
        # rng=range(pretty(range(vals)))
        # out$min[i]=rng[1]
        # out$max[i]=rng[2]
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
    mdCols="ANY",
    xlim="numeric",
    ylim="numeric"
  ),
  contains="EpivizDevice",
  methods=list(
    getData=function(chr, start, end, cols) {
      nCols=length(cols)
      out=list(min=c(xlim[1],ylim[1]),
               max=c(xlim[2],ylim[2]),
               data=list(gene=character(),
                         start=integer(),
                         end=integer(),
                         probe=character()))
      for (i in seq_along(cols)) {
        out$data[[i+4]]=numeric()
      }
      
      ogr=.self$subsetByOverlaps(chr,start,end)
      if (length(ogr)<1) {
        return(out)  
      }
      
      out$data$gene=ogr$SYMBOL
      out$data$start=start(ogr)
      out$data$end=end(ogr)
      out$data$probe=ogr$PROBEID

      for (i in seq_along(cols)) {
        vals=unname(mcols(ogr)[[cols[i]]])
        if (all(is.na(vals))) {
          next
        }
        naIndex=is.na(vals)
        if (any(naIndex)) {
          out$data[[i+4]]=vals[!naIndex]
          vals=vals[!naIndex]
        } else {
          out$data[[i+4]]=vals
        }
        # rng=range(pretty(range(vals)))
        # out$min[i]=rng[1]
        # out$max[i]=rng[2]
      }
      return(out)
    }
  )
)

.newBlockDevice <- function(obj, id)
{
  if (!is(obj, "GenomicRanges")) {
    stop("'obj' must be a 'GenomicRanges' object")
  }
  return(EpivizBlockDevice$new(gr=obj, id=id))
}

.newBpDevice <- function(obj, id, mdCols=names(mcols(obj)), ylim=NULL, ...) {
  if (!is(obj, "GenomicRanges")) {
    stop("'obj' must be a 'GenomicRanges' object")
  }

  if (!all(mdCols %in% names(mcols(obj))))
    stop("mdCols not found in GRanges object")
  
  if (missing(ylim) || is.null(ylim)) {
    ylim <- sapply(mdCols, function(i) range(mcols(obj)[,i], na.rm=TRUE))
    ylim <- apply(ylim,2,function(rng) range(pretty(seq(rng[1],rng[2],len=10))))
  }

  return(EpivizBpDevice$new(gr=obj,id=id,mdCols=mdCols,ylim=ylim))
}

.newGeneDevice <- function(obj, id, x, y, xlim=NULL, ylim=NULL, ...) {
	if (is(obj, "SummarizedExperiment")) {
		return(.newGeneSEDevice(obj, id=id, x=x, y=y, xlim=xlim, ylim=ylim, ...))
	} else {
		if (!is(obj, "ExpressionSet")) {
			stop("'obj' must be of class 'ExpressionSet' or 'SummarizedExperiment'")
		} 
		if (annotation(obj) != 'hgu133plus2') {
			stop("only 'hgu133plus2' affy chips supported for now")
		}
		
		# verify x and y are valid names
		if (any(!(c(x,y) %in% sampleNames(obj)))) {
			stop("'x' or 'y' not found in 'sampleNames'")
		}
		
		# make GRanges object with appropriate info
		probeids = featureNames(obj)
		annoName = paste0(annotation(obj), ".db")
		if (!require(annoName, character.only=TRUE)) {
			stop("package '", annoName, "' is required")
		}
		
		res = suppressWarnings(select(get(annoName), keys=probeids, columns=c("SYMBOL", "CHR", "CHRLOC", "CHRLOCEND"), keytype="PROBEID"))
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
		
		if (missing(xlim) || is.null(xlim)) {
			rng <- range(mcols(gr)[,x])
			xlim <- range(pretty(seq(rng[1],rng[2],len=10)))
		}
		if (missing(ylim) || is.null(ylim)) {
			rng <- range(mcols(gr)[,y])
			ylim <- range(pretty(seq(rng[1],rng[2],len=10)))
		}
		
		return(EpivizGeneDevice$new(gr=gr, id=id, mdCols=c(x,y),xlim=xlim,ylim=ylim))
	}
}

.newGeneSEDevice <- function(obj, id, x, y, xlim=NULL, ylim=NULL, ...) {
	# verify x and y are valid samples
	if (any(!(c(x,y) %in% colnames(obj)))) {
		stop("'x' or 'y' not found as samples")
	}
	
	gr <- rowData(obj)
	
	mcols(gr)[,x] <- assay(obj)[,x]
	mcols(gr)[,y] <- assay(obj)[,y]
	
	mcols(gr)[,"SYMBOL"] = ""
	mcols(gr)[,"PROBEID"] = rownames(obj)
	
	if (missing(xlim) || is.null(xlim)) {
		rng <- range(mcols(gr)[,x])
		xlim <- range(pretty(seq(rng[1],rng[2],len=10)))
	}
	if (missing(ylim) || is.null(ylim)) {
		rng <- range(mcols(gr)[,y])
		ylim <- range(pretty(seq(rng[1],rng[2],len=10)))
	}
	
	return(EpivizGeneDevice$new(gr=gr, id=id, mdCols=c(x,y), xlim=xlim, ylim=ylim))
}

.typeMap <- list(gene=list(constructor=.newGeneDevice,
                           class="EpivizGeneDevice",
                           description="Scatterplot indexed by probeid",
                           input_class="ExpressionSet"),
              bp=list(constructor=.newBpDevice,
                      class="EpivizBpDevice",
                      description="Basepair resolution line plot",
                      input_class="GRanges"),
              block=list(constructor=.newBlockDevice,
                         class="EpivizBlockDevice",
                         description="Region plot",
                         input_class="GRanges"))

newDevice <- function(obj, id, type="block",...)                      
{
  if (!type %in% names(.typeMap))
    stop("Unknown device type")
  obj <- .typeMap[[type]]$constructor(obj, id, ...)
  obj$makeTree()
  return(obj)
}
