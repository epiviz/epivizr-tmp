EpivizFeatureDevice <- setRefClass("EpivizFeatureDevice",
  contains="EpivizDevice",
  fields=list(assay="ANY"),
  methods=list(
    initialize=function(assay=1, ...) {
      assay <<- assay
      callSuper(...)
    },
    .checkColumns=function(columns) {
      all(columns %in% rownames(colData(object)))
    },
    .getColumns=function() {
      rownames(colData(object))
    },
    .getLimits=function() {
      mat <- GenomicRanges::assay(object, i=assay)
      colIndex <- match(columns, rownames(colData(object)))
      sapply(seq(along=columns), function(i) range(pretty(range(mat[,i]))))
    }
  )
)

.valid.EpivizFeatureDevice.object <- function(x) {
  if(!is(x$object, "SummarizedExperiment"))
    return("'object' must be of class 'SummarizedExperiment'")
  if(!is(rowData(x$object), "GIntervalTree"))
    return("'rowData(object)' must be of class 'GIntervalTree'")
  NULL
}

.valid.EpivizFeatureDevice.ylim <- function(x) {
  if(!is(x$ylim, "matrix"))
    return("'ylim' must be a matrix")
  if(nrow(x$ylim) != 2)
    return("'ylim' must have two rows")
  if(ncol(x$ylim) != length(x$columns))
    return("'ylim' must have 'length(columns)' columns")
  NULL
}

.valid.EpivizFeatureDevice.assay <- function(x) {
  if (is.character(x$assay)) {
    if(!(x$assay %in% names(assays(x$object))))
      return("'assay' not found in 'object'")
    return(NULL)
  }

  if (x$assay > length(assays(x$object)))
    return("'assay' not found in 'object'")
  NULL
}

.valid.EpivizFeatureDevice <- function(x) {
  c(.valid.EpivizFeatureDevice.object(x),
    .valid.EpivizFeatureDevice.ylim(x),
    .valid.EpivizFeatureDevice.assay(x))
}

IRanges::setValidity2("EpivizFeatureDevice", .valid.EpivizFeatureDevice)

EpivizFeatureDevice$methods(
    getMeasurements=function(devName, devId) {
     out <- paste(devName, columns, sep="$")
      nms <- paste(devId, columns, sep="$")
      names(out) <- nms
      out
    },
    getData=function(chr, start, end, columnsRequested) {
      columnsMatch <- match(columnsRequested, columns)
      if (any(is.na(columnsMatch)))
        stop("'columnsRequested' not found in device 'columns'")

      nCols=length(cols)
      out=list(min=unname(.self$ylim[1,columnsMatch]),
               max=unname(.self$ylim[2,columnsMatch]),
               data=list(gene=character(),
                         start=integer(),
                         end=integer(),
                         probe=character()))
      for (i in seq_along(cols)) {
        out$data[[i+4]]=numeric()
      }
      
      sobject = .self$subsetByOverlaps(chr,start,end)
      if (length(sobject)<1) {
        return(out)  
      }
      
      out$data$gene=sobject$SYMBOL
      out$data$start=start(sobject)
      out$data$end=end(sobject)
      out$data$probe=sobject$PROBEID

      for (i in seq_along(columnsRequested)) {
        vals=unname(mcols(sobject)[[columnsRequested[i]]])
        if (all(is.na(vals))) {
          next
        }
        out$data[[i+4]]=vals
        naIndex=is.na(vals)
        if (any(naIndex)) 
          out$data[[i+4]]=vals[!naIndex]
      }
      return(out)
    }
  )

