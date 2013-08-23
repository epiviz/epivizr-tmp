EpivizBpDevice <- setRefClass("EpivizBpDevice",
  contains="EpivizTrackDevice",
  methods=list(
    .checkColumns=function(columns) {
      all(columns %in% names(mcols(object)))
    },
    .getColumns=function() {
      names(mcols(object))
    },
    .getLimits=function() {
      sapply(mcols(object)[columns], function(x) range(pretty(range(x))))
    }
  )
)

.valid.EpivizBpDevice.ylim <- function(x) {
  if(!is(x$ylim, "matrix"))
    return("'ylim' must be a matrix")
  if(nrow(x$ylim) != 2)
    return("'ylim' must have two rows")
  if(ncol(x$ylim) != length(x$columns))
    return("'ylim' must have 'length(columns)' columns")
  NULL
}

.valid.EpivizBpDevice <- function(x) {
  c(.valid.EpivizBpDevice.ylim(x))
}

IRanges::setValidity2("EpivizBpDevice", .valid.EpivizBpDevice)

EpivizBpDevice$methods(
  getMeasurements=function(devName, devId) {
    out <- paste(devName, columns, sep="$")
    nms <- paste(devId, columns, sep="$")
    names(out) <- nms
    out
  },
  getData=function(chr, start, end, columnsRequested) {
      out <- callSuper(chr,start,end,columnsRequested)
      for (i in seq_along(columnsRequested)) {
          origData <- out$data[[i]]
          out$data[[i]] <- list(bp=origData$start, value=origData$value)
      }
      return(out)
    }
)

