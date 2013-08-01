EpivizBpDevice <- setRefClass("EpivizBpDevice",
  contains="EpivizTrackDevice",
  methods=list(
    getData=function(chr, start, end, columnsRequested) {
      out <- callSuper(chr,start,end,columnsRequested)
      for (i in seq_along(columnsRequested)) {
          origData <- out$data[[i]]
          out$data[[i]] <- list(bp=origData$start, value=origData$value)
      }
      return(out)
    }
  )
)
