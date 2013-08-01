EpivizFeatureDevice <- setRefClass("EpivizFeatureDevice",
  contains="EpivizDevice",
  methods=list(
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
)
