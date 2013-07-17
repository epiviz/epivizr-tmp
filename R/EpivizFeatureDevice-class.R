EpivizFeatureDevice <- setRefClass("EpivizFeatureDevice",
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
