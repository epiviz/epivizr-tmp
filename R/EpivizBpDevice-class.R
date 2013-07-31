EpivizBpDevice <- setRefClass("EpivizBpDevice",
  contains="EpivizTrackDevice",
  methods=list(
    # getData=function(chr, start, end, columns) {
    #   nCols=length(cols)
    #   out=list(min=unname(.self$ylim[1,]),
    #            max=unname(.self$ylim[2,]),
    #            data=vector("list",nCols))
    #   for (i in seq_along(cols)) {
    #     out$data[[i]]=list(bp=integer(), value=numeric())
    #   }
      
    #   ogr=.self$subsetByOverlaps(chr,start,end)
    #   if (length(ogr)<1) {
    #     return(out)  
    #   }
      
    #   bp=start(ogr)
      
    #   for (i in seq_along(cols)) {
    #     vals=mcols(ogr)[[cols[i]]]
    #     if (all(is.na(vals))) {
    #       next
    #     }
    #     naIndex=is.na(vals)
    #     if (any(naIndex)) {
    #       out$data[[i]]=list(bp=bp[!naIndex], value=vals[!naIndex])
    #       vals=vals[!naIndex]
    #     } else {
    #       out$data[[i]]=list(bp=bp,value=vals)
    #     }
    #     # rng=range(pretty(range(vals)))
    #     # out$min[i]=rng[1]
    #     # out$max[i]=rng[2]
    #   }
    #   return(out)
    # },
    # update=function(gr=gr,mdCols=NULL) {
    #   callSuper(gr=gr)
    #   if (!is.null(mdCols)) {
    #     if (!all(mdCols %in% mcols(gr))) {
    #       stop("invalid mdCols specified")
    #     }
    #     mdCols <<- mdCols
    #   }
    #   invisible()
    # }
  )
)
