EpivizTrackData <- setRefClass("EpivizTrackData",
  contains="EpivizData")

.valid.EpivizTrackData.object <- function(x) {
	if(!is(x$object, "GIntervalTree"))
		return("'object' is not a 'GIntervalTree' object")
	NULL
}

.valid.EpivizTrackData <- function(x) {
	c(.valid.EpivizTrackData.object(x))
}

IRanges::setValidity2("EpivizTrackData", .valid.EpivizTrackData)

EpivizTrackData$methods(
  	getData=function(chr, start, end, columnsRequested) {
  		if (is.null(columns))
  			return(callSuper(chr,start,end))

  		columnsMatch <- match(columnsRequested, columns)
  		if (any(is.na(columnsMatch)))
  			stop("'columnsRequested' not found in device 'columns'")

 		nCols=length(columnsRequested)
      	out=list(min=unname(.self$ylim[1,columnsMatch]),
        	     max=unname(.self$ylim[2,columnsMatch]),
            	 data=vector("list",nCols))

      	for (i in seq_along(columnsRequested)) {
        	out$data[[i]]=list(start=integer(), end=integer(), value=numeric())
	    }

  		sobject <- .self$subsetByOverlaps(chr, start, end)
		if (length(sobject)<1) {
	    	return(out)  
	  	}
      
	    for (i in seq_along(columnsRequested)) {
	      vals=mcols(sobject)[[columnsRequested[i]]]
	      if (all(is.na(vals))) {
	        next
	      }
	      out$data[[i]]=list(start=start(sobject), end=end(sobject), value=vals)
	      naIndex=is.na(vals)
	      if (any(naIndex)) {
	      	for (x in c("start", "end", "value"))
	      		out$data[[i]][[x]] <- out$data[[i]][[x]][!naIndex]
	      }
	    }
	    return(out)
  	}
  )


