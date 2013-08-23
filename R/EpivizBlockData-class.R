EpivizBlockData <- setRefClass("EpivizBlockData",
  contains="EpivizTrackData",
  methods=list(
  	initialize=function(object=GIntervalTree(GRanges()), ...) {
  		callSuper(object=object, ...)
  		columns <<- NULL
  	},
    plot=function(...) {
      mgr$blockChart(ms=names(getMeasurements())[1], ...)
    }
  )
)

.valid.EpivizBlockData.ylim <- function(x) {
	if (!is.null(x$ylim))
		return("'ylim' must be 'NULL'")
	NULL
}

.valid.EpivizBlockData <- function(x) {
	c(.valid.EpivizBlockData.ylim(x))
}

IRanges::setValidity2("EpivizBlockData", .valid.EpivizBlockData)
