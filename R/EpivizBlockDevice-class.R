EpivizBlockDevice <- setRefClass("EpivizBlockDevice",
  contains="EpivizTrackDevice",
  methods=list(
  	initialize=function(object=GIntervalTree(GRanges()), ...) {
  		callSuper(object=object, ...)
  		columns <<- NULL
  	}
  )
)

.valid.EpivizBlockDevice.ylim <- function(x) {
	if (!is.null(x$ylim))
		return("'ylim' must be 'NULL'")
	NULL
}

.valid.EpivizBlockDevice <- function(x) {
	c(.valid.EpivizBlockDevice.ylim(x))
}

IRanges::setValidity2("EpivizBlockDevice", .valid.EpivizBlockDevice)
