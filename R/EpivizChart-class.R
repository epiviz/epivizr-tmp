EpivizChart <- setRefClass("EpivizChart",
	fields=list(
		measurements="character",
		id="character",
		mgr="EpivizDeviceMgr",
		type="character"),
	methods=list(
		setId=function(id) {id <<- id},
		getId=function() {return(id)},
		show=function() {
			cat("EpivizChart object: ", getId(), "\n")
			cat("type: ", type, "\n")
			cat("measurements: ", paste0(measurements, collapse=","), "\n")
		}
	)
)

# chart methods
EpivizDeviceMgr$methods(
	blockChart=function(ms, ...) {
		if (!.self$.checkMeasurements(msType="block", ms=ms, ...))
			stop("invalid measurements")

		chartObj <- EpivizChart$new(
						measurements=ms,
						mgr=.self,
						type="blocksTrack")
		addChart(chartObj, ...)
		chartObj
	},

	lineChart=function(ms, ...) {
		if (!.self$.checkMeasurements(msType="bp", ms=ms, ...))
			stop("invalid measurements")

		chartObj <- EpivizChart$new(
					measurements=ms,
					mgr=.self,
					type="lineTrack")
		addChart(chartObj, ...)
		chartObj
	},

	scatterChart=function(x,y, ...) {
		ms <- c(x,y)

		if(!.self$.checkMeasurements(msType="gene", ms=ms, ...))
			stop("invalid measurements")

		chartObj <- EpivizChart$new(
					measurements=ms,
					mgr=.self,
					type="geneScatterPlot")
		addChart(chartObj, ...)
		chartObj
	}
)