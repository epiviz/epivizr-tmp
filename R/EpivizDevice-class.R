EpivizDevice <- setRefClass("EpivizDevice",
	fields=list(
		msObject="EpivizData",
		chartObject="EpivizChart"
	),
	methods=list(
		getMsObject=function() {msObject},
		getChartObject=function() {chartObject},
		getMsId=function() {msObject$getId()},
		getChartId=function() {chartObject$getId()}
	)
)