context("addCharts")

sendRequest=sendRequest

test_that("blockChart works", {
	sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    msObj <- mgr$addMeasurements(gr, "ms1", sendRequest=sendRequest)
    msId <- msObj$getId()

    chartObj <- mgr$blockChart(msId, sendRequest=sendRequest)
    chartId <- chartObj$getId()

    expect_is(chartObj, "EpivizChart")
    expect_equal(chartObj$measurements, msId)
    expect_equal(chartObj$type, "blocksTrack")
    expect_false(is.null(mgr$chartList[[chartId]]))

    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("plot block works", {
	sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    msObj <- mgr$addMeasurements(gr, "ms1", sendRequest=sendRequest)
    msId <- msObj$getId()

	chartObj <- msObj$plot(sendRequest=sendRequest)
    chartId <- chartObj$getId()

    expect_is(chartObj, "EpivizChart")
    expect_equal(chartObj$measurements, msId)
    expect_equal(chartObj$type, "blocksTrack")
    expect_false(is.null(mgr$chartList[[chartId]]))

    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("addDevice block works", {
	sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
  	devObj <- mgr$addDevice(gr, "ms1", sendRequest=sendRequest)
  	expect_is(devObj, "EpivizDevice")

  	msId <- devObj$getMsId()
    chartId <- devObj$getChartId()

    chartObj <- devObj$getChartObject()
    expect_equal(chartObj$measurements, msId)
    expect_equal(chartObj$type, "blocksTrack")
    expect_false(is.null(mgr$chartList[[chartId]]))

    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("lineChart works", {
	sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    msObj <- mgr$addMeasurements(gr, "ms1", sendRequest=sendRequest, type="bp")
    msId <- msObj$getId()

    chartObj <- mgr$lineChart(paste0(msId,"$score2"), sendRequest=sendRequest)
    chartId <- chartObj$getId()

    expect_equal(chartObj$measurements, paste0(msId,"$score2"))
    expect_equal(chartObj$type, "lineTrack")
    expect_false(is.null(mgr$chartList[[chartId]]))

    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("plot bp works", {
	sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    msObj <- mgr$addMeasurements(gr, "ms1", sendRequest=sendRequest, type="bp")
    msId <- msObj$getId()

    chartObj <- msObj$plot(sendRequest=sendRequest)
    chartId <- chartObj$getId()

    ms <- paste0(msId,"$score",1:2)
    expect_equal(chartObj$measurements, ms)
    expect_equal(chartObj$type, "lineTrack")
    expect_false(is.null(mgr$chartList[[chartId]]))

    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("addDevice bp works", {
	sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
  	devObj <- mgr$addDevice(gr, "ms1", sendRequest=sendRequest, type="bp")
  	expect_is(devObj, "EpivizDevice")

    msId <- devObj$getMsId()
    chartId <- devObj$getChartId()

    ms <- paste0(msId,"$score",1:2)
    chartObj <- devObj$getChartObject()
    expect_equal(chartObj$measurements, ms)
    expect_equal(chartObj$type, "lineTrack")
    expect_false(is.null(mgr$chartList[[chartId]]))

    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("scatterChart works", {
	sendRequest=sendRequest
  sset <- makeSExp()
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    msObj <- mgr$addMeasurements(sset, "ms1", sendRequest=sendRequest, columns=c("A","B"), assay="counts2")
    msId <- msObj$getId()

    chartObj <- mgr$scatterChart(x=paste0(msId,"$A"), y=paste0(msId,"$B"),sendRequest=sendRequest)
    chartId <- chartObj$getId()

    expect_equal(chartObj$measurements, paste0(msId,"$",c("A","B")))
    expect_equal(chartObj$type, "geneScatterPlot")
    expect_false(is.null(mgr$chartList[[chartId]]))

    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("plot feature works", {
	sendRequest=sendRequest
  sset <- makeSExp()
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
    msObj <- mgr$addMeasurements(sset, "ms1", sendRequest=sendRequest, columns=c("A","B"), assay="counts2")
    msId <- msObj$getId()

    chartObj <- msObj$plot(sendRequest=sendRequest)
    chartId <- chartObj$getId()

    expect_equal(chartObj$measurements, paste0(msId,"$",c("A","B")))
    expect_equal(chartObj$type, "geneScatterPlot")
    expect_false(is.null(mgr$chartList[[chartId]]))

    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})

test_that("addDevice feature works", {
	sendRequest=sendRequest
  sset <- makeSExp()
  mgr <- .startMGR(openBrowser=sendRequest)

  tryCatch({
  	devObj <- mgr$addDevice(sset, "ms1", sendRequest=sendRequest, columns=c("A","B"), assay="counts2")
	expect_is(devObj, "EpivizDevice")

    msId <- devObj$getMsId()
    chartId <- devObj$getChartId()

    chartObj <- devObj$getChartObject()
    expect_equal(chartObj$measurements, paste0(msId,"$",c("A","B")))
    expect_equal(chartObj$type, "geneScatterPlot")
    expect_false(is.null(mgr$chartList[[chartId]]))

    connected <- !is.null(mgr$chartIdMap[[chartId]])
    expect_equal(connected, sendRequest)
  }, finally=mgr$stopServer())
})