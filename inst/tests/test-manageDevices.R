context("manage devices")

sendRequest = getOption("epivizrTestSendRequest")

test_that("rmDevice works", {
  sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  
  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
    devObj <- mgr$addDevice(gr, "dev1", sendRequest=sendRequest, type="bp")
    devId <- devObj$getId()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    mgr$rmDevice(devObj)

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    expect_true(all(sapply(mgr$msList, length)==0))
    expect_true(is.null(mgr$msList$bp[[devObj$getMsId()]]))

    expect_equal(length(mgr$chartList), 0)
    expect_true(is.null(mgr$chartList[[devObj$getChartId()]]))

    expect_true(length(mgr$deviceList)==0)
    expect_true(is.null(mgr$deviceList[[devObj$getId()]]))

    devObj <- mgr$addDevice(gr, "dev1", sendRequest=sendRequest, type="bp")
    devId <- devObj$getId()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    mgr$rmDevice(devId)

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    expect_true(all(sapply(mgr$msList, length)==0))
    expect_true(is.null(mgr$msList$bp[[devObj$getMsId()]]))

    expect_equal(length(mgr$chartList), 0)
    expect_true(is.null(mgr$chartList[[devObj$getChartId()]]))

    expect_true(length(mgr$deviceList)==0)
    expect_true(is.null(mgr$deviceList[[devObj$getId()]]))
  },finally=mgr$stopServer())
})

test_that("listDevices works", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  eset <- makeEset()

  mgr <- .startMGR(openBrowser=sendRequest)
  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
    msObj1 <- mgr$addMeasurements(gr1, "dev1", sendRequest=sendRequest); msId1 <- msObj1$getId()
    msObj2 <- mgr$addMeasurements(gr2, "dev2", sendRequest=sendRequest); msId2 <- msObj2$getId()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    chart1 <- msObj1$plot(sendRequest=sendRequest); chartId1=chart1$getId()
    chart2 <- msObj2$plot(sendRequest=sendRequest); chartId2=chart2$getId()


    dev3 <- mgr$addDevice(gr3, "dev3", sendRequest=sendRequest, type="bp"); devId3 <- dev3$getId()
    dev4 <- mgr$addDevice(eset, "dev4", sendRequest = sendRequest, columns=c("SAMP_1", "SAMP_2")); devId4 <- dev4$getId()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    devDF <- mgr$listDevices()
    
    ids <- c(devId3,devId4)
    if (sendRequest) {
      expect_false(any(sapply(mgr$deviceList[ids], is.null)))
    }

    type <- c("lineTrack", "geneScatterPlot")
    ms <- c(paste0(dev3$getMsId(), "__score"),
            paste0(dev4$getMsId(), "__SAMP_", 1:2, collapse=","))
    connected <- if (sendRequest) rep("*", 2) else rep("", 2)
    expected_df <- data.frame(id=ids,
                              type=type,
                              measurements=ms,
                              connected=connected,
                              stringsAsFactors=FALSE)

    expect_equal(devDF, expected_df)
  }, finally=mgr$stopServer())
})

test_that("rmAllDevices works", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  eset <- makeEset()

  mgr <- .startMGR(openBrowser=sendRequest)
  tryCatch({
    if (sendRequest) wait_until(mgr$server$socketConnected)
    msObj1 <- mgr$addMeasurements(gr1, "dev1", sendRequest=sendRequest); msId1 <- msObj1$getId()
    msObj2 <- mgr$addMeasurements(gr2, "dev2", sendRequest=sendRequest); msId2 <- msObj2$getId()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    chart1 <- msObj1$plot(sendRequest=sendRequest); chartId1=chart1$getId()
    chart2 <- msObj2$plot(sendRequest=sendRequest); chartId2=chart2$getId()


    dev3 <- mgr$addDevice(gr3, "dev3", sendRequest=sendRequest, type="bp"); devId3 <- dev3$getId()
    dev4 <- mgr$addDevice(eset, "dev4", sendRequest = sendRequest, columns=c("SAMP_1", "SAMP_2")); devId4 <- dev4$getId()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    msDF <- mgr$listMeasurements()
    chartDF <- mgr$listCharts()    
    # devsDF <- mgr$listDevices()
  
    expected_msDF <- list(gene=data.frame(id=dev4$getMsId(),
                             name="dev4",
                             length=length(dev4$getMsObject()$object),
                             connected=ifelse(sendRequest,"*",""),
                             columns=paste0("SAMP_",1:2,collapse=","),
                             stringsAsFactors=FALSE),
                        bp=data.frame(id=dev3$getMsId(),
                                      name="dev3",
                                      length=length(gr3),
                                      connected=ifelse(sendRequest,"*",""),
                                      columns="score",
                                      stringsAsFactors=FALSE),
                        block=data.frame(id=c(msId1,msId2),
                              name=c("dev1","dev2"),
                              length=c(length(gr1),length(gr2)),
                              connected=ifelse(sendRequest,c("*","*"),c("","")),
                              columns=c("",""),
                              stringsAsFactors=FALSE)
                        )
    expect_equal(msDF, expected_msDF)

    ids <- c(chart1$getId(), chart2$getId(), dev3$getChartId(), dev4$getChartId())
    type <- c("blocksTrack", "blocksTrack", "lineTrack", "geneScatterPlot")
    ms <- c(msId1, msId2, 
            paste0(dev3$getMsId(), "__score"),
            paste0(dev4$getMsId(), "__SAMP_", 1:2, collapse=","))
    connected <- if (sendRequest) rep("*", 4) else rep("", 4)
    expected_chartDF <- data.frame(id=ids,
                              type=type,
                              measurements=ms,
                              connected=connected,
                              stringsAsFactors=FALSE)

    expect_equal(chartDF, expected_chartDF)
    mgr$rmAllDevices()

    if (sendRequest) wait_until(!mgr$server$requestWaiting)
    expect_true(length(mgr$deviceList) == 0)
    expect_true(length(mgr$msList$block)==2)
    expect_true(length(mgr$msList$bp)==0)
    expect_true(length(mgr$msList$gene)==0)
    expect_true(is.null(mgr$msList$gene[[dev4$getMsId()]]))
    expect_true(is.null(mgr$msList$bp[[dev3$getMsId()]]))
    expect_false(is.null(mgr$msList$block[[msId1]]))
    expect_false(is.null(mgr$msList$block[[msId2]]))

  }, finally=mgr$stopServer())
})
