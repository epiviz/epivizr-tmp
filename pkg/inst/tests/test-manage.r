context("device management")

test_that("addDevice works for blocks", {
  sendRequest=FALSE
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  mgr <- .startMGR()
  
  tryCatch({
    devId <- mgr$addDevice(gr, "dev1", sendRequest=sendRequest)
  
    if (sendRequest) {
      message("Press enter to continue")
      scan()
    }
    expect_equal(length(mgr$devices$block), 1)
    expect_false(is.null(mgr$devices$block[[devId]]))
    expect_equal(mgr$devices$block[[devId]]$name, "dev1")
    expect_equal(mgr$devices$block[[devId]]$measurements, devId)
    expect_equal(mgr$devices$block[[devId]]$obj$gr, gr)
    expect_equal(mgr$activeId, devId)
    
    if (sendRequest) {
      expect_false(is.null(mgr$chartIdMap[[devId]]))
    }
  }, finally=mgr$stop())
})

test_that("addDevice works for bp", {
  sendRequest=FALSE
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  mgr <- .startMGR()
  
  tryCatch({
    devId <- mgr$addDevice(gr, "dev1", sendRequest=sendRequest, type="bp")
    
    if (sendRequest) {
      message("Press enter to continue")
      scan()
    }
    expect_equal(length(mgr$devices$bp), 1)
    expect_false(is.null(mgr$devices$bp[[devId]]))
    expect_equal(mgr$devices$bp[[devId]]$name, "dev1")
    expect_equal(mgr$devices$bp[[devId]]$measurements, paste0(devId,"$","score",1:2))
    expect_equal(mgr$devices$bp[[devId]]$obj$gr, gr)
    expect_equal(mgr$devices$bp[[devId]]$obj$mdCols, paste0("score",1:2))
    expect_equal(mgr$activeId, devId)
    
    if (sendRequest) {
      expect_false(is.null(mgr$chartIdMap[[devId]]))
    }
  }, finally=mgr$stop())
})

test_that("rmDevice works", {
  sendRequest=TRUE
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  
  mgr <- .startMGR()
  
  tryCatch({
    devId <- mgr$addDevice(gr, "dev1", sendRequest=sendRequest, type="bp")
    if (sendRequest) {
      message("Press enter to continue")
      scan()
    }
    mgr$rmDevice(devId)
    if (sendRequest) {
      message("Press enter to continue")
      scan()
    }
  
    expect_equal(length(mgr$devices$block), 0)
    expect_true(is.null(mgr$devices$block[[devId]]))
    expect_equal(mgr$activeId, "")
  },finally=mgr$stop())
})

test_that("listDevices works", {
  sendRequest=FALSE
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  
  mgr <- .startMGR()
  tryCatch({
    devId1 <- mgr$addDevice(gr1, "dev1", sendRequest=sendRequest)
    devId2 <- mgr$addDevice(gr2, "dev2", sendRequest=sendRequest)
    devId3 <- mgr$addDevice(gr3, "dev3", sendRequest=sendRequest, type="bp")
    
    devs <- mgr$listDevices()
    expected_df <- list(bp=data.frame(id=devId3,
                                      active="*",
                                      name="dev3",
                                      length=length(gr3),
                                      connected="",
                                      stringsAsFactors=FALSE),
                        block=data.frame(id=c(devId1,devId2),
                              active=c("",""),
                              name=c("dev1","dev2"),
                              length=c(length(gr1),length(gr2)),
                              connected=c("",""),
                              stringsAsFactors=FALSE)
                        )
    expect_equal(devs, expected_df)
  }, finally=mgr$stop())
})

test_that("setActive works", {
  sendRequest=FALSE
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=1))
  
  mgr <- .startMGR()
  tryCatch({
    devId1 <- mgr$addDevice(gr1, "dev1", sendRequest=sendRequest)
    devId2 <- mgr$addDevice(gr2, "dev2", sendRequest=sendRequest)
  
    mgr$setActive(devId1)
    devs <- mgr$listDevices()
    expected_df <- list(block=data.frame(id=c(devId1,devId2),
                            active=c("*",""),
                            name=c("dev1","dev2"),
                            length=c(length(gr1),length(gr2)),
                            connected=c("",""),
                            stringsAsFactors=FALSE))
    expect_equal(devs, expected_df)
  },finally=mgr$stop())
})

test_that("getMeasurements works", {
  sendRequest=FALSE
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  
  mgr <- .startMGR()
  tryCatch({
    devId1 <- mgr$addDevice(gr1, "dev1", sendRequest=sendRequest)
    devId2 <- mgr$addDevice(gr2, "dev2", sendRequest=sendRequest)
    devId3 <- mgr$addDevice(gr3, "dev3", sendRequest=sendRequest, type="bp")
    
    res <- mgr$getMeasurements()
    out <- list(geneMeasurements=list(),
                bpMeasurements=structure(list("dev3$score"), names=paste0(devId3,"$score")), 
                blockMeasurements=structure(list("dev1","dev2"), names=c(devId1,devId2)))
   
    #print(res);print(out)
    expect_equal(res,out)
  }, finally=mgr$stop())
})
