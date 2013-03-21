context("device management")

test_that("addDevice works for blocks", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  dev <- epivizr::newDevice(gr)
  mgr <- .startMGR()
  
  tryCatch({
    devId <- mgr$addDevice(dev, "dev1")
  
    expect_equal(length(mgr$devices$block), 1)
    expect_false(is.null(mgr$devices$block[[devId]]))
    expect_equal(mgr$devices$block[[devId]]$name, "dev1")
    expect_equal(mgr$devices$block[[devId]]$obj$gr, gr)
    expect_equal(mgr$activeId, devId)
    expect_equal(mgr$activeType, "block")
  }, finally=mgr$stop())
})

test_that("delDevice works", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  dev <- epivizr::newDevice(gr)
  mgr <- .startMGR()
  
  tryCatch({
    devId <- mgr$addDevice(dev, "dev1")
    mgr$delDevice(devId)
  
    expect_equal(length(mgr$devices$block), 0)
    expect_true(is.null(mgr$devices$block[[devId]]))
    expect_equal(mgr$activeId, "")
    expect_equal(mgr$activeType, "")
  },finally=mgr$stop())
})

test_that("listDevices works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  
  mgr <- .startMGR()
  tryCatch({
    dev1 <- epivizr::newDevice(gr1)
    dev2 <- epivizr::newDevice(gr2)
    dev3 <- epivizr::newDevice(gr3, type="bp")
    
    devId1 <- mgr$addDevice(dev1, "dev1")
    devId2 <- mgr$addDevice(dev2, "dev2")
    devId3 <- mgr$addDevice(dev3, "dev3")
    
    devs <- mgr$listDevices()
    expected_df <- list(bp=data.frame(id=devId3,
                                      active="*",
                                      name="dev3",
                                      length=length(gr3),
                                      stringsAsFactors=FALSE),
                        block=data.frame(id=c(devId1,devId2),
                              active=c("",""),
                              name=c("dev1","dev2"),
                              length=c(length(gr1),length(gr2)),
                              stringsAsFactors=FALSE)
                        )
    expect_equal(devs, expected_df)
  }, finally=mgr$stop())
})

test_that("setActive works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=1))
  
  mgr <- .startMGR()
  tryCatch({
    dev1 <- epivizr::newDevice(gr1)
    dev2 <- epivizr::newDevice(gr2)
  
    devId1 <- mgr$addDevice(dev1, "dev1")
    devId2 <- mgr$addDevice(dev2, "dev2")
  
    mgr$setActive(devId1)
    devs <- mgr$listDevices()
    expected_df <- list(block=data.frame(id=c(devId1,devId2),
                            active=c("*",""),
                            name=c("dev1","dev2"),
                            length=c(length(gr1),length(gr2)),
                            stringsAsFactors=FALSE))
    expect_equal(devs, expected_df)
  },finally=mgr$stop())
})

test_that("getMeasurements works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  
  mgr <- .startMGR()
  tryCatch({
    dev1 <- epivizr::newDevice(gr1)
    dev2 <- epivizr::newDevice(gr2)
    dev3 <- epivizr::newDevice(gr3, type="bp")
    
    devId1 <- mgr$addDevice(dev1, "dev1")
    devId2 <- mgr$addDevice(dev2, "dev2")
    devId3 <- mgr$addDevice(dev3, "dev3")
    
    res <- mgr$getMeasurements()
    out <- list(bpMeasurements="dev3", blockMeasurements=c("dev1","dev2"))
   
    expect_equal(res,out)
  }, finally=mgr$stop())
})