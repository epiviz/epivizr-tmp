context("device management")
localURL="http://localhost/~hcorrada/epiviz"

test_that("addDevice works for blocks", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  dev <- epivizr::newDevice(gr)
  mgr <- startEpiviz(localURL=localURL,debug=TRUE)
  
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
  tryCatch({
    mgr <- startEpiviz(localURL=localURL,debug=TRUE)
    devId <- mgr$addDevice(dev, "dev1")
    mgr$delDevice(devId)
  
    expect_equal(length(mgr$devices$block), 0)
    expect_true(is.null(mgr$devices$block[[devId]]))
    expect_equal(mgr$activeId, "")
    expect_equal(mgr$activeType, "")
  },finally=mgr$stop())
})

test_that("listDevice works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=1))
  
  tryCatch({
    mgr <- startEpiviz(localURL=localURL,debug=TRUE)
  
    dev1 <- epivizr::newDevice(gr1)
    dev2 <- epivizr::newDevice(gr2)
  
    devId1 <- mgr$addDevice(dev1, "dev1")
    devId2 <- mgr$addDevice(dev2, "dev2")
  
    devs <- mgr$listDevices()
    expected_df <- list(block=data.frame(id=c(devId1,devId2),
                            active=c("","*"),
                            name=c("dev1","dev2"),
                            length=c(length(gr1),length(gr2)),
                            stringsAsFactors=FALSE))
    expect_equal(devs, expected_df)
  }, finally=mgr$stop())
})

test_that("setActive works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=1))
  
  tryCatch({
    mgr <- startEpiviz(localURL=localURL,debug=TRUE)
  
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
