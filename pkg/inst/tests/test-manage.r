context("device management")

test_that("addDevice works", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  dev <- epivizr::newDevice(gr)
  mgr <- startEpiviz()
  devId <- mgr$addDevice(dev, "dev1")
  
  expect_equal(length(mgr$devices), 1)
  expect_false(is.null(mgr$devices[[devId]]))
  expect_equal(mgr$devices[[devId]]$name, "dev1")
  expect_equal(mgr$devices[[devId]]$obj$gr, gr)
  expect_equal(mgr$activeId, devId)
  mgr$stop()
})

test_that("delDevice works", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  dev <- epivizr::newDevice(gr)
  mgr <- startEpiviz()
  devId <- mgr$addDevice(dev, "dev1")
  mgr$delDevice(devId)
  
  expect_equal(length(mgr$devices), 0)
  expect_true(is.null(mgr$devices[[devId]]))
  expect_equal(mgr$activeId, "")
  mgr$stop()
})

test_that("listDevice works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=1))
  
  mgr <- startEpiviz()
  
  dev1 <- epivizr::newDevice(gr1)
  dev2 <- epivizr::newDevice(gr2)
  
  devId1 <- mgr$addDevice(dev1, "dev1")
  devId2 <- mgr$addDevice(dev2, "dev2")
  
  devs <- mgr$listDevices()
  expected_df <- data.frame(id=c(devId1,devId2),
                            active=c("","*"),
                            name=c("dev1","dev2"),
                            length=c(length(gr1),length(gr2)),
                            stringsAsFactors=FALSE)
  expect_equal(devs, expected_df)
  mgr$stop()
})

test_that("setActive works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=1))
  
  mgr <- startEpiviz()
  
  dev1 <- epivizr::newDevice(gr1)
  dev2 <- epivizr::newDevice(gr2)
  
  devId1 <- mgr$addDevice(dev1, "dev1")
  devId2 <- mgr$addDevice(dev2, "dev2")
  
  mgr$setActive(devId1)
  devs <- mgr$listDevices()
  expected_df <- data.frame(id=c(devId1,devId2),
                            active=c("*",""),
                            name=c("dev1","dev2"),
                            length=c(length(gr1),length(gr2)),
                            stringsAsFactors=FALSE)
  expect_equal(devs, expected_df)
  mgr$stop()
})
