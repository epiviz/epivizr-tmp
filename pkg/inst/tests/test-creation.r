test_that("stop shuts down the socket connection", {
  mgr=startEpiviz()
  mgr$stop()
  expect_true(mgr$isClosed())
})

test_that("startEpiviz creates a proper object", {
  mgr <- startEpiviz()
  expect_is(mgr, "EpivizDeviceMgr")
  expect_is(mgr$devices, "list")
  expect_equal(length(mgr$devices), 0)
  expect_is(mgr$server, "environment")
  expect_false(mgr$isClosed())
  mgr$stop()
})

test_that("create device works", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  dev <- epivizr::newDevice(gr)
  expect_is(dev,"EpivizDevice")
  expect_is(dev$gr, "GenomicRanges")
  expect_equal(dev$gr, gr)
  expect_true(is.null(dev$mdCols))
  expect_equal(dev$minValue, -6)
  expect_equal(dev$maxValue, 6)
})

test_that("addDevice works", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  dev <- epivizr::newDevice(gr)
  mgr <- startEpiviz()
  devId <- mgr$addDevice(dev, "dev1")
  
  expect_equal(length(mgr$devices), 1)
  expect_false(is.null(mgr$devices[[devId]]))
  expect_equal(mgr$devices[[devId]]$name, "dev1")
  expect_equal(mgr$devices[[devId]]$obj$gr, gr)
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
                            name=c("dev1","dev2"),
                            length=c(length(gr1),length(gr2)),
                            stringsAsFactors=FALSE)
  expect_equal(devs, expected_df)
  mgr$stop()
})