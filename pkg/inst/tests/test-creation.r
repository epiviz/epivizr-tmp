context("object creation")

test_that("stop shuts down the socket connection", {
  mgr=.startMGR()
  mgr$stop()
  expect_true(mgr$isClosed())
})

test_that("startEpiviz creates a proper object", {
  mgr <- .startMGR()
  expect_is(mgr, "EpivizDeviceMgr")
  
  expect_is(mgr$devices, "list")
  expect_equal(length(mgr$devices), 3)
  
  expect_is(mgr$devices$gene, "list")
  expect_equal(length(mgr$devices$gene), 0)
  
  expect_is(mgr$devices$bp, "list")
  expect_equal(length(mgr$devices$bp), 0)
  
  expect_is(mgr$devices$block, "list")
  expect_equal(length(mgr$devices$block), 0)
  
  expect_is(mgr$server, "environment")
  expect_equal(mgr$activeId, "")
  expect_equal(mgr$chartIdMap, list())
  
  expect_false(mgr$isClosed())
  mgr$stop()
})

test_that("create device works for block", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  dev <- epivizr::newDevice(gr)
  expect_is(dev,"EpivizDevice")
  expect_is(dev, "EpivizBlockDevice")
  expect_is(dev$gr, "GIntervalTree")
  expect_equal(ranges(dev$gr), ranges(gr))
  expect_equal(seqnames(dev$gr), seqnames(gr))
  expect_equal(as(dev$gr, "GRanges"), gr)
})

test_that("create device works for bp data", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1),score=rnorm(10))
  dev <- epivizr::newDevice(gr, type="bp", mdCols="score")
  expect_is(dev,"EpivizDevice")
  expect_is(dev, "EpivizBpDevice")
  expect_is(dev$gr, "GIntervalTree")
  expect_equal(as(dev$gr, "GRanges"), gr)
  expect_equal(dev$mdCols, "score")
  })

test_that("create device works for gene data", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=50),score=rnorm(10))
  dev <- epivizr::newDevice(gr, type="gene", mdCols="score")
  expect_is(dev,"EpivizDevice")
  expect_is(dev, "EpivizGeneDevice")
  expect_is(dev$gr, "GIntervalTree")
  expect_equal(as(dev$gr, "GRanges"), gr)
  expect_equal(dev$mdCols, "score")
})