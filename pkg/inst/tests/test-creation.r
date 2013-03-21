context("object creation")
localURL="http://localhost/~hcorrada/epiviz"

test_that("stop shuts down the socket connection", {
  mgr=startEpiviz(localURL=localURL,debug=TRUE)
  mgr$stop()
  expect_true(mgr$isClosed())
})

test_that("startEpiviz creates a proper object", {
  mgr <- startEpiviz(localURL=localURL,debug=TRUE)
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
  expect_equal(mgr$activeType, "")
  
  expect_false(mgr$isClosed())
  mgr$stop()
})

test_that("create device works", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  dev <- epivizr::newDevice(gr)
  expect_is(dev,"EpivizDevice")
  expect_is(dev, "EpivizBlockDevice")
  expect_is(dev$gr, "GenomicRanges")
  expect_equal(dev$gr, gr)
})
