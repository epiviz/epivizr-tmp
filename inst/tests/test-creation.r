context("object creation")

openBrowser=sendRequest

test_that("stop shuts down the server connection", {
  mgr=.startMGR(openBrowser=openBrowser)
  expect_equal(mgr$isClosed(), !openBrowser)
  
  mgr$stopServer()
  expect_true(mgr$isClosed())
})

test_that("startEpiviz creates a proper object", {
  mgr <- .startMGR(openBrowser)
  expect_is(mgr, "EpivizDeviceMgr")
  
  expect_is(mgr$devices, "list")
  expect_equal(length(mgr$devices), 3)
  
  expect_is(mgr$devices$gene, "list")
  expect_equal(length(mgr$devices$gene), 0)
  
  expect_is(mgr$devices$bp, "list")
  expect_equal(length(mgr$devices$bp), 0)
  
  expect_is(mgr$devices$block, "list")
  expect_equal(length(mgr$devices$block), 0)
  
  expect_is(mgr$server, "EpivizServer")
  expect_equal(mgr$activeId, "")
  expect_equal(mgr$chartIdMap, list())
  
  expect_equal(mgr$isClosed(), !openBrowser)
  mgr$stopServer()
})

