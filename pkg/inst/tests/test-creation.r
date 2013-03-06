test_that("stop shuts down the socket connection", {
  mgr=startEpiviz()
  mgr$stop()
  expect_true(mgr$isClosed())
})
