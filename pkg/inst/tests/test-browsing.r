context("browser commands")

test_that("refresh works", {
  mgr <- startEpiviz()
  mgr$refresh()
  mgr$stop()
})

test_that("navigate works", {
  mgr <- startEpiviz()
  mgr$navigate(chr="chr10", start=2000, end=10000)
  mgr$stop()
})