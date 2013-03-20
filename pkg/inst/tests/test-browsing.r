context("browser commands")
localURL="http://localhost/~hcorrada/epiviz"

test_that("refresh works", {
  mgr <- startEpiviz(localURL=localURL,debug=TRUE)
  mgr$refresh()
  mgr$stop()
})

test_that("navigate works", {
  mgr <- startEpiviz(localURL=localURL,debug=TRUE)
  mgr$navigate(chr="chr10", start=2000, end=10000)
  mgr$stop()
})