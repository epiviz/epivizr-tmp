context("browser commands")
localURL="http://localhost/~hcorrada/epiviz"

test_that("refresh works", {
  tryCatch({
    mgr <- startEpiviz(localURL=localURL,debug=TRUE)
    mgr$refresh()
  }, finally=mgr$stop())
})

test_that("navigate works", {
  tryCatch({
    mgr <- startEpiviz(localURL=localURL,debug=TRUE)
    mgr$navigate(chr="chr10", start=2000, end=10000)
  }, finally=mgr$stop())
})