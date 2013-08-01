context("register measurement")

openBrowser=sendRequest

test_that("register measurement works for block", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  dev <- epivizr::register(gr)
  expect_true(validObject(dev))

  expect_is(dev,"EpivizDevice")
  expect_is(dev, "EpivizBlockDevice")
  expect_is(dev$object, "GenomicRanges")
  expect_is(dev$tree, "GIntervalTree")
  
  expect_equal(dev$object, gr)
  expect_equal(as(dev$tree, "GRanges"), unname(gr))
  expect_equal(seqnames(dev$tree), seqnames(gr))
})

test_that("register works for bp data", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1),score=rnorm(10))
  dev <- epivizr::register(gr, columns="score", type="bp")
  expect_true(validObject(dev))

  expect_is(dev,"EpivizDevice")
  expect_is(dev, "EpivizBpDevice")
  expect_is(dev$object, "GenomicRanges")
  expect_is(dev$tree, "GIntervalTree")
  
  expect_equal(dev$object, gr)
  expect_equal(as(dev$tree, "GRanges"), unname(gr))
  expect_equal(seqnames(dev$tree), seqnames(gr))
  expect_equal(dev$columns, "score")

  rng=range(pretty(range(gr$score)))
  expect_equal(dev$ylim, cbind(score=rng))
})

test_that("register works for ExpressionSet", {
  eset <- makeEset()
  dev <- epivizr::register(eset, columns=c("SAMP_1", "SAMP_2"))
  expect_true(validObject(dev))

  expect_is(dev,"EpivizDevice")
  expect_is(dev, "EpivizFeatureDevice")
  expect_is(dev$object, "GRanges")
  expect_is(dev$tree, "GIntervalTree")

  m <- match(dev$object$PROBEID, featureNames(eset))
  expect_equal(exprs(eset)[m,"SAMP_1"], dev$object$SAMP_1)
  expect_equal(exprs(eset)[m,"SAMP_2"], dev$object$SAMP_2)

  rngs <- apply(exprs(eset)[m,c("SAMP_1","SAMP_2")], 2, function(x) range(pretty(range(x))))
  expect_equal(dev$ylim, rngs, check.attributes=FALSE)
})

test_that("register works for SummarizedExperiment", {
	sset <- makeSExp()
	dev <- epivizr::register(sset, columns=c("A","B"), assay=2)
	expect_true(validObject(dev))

	expect_is(dev, "EpivizDevice")
	expect_is(dev, "EpivizFeatureDevice")
	expect_is(dev$object, "GRanges")
	expect_is(dev$tree, "GIntervalTree")

	mat <- assay(sset,"counts2")[,c("A","B")]
	expect_equal(mat[,"A"], dev$object$A)
	expect_equal(mat[,"B"], dev$object$B)
	rngs <- apply(mat, 2, function(x) range(pretty(range(x))))
	expect_equal(dev$ylim, rngs, check.attributes=FALSE)
})
