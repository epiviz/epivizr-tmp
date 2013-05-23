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

test_that("create device works for block", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  dev <- epivizr::newDevice(gr, id="test1")
  expect_is(dev,"EpivizDevice")
  expect_is(dev, "EpivizBlockDevice")
  expect_is(dev$gr, "GenomicRanges")
  expect_is(dev$tree, "IntervalForest")
  
  expect_equal(dev$gr, gr)
  expect_equal(as(dev$tree, "IRanges"), unname(ranges(gr)))
  expect_equal(dev$tree@partition, seqnames(gr))
})

test_that("create device works for bp data", {
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1),score=rnorm(10))
  dev <- epivizr::newDevice(gr, id="test1", type="bp", mdCols="score")
  expect_is(dev,"EpivizDevice")
  expect_is(dev, "EpivizBpDevice")
  expect_is(dev$gr, "GenomicRanges")
  expect_is(dev$tree, "IntervalForest")
  
  expect_equal(dev$gr, gr)
  expect_equal(as(dev$tree, "IRanges"), unname(ranges(gr)))
  expect_equal(dev$tree@partition, seqnames(gr))
  expect_equal(dev$mdCols, "score")
})

test_that("create device works for gene data", {
    require(hgu133plus2.db)
  nprobeids=1000
  nsamples=6
  expr=matrix(rnorm(nprobeids*nsamples), nr=nprobeids)
  pd=data.frame(a=1:nsamples,b=10*(1:nsamples))
  rownames(pd)=paste0("SAMP_",1:nsamples)
  rownames(expr)=head(keys(hgu133plus2.db, keytype="PROBEID"), nprobeids)
  colnames(expr)=rownames(pd)

  eset <- ExpressionSet(assayData=expr, phenoData=AnnotatedDataFrame(pd),annotation="hgu133plus2")
  dev <- epivizr::newDevice(eset, id = "test1", type="gene", x="SAMP_1", y="SAMP_2")

  expect_is(dev,"EpivizDevice")
  expect_is(dev, "EpivizGeneDevice")
  expect_is(dev$gr, "GRanges")
  expect_is(dev$tree, "IntervalForest")

  m <- match(dev$gr$PROBEID, featureNames(eset))
  expect_equal(exprs(eset)[m,"SAMP_1"], dev$gr$SAMP_1)
  expect_equal(exprs(eset)[m,"SAMP_2"], dev$gr$SAMP_2)
})