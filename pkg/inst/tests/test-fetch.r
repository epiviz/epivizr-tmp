context("data fetch")
localURL="http://localhost/~hcorrada/epiviz"

test_that("device data fetch works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1),
                 seqinfo=Seqinfo(seqnames="chr1",genome="hcb"))
  dev1 <- epivizr::newDevice(gr1)
  
  res <- dev1$getData(chr="chr1", start=2, end=6)
  out <- GRanges(seqnames="chr1", ranges=IRanges(start=2:6,width=1),
                 seqinfo=Seqinfo(seqnames="chr1",genome="hcb"))
  expect_equal(res,out)
})

test_that("mgr devId fetch works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1),
                 seqinfo=Seqinfo(seqnames="chr1",genome="hcb"))
  
  tryCatch({
    mgr <- startEpiviz(localURL=localURL,debug=TRUE)
    devId <- mgr$addDevice(epivizr::newDevice(gr1), "dev1")
  
    res <- mgr$getData(devId, chr="chr1", start=2, end=6)
    out <- list(GRanges(seqnames="chr1", ranges=IRanges(start=2:6,width=1),
                      seqinfo=Seqinfo(seqnames="chr1",genome="hcb")))
    names(out)=devId
    expect_equal(res,out)
  }, finally=mgr$stop())
})

test_that("mgr no id fetch works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1),
                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=1),
                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  
  tryCatch({
    mgr <- startEpiviz(localURL=localURL,debug=TRUE)
  
    devId1 <- mgr$addDevice(epivizr::newDevice(gr1), "dev1")
    devId2 <- mgr$addDevice(epivizr::newDevice(gr2), "dev2")
  
    res <- mgr$getData(NULL, chr="chr1", start=2, end=6)
    out <- list(block=list(GRanges(seqnames="chr1", ranges=IRanges(start=2:6,width=1),
                      seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb")),
              GRanges(seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))))
    names(out$block)=c(devId1,devId2)
    expect_equal(res,out)
  }, finally=mgr$stop())
})