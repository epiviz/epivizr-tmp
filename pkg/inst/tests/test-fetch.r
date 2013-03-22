context("data fetch")

test_that("device data fetch works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1),
                 seqinfo=Seqinfo(seqnames="chr1",genome="hcb"))
  dev1 <- epivizr::newDevice(gr1)
  
  res <- dev1$getData(chr="chr1", start=2, end=6)
  out <- list(start=2:6,end=2:6)
  expect_equal(res,out)
})

test_that("mgr fetch works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100),
                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100),
                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=5), width=1), score=seq(1,100,by=5),
                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  
  tryCatch({
    mgr <- .startMGR()
  
    devId1 <- mgr$addDevice(epivizr::newDevice(gr1), "dev1")
    devId2 <- mgr$addDevice(epivizr::newDevice(gr2), "dev2")
    devId3 <- mgr$addDevice(epivizr::newDevice(gr3, type="bp"), "dev3")
  
    measurements=list(bpMeasurements=paste0(devId3,"$score"),blockMeasurements=c(devId1,devId2))
    res <- mgr$getData(measurements, chr="chr1", start=2, end=6)
    out <- list(chr="chr1",start=2,end=6)
    out$bpData=list(start=2,end=6,chr="chr1")
    out$bpData$min=structure(c(5),names=paste0(devId3,"$","score"))
    out$bpData$max=structure(c(10),names=paste0(devId3,"$","score"))
    out$bpData$data=structure(list(list(bp=6,value=6)),names=paste0(devId3,"$","score"))
    
    out$blockData=list(start=2,end=6,chr="chr1")
    out$blockData$data=structure(list(list(start=1:6, end=100:105),
                                      list(start=integer(), end=integer())),
                                 names=c(devId1,devId2))
    
    #print(res);print(out)
    expect_equal(res,out)
  }, finally=mgr$stop())
})

test_that("mgr fetch no data works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100),
                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100),
                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=5), width=1), score=seq(1,100,by=5),
                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  
  tryCatch({
    mgr <- .startMGR()
    
    devId1 <- mgr$addDevice(epivizr::newDevice(gr1), "dev1")
    devId2 <- mgr$addDevice(epivizr::newDevice(gr2), "dev2")
    devId3 <- mgr$addDevice(epivizr::newDevice(gr3, type="bp"), "dev3")
    
    measurements=list(bpMeasurements=paste0(devId3,"$score"),blockMeasurements=c(devId1,devId2))
    res <- mgr$getData(measurements, chr="chr11", start=2, end=6)
    
    out <- list(chr="chr11",start=2,end=6)
    out$bpData=list(start=2,end=6,chr="chr11")
    out$bpData$min=structure(c(-6),names=paste0(devId3,"$","score"))
    out$bpData$max=structure(c(6),names=paste0(devId3,"$","score"))
    out$bpData$data=structure(list(list(bp=integer(),value=numeric())),names=paste0(devId3,"$","score"))
    
    out$blockData=list(start=2,end=6,chr="chr11")
    out$blockData$data=structure(list(list(start=integer(), end=integer()),
                                      list(start=integer(), end=integer())),
                                 names=c(devId1,devId2))
    
    #print(res);print(out)
    expect_equal(res,out)
  }, finally=mgr$stop())
})