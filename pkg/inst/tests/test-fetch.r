context("data fetch")

sendRequest=sendRequest

test_that("device data fetch works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1),
                 seqinfo=Seqinfo(seqnames="chr1",genome="hcb"))
  dev1 <- epivizr::newDevice(gr1,id="testid")
  
  res <- dev1$getData(chr="chr1", start=2, end=6)
  out <- list(start=2:6,end=2:6)
  expect_equal(res,out)
})

test_that("device data fetch works on bp data", {
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=5), width=1), score1=seq(1,100,by=5), score2=-seq(1,100,by=5),
                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  
  dev1 <- epivizr::newDevice(gr3,id="testid",type="bp")
  
  res <- dev1$getData(chr="chr1", start=2, end=6,cols=c("score1","score2"))
  out=list()
  out$min=c(5,-10)
  out$max=c(10,-5)
  out$data=list(list(bp=6,value=6),list(bp=6,value=-6))

  expect_equal(res,out)
})

test_that("device data fetch works on bp data with NAs", {
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=5), width=1), score1=seq(1,100,by=5), score2=-seq(1,100,by=5),
                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  gr3$score2[1:10]=NA
  
  dev1 <- epivizr::newDevice(gr3,id="testid",type="bp")
  
  res <- dev1$getData(chr="chr1", start=2, end=6,cols=c("score1","score2"))
  out=list()
  out$min=c(5,-6)
  out$max=c(10,6)
  out$data=list(list(bp=6,value=6),list(bp=integer(),value=numeric()))

 expect_equal(res,out)
})

test_that("device data fetch works on gene data", {
  eset <- makeEset()
  dev1 <- epivizr::newDevice(eset, id="testid", type="gene", x="SAMP_1", y="SAMP_2")
  res <- dev1$getData(chr="chr6",start=30000000,end=40000000, cols=c("SAMP_1","SAMP_2"))

  tmp <- subsetByOverlaps(dev1$gr, GRanges(seqnames="chr6",ranges=IRanges(start=30000000,end=40000000)))
  m <- match(tmp$PROBEID, featureNames(eset))
  mat <- exprs(eset)[m,c("SAMP_1","SAMP_2")]

  lims <- unname(apply(mat, 2, function(x) range(pretty(x))))

  out <- list()
  out$min=lims[1,]
  out$max=lims[2,]
  out$data <- list(gene=tmp$SYMBOL,
                   start=start(tmp),
                   end=end(tmp),
                   probe=tmp$PROBEID,
                   unname(mat[,1]),
                   unname(mat[,2]))

  expect_equal(res,out)
})

test_that("mgr fetch works", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr6", ranges=IRanges(start=30000000+(1:10), width=100),
                 seqinfo=Seqinfo(seqnames=c("chr6","chr7"),genome="hcb"))
  gr2 <- GRanges(seqnames="chr7", ranges=IRanges(start=30000000+(2:20), width=100),
                 seqinfo=Seqinfo(seqnames=c("chr6","chr7"),genome="hcb"))
  gr3 <- GRanges(seqnames="chr6", ranges=IRanges(start=30000000+seq(1,100,by=5), width=1), score1=seq(1,100,by=5), score2=-seq(1,100,by=5),
                 seqinfo=Seqinfo(seqnames=c("chr6","chr7"),genome="hcb"))
  eset <- makeEset()


  tryCatch({
    mgr <- .startMGR(openBrowser=sendRequest, chr="chr6", start=30000000, end=40000000)
  
    dev1 <- mgr$addDevice(gr1, "dev1",sendRequest=sendRequest); devId1=dev1$id
    dev2 <- mgr$addDevice(gr2, "dev2",sendRequest=sendRequest); devId2=dev2$id
    dev3 <- mgr$addDevice(gr3, "dev3", sendRequest=sendRequest, type="bp"); devId3=dev3$id
    dev4 <- mgr$addDevice(eset, "dev4", sendRequest=sendRequest, type="gene", x="SAMP_1", y="SAMP_2"); devId4=dev4$id  

    tmp <- subsetByOverlaps(dev4$gr, GRanges(seqnames="chr6",ranges=IRanges(start=30000000,end=40000000)))

    m <- match(tmp$PROBEID, featureNames(eset))
    mat <- exprs(eset)[m,c("SAMP_1","SAMP_2")]
    lims <- unname(apply(mat, 2, function(x) range(pretty(x))))
    
    if (sendRequest) { 
      tryCatch(mgr$service(),interrupt=function(e) NULL)
    }
    
    measurements=list(geneMeasurements=paste0(devId4,"$SAMP_", 1:2), bpMeasurements=paste0(devId3,"$score",1:2),blockMeasurements=c(devId1,devId2))
    res <- mgr$getData(measurements, chr="chr6", start=30000000, end=40000000)
    
    out <- list(chr="chr6",start=30000000,end=40000000)
    out$geneData=list(start=30000000,end=40000000,chr="chr6")
    out$geneData$min=structure(lims[1,],names=paste0(devId4,"$","SAMP_",1:2))
    out$geneData$max=structure(lims[2,],names=paste0(devId4,"$","SAMP_",1:2))
    out$geneData$data=list(gene=tmp$SYMBOL,
                   start=start(tmp),
                   end=end(tmp),
                   probe=tmp$PROBEID,
                   unname(mat[,1]),
                   unname(mat[,2]))
    names(out$geneData$data)[5:6]=paste0(devId4,"$SAMP_",1:2)

    out$bpData=list(start=30000000,end=40000000,chr="chr6")
    out$bpData$min=structure(c(0,-100),names=paste0(devId3,"$","score",1:2))
    out$bpData$max=structure(c(100,0),names=paste0(devId3,"$","score",1:2))
    out$bpData$data=structure(list(list(bp=30000000+seq(1,100,by=5),value=seq(1,100,by=5)),
                                   list(bp=30000000+seq(1,100,by=5),value=-seq(1,100,by=5))),
                              names=paste0(devId3,"$","score",1:2))
    
    out$blockData=list(start=30000000,end=40000000,chr="chr6")
    out$blockData$data=structure(list(list(start=30000000+(1:10), end=30000000+(100:109)),
                                      list(start=integer(), end=integer())),
                                 names=c(devId1,devId2))
    
    #cat("res\n"); print(res$geneData)
    #cat("out\n"); print(out$geneData)

    expect_equal(res$geneData,out$geneData)
    expect_equal(res$bpData,out$bpData)
    expect_equal(res$blockData,out$blockData)
  }, finally=mgr$stopServer())
})

test_that("mgr fetch no data works", {
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100),
                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100),
                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=5), width=1), score=seq(1,100,by=5),
                 seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
  
  tryCatch({
    sendRequest=sendRequest
    mgr <- .startMGR(openBrowser=sendRequest)
    
    dev1 <- mgr$addDevice(gr1, "dev1"); devId1=dev1$id
    dev2 <- mgr$addDevice(gr2, "dev2"); devId2=dev2$id
    dev3 <- mgr$addDevice(gr3, "dev3", type="bp"); devId3=dev3$id
    
    if (sendRequest) {
      tryCatch(mgr$service(), interrupt=function(e) NULL)
    }
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

  expect_equal(res,out)
  }, finally=mgr$stopServer())
})

test_that("data with NAs are handled", {
    sendRequest=sendRequest
    gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100),
                   seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
    gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100),
                   seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
    gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=5), width=1), score1=seq(1,100,by=5), score2=-seq(1,100,by=5),
                   seqinfo=Seqinfo(seqnames=c("chr1","chr2"),genome="hcb"))
    
    gr3$score2[1:10] <- NA
    
    tryCatch({
      mgr <- .startMGR(openBrowser=sendRequest, chr="chr1", start=2, end=6)
      
      dev1 <- mgr$addDevice(gr1, "dev1",sendRequest=sendRequest); devId1=dev1$id
      dev2 <- mgr$addDevice(gr2, "dev2",sendRequest=sendRequest); devId2=dev2$id
      dev3 <- mgr$addDevice(gr3, "dev3", sendRequest=sendRequest, type="bp"); devId3=dev3$id
      
      if (sendRequest) { 
        tryCatch(mgr$service(),interrupt=function(e) NULL)
      }
      
      measurements=list(bpMeasurements=paste0(devId3,"$score",1:2),blockMeasurements=c(devId1,devId2))
      res <- mgr$getData(measurements, chr="chr1", start=2, end=6)
      
      out <- list(chr="chr1",start=2,end=6)
      out$bpData=list(start=2,end=6,chr="chr1")
      out$bpData$min=structure(c(5,-6),names=paste0(devId3,"$","score",1:2))
      out$bpData$max=structure(c(10,6),names=paste0(devId3,"$","score",1:2))
      out$bpData$data=structure(list(list(bp=6,value=6),list(bp=integer(),value=numeric())),names=paste0(devId3,"$","score",1:2))
      
      out$blockData=list(start=2,end=6,chr="chr1")
      out$blockData$data=structure(list(list(start=1:6, end=100:105),
                                        list(start=integer(), end=integer())),
                                   names=c(devId1,devId2))
      
      #print(res);print(out)
      expect_equal(res,out)
    }, finally=mgr$stopServer())
})