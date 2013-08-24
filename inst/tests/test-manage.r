context("device management")

sendRequest=sendRequest

test_that("addDevice works for blocks", {
  sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    dev <- mgr$addDevice(gr, "dev1", sendRequest=sendRequest)
    devId <- dev$id
    
    expect_equal(length(mgr$devices$block), 1)
    expect_false(is.null(mgr$devices$block[[devId]]))
    expect_equal(mgr$devices$block[[devId]]$name, "dev1")
    expect_equal(mgr$devices$block[[devId]]$measurements, devId)
    expect_equal(as(mgr$devices$block[[devId]]$obj$tree, "GRanges"), unname(gr))
    expect_equal(mgr$activeId, devId)
    
    if (sendRequest) {
      expect_false(is.null(mgr$chartIdMap[[devId]]))
    }
  }, finally=mgr$stopServer())
})

test_that("update works", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  gr2 <- GRanges(seqnames="chr12", ranges=IRanges(start=1:1000,width=10))
  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    dev <- mgr$addDevice(gr1, "dev1", sendRequest=sendRequest)
    devId <- dev$id
    mgr$updateDevice(dev, gr2)
    
    expect_equal(mgr$devices$block[[devId]]$obj$object, gr2)
    expect_equal(as(mgr$devices$block[[devId]]$obj$tree, "GRanges"), unname(gr2))    
  }, finally=mgr$stopServer())
})

test_that("addDevice works for bp", {
  sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    dev <- mgr$addDevice(gr, "dev1", sendRequest=sendRequest, type="bp")
    devId <- dev$id
    
    expect_equal(length(mgr$devices$bp), 1)
    expect_false(is.null(mgr$devices$bp[[devId]]))
    expect_equal(mgr$devices$bp[[devId]]$name, "dev1")
    expect_equal(mgr$devices$bp[[devId]]$measurements, paste0(devId,"$","score",1:2))
    expect_equal(as(mgr$devices$bp[[devId]]$obj$tree, "GRanges"), unname(gr))
    expect_equal(mgr$devices$bp[[devId]]$obj$columns, paste0("score",1:2))
    expect_equal(mgr$activeId, devId)
    
    if (sendRequest) {
      expect_false(is.null(mgr$chartIdMap[[devId]]))
    }
  }, finally=mgr$stopServer())
})

test_that("addDevice works for gene", {
  sendRequest=sendRequest
  eset <- makeEset()
  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    dev <- mgr$addDevice(eset, "dev1", sendRequest=sendRequest, columns=c("SAMP_1","SAMP_2"))
    devId <- dev$id
    
    expect_equal(length(mgr$devices$gene), 1)
    expect_false(is.null(mgr$devices$gene[[devId]]))
    expect_equal(mgr$devices$gene[[devId]]$name, "dev1")
    expect_equal(mgr$devices$gene[[devId]]$measurements, paste0(devId,"$","SAMP_",1:2))
    expect_equal(mgr$devices$gene[[devId]]$obj$columns, paste0("SAMP_",1:2))
    expect_equal(mgr$activeId, devId)
    
    if (sendRequest) {
      expect_false(is.null(mgr$chartIdMap[[devId]]))
    }
  }, finally=mgr$stopServer())
})

test_that("rmDevice works", {
  sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
    score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  
  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    dev <- mgr$addDevice(gr, "dev1", sendRequest=sendRequest, type="bp")
    devId <- dev$id
    mgr$rmDevice(dev)
    
    expect_equal(length(mgr$devices$block), 0)
    expect_true(is.null(mgr$devices$block[[devId]]))
    expect_equal(mgr$activeId, "")
  },finally=mgr$stopServer())
})

test_that("listDevices works", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100))
  gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
  eset <- makeEset()

  mgr <- .startMGR(openBrowser=sendRequest)
  tryCatch({
    dev1 <- mgr$addDevice(gr1, "dev1", sendRequest=sendRequest); devId1=dev1$id
    dev2 <- mgr$addDevice(gr2, "dev2", sendRequest=sendRequest); devId2=dev2$id
    dev3 <- mgr$addDevice(gr3, "dev3", sendRequest=sendRequest, type="bp"); devId3=dev3$id
    dev4 <- mgr$addDevice(eset, "dev4", sendRequest = sendRequest, columns=c("SAMP_1", "SAMP_2")); devId4=dev4$id
    
    devs <- mgr$listDevices()
    expected_df <- list(gene=data.frame(id=devId4,
                             active="*",
                             name="dev4",
                             length=length(dev4$object),
                             connected=ifelse(sendRequest,"*",""),
                             stringsAsFactors=FALSE),
                        bp=data.frame(id=devId3,
                                      active="",
                                      name="dev3",
                                      length=length(gr3),
                                      connected=ifelse(sendRequest,"*",""),
                                      stringsAsFactors=FALSE),
                        block=data.frame(id=c(devId1,devId2),
                              active=c("",""),
                              name=c("dev1","dev2"),
                              length=c(length(gr1),length(gr2)),
                              connected=ifelse(sendRequest,c("*","*"),c("","")),
                              stringsAsFactors=FALSE)
                        )
    expect_equal(devs, expected_df)
  }, finally=mgr$stopServer())
})

test_that("setActive works", {
  sendRequest=sendRequest
  gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=1))
  
  mgr <- .startMGR(openBrowser=sendRequest)
  tryCatch({
    dev1 <- mgr$addDevice(gr1, "dev1", sendRequest=sendRequest); devId1=dev1$id
    dev2 <- mgr$addDevice(gr2, "dev2", sendRequest=sendRequest); devId2=dev2$id
  
    mgr$setActive(devId1)
    
        devs <- mgr$listDevices()
    expected_df <- list(block=data.frame(id=c(devId1,devId2),
                            active=c("*",""),
                            name=c("dev1","dev2"),
                            length=c(length(gr1),length(gr2)),
                            connected=ifelse(sendRequest,c("*","*"),c("","")),
                            stringsAsFactors=FALSE))
    expect_equal(devs, expected_df)
  },finally=mgr$stopServer())
})

