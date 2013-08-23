context("addMeasurements")

sendRequest=sendRequest

test_that("addMeasurements works for blocks", {
  sendRequest=sendRequest
  gr <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
  mgr <- .startMGR(openBrowser=sendRequest)
  
  tryCatch({
    msObj <- mgr$addMeasurements(gr, "ms1", sendRequest=sendRequest)
    msId <- msObj$getId()
    
    expect_equal(length(mgr$msList$block), 1)
    expect_false(is.null(mgr$msList$block[[msId]]))
    expect_equal(mgr$msList$block[[msId]]$name, "ms1")
    expect_equal(mgr$msList$block[[msId]]$measurements, msId)
    expect_equal(as(mgr$msList$block[[msId]]$obj$tree, "GRanges"), unname(gr))
    
    connected <- mgr$msList$block[[msId]]$connected
    expect_equal(connected, sendRequest)

  }, finally=mgr$stopServer())
})

# test_that("update works", {
#   sendRequest=sendRequest
#   gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
#   gr2 <- GRanges(seqnames="chr12", ranges=IRanges(start=1:1000,width=10))
#   mgr <- .startMGR(openBrowser=sendRequest)
  
#   tryCatch({
#     dev <- mgr$addDevice(gr1, "dev1", sendRequest=sendRequest)
#     devId <- dev$id
#     mgr$updateDevice(dev, gr2)
    
#     expect_equal(mgr$devices$block[[devId]]$obj$object, gr2)
#     expect_equal(as(mgr$devices$block[[devId]]$obj$tree, "GRanges"), unname(gr2))    
#   }, finally=mgr$stopServer())
# })

# test_that("addMeasurements works for bp", {
#   sendRequest=sendRequest
#   gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
#     score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
#   mgr <- .startMGR(openBrowser=sendRequest)
  
#   tryCatch({
#     msObj <- mgr$addMeasurements(gr, "ms1", sendRequest=sendRequest, type="bp")
#     msId <- msObj$getId()
    
#     expect_equal(length(mgr$msList$bp), 1)
#     expect_false(is.null(mgr$msList$bp[[msId]]))
#     expect_equal(mgr$msList$bp[[msId]]$name, "ms1")
#     expect_equal(mgr$msList$bp[[msId]]$measurements, paste0(msId,"$","score",1:2))
#     expect_equal(as(mgr$msList$bp[[msId]]$obj$tree, "GRanges"), unname(gr))
#     expect_equal(mgr$msList$bp[[msId]]$obj$columns, paste0("score",1:2))
    
#     connected <- mgr$msList$bp[[msId]]$connected
#     expect_equal(connected, sendRequest)
#   }, finally=mgr$stopServer())
# })

# test_that("addMeasurements works for ExpressionSet", {
#   sendRequest=sendRequest
#   eset <- makeEset()
#   mgr <- .startMGR(openBrowser=sendRequest)
  
#   tryCatch({
#     msObj <- mgr$addMeasurements(eset, "ms1", sendRequest=sendRequest, columns=c("SAMP_1","SAMP_2"))
#     msId <- msObj$getId()
    
#     expect_equal(length(mgr$msList$gene), 1)
#     expect_false(is.null(mgr$msList$gene[[msId]]))
#     expect_equal(mgr$msList$gene[[msId]]$name, "ms1")
#     expect_equal(mgr$msList$gene[[msId]]$measurements, paste0(msId,"$","SAMP_",1:2))
#     expect_equal(mgr$msList$gene[[msId]]$obj$columns, paste0("SAMP_",1:2))

#     connected <- mgr$msList$gene[[msId]]$connected
#     expect_equal(connected, sendRequest)
#   }, finally=mgr$stopServer())
# })

# test_that("rmDevice works", {
#   sendRequest=sendRequest
#   gr <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), 
#     score1=rnorm(length(seq(1,100,by=25))),score2=rnorm(length(seq(1,100,by=25))))
  
#   mgr <- .startMGR(openBrowser=sendRequest)
  
#   tryCatch({
#     dev <- mgr$addDevice(gr, "dev1", sendRequest=sendRequest, type="bp")
#     devId <- dev$id
#     mgr$rmDevice(dev)
    
#     expect_equal(length(mgr$devices$block), 0)
#     expect_true(is.null(mgr$devices$block[[devId]]))
#     expect_equal(mgr$activeId, "")
#   },finally=mgr$stopServer())
# })

# test_that("listDevices works", {
#   sendRequest=sendRequest
#   gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
#   gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100))
#   gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
#   eset <- makeEset()

#   mgr <- .startMGR(openBrowser=sendRequest)
#   tryCatch({
#     dev1 <- mgr$addDevice(gr1, "dev1", sendRequest=sendRequest); devId1=dev1$id
#     dev2 <- mgr$addDevice(gr2, "dev2", sendRequest=sendRequest); devId2=dev2$id
#     dev3 <- mgr$addDevice(gr3, "dev3", sendRequest=sendRequest, type="bp"); devId3=dev3$id
#     dev4 <- mgr$addDevice(eset, "dev4", sendRequest = sendRequest, columns=c("SAMP_1", "SAMP_2")); devId4=dev4$id
    
#     devs <- mgr$listDevices()
#     expected_df <- list(gene=data.frame(id=devId4,
#                              active="*",
#                              name="dev4",
#                              length=length(dev4$object),
#                              connected=ifelse(sendRequest,"*",""),
#                              stringsAsFactors=FALSE),
#                         bp=data.frame(id=devId3,
#                                       active="",
#                                       name="dev3",
#                                       length=length(gr3),
#                                       connected=ifelse(sendRequest,"*",""),
#                                       stringsAsFactors=FALSE),
#                         block=data.frame(id=c(devId1,devId2),
#                               active=c("",""),
#                               name=c("dev1","dev2"),
#                               length=c(length(gr1),length(gr2)),
#                               connected=ifelse(sendRequest,c("*","*"),c("","")),
#                               stringsAsFactors=FALSE)
#                         )
#     expect_equal(devs, expected_df)
#   }, finally=mgr$stopServer())
# })

# test_that("setActive works", {
#   sendRequest=sendRequest
#   gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=1))
#   gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=1))
  
#   mgr <- .startMGR(openBrowser=sendRequest)
#   tryCatch({
#     dev1 <- mgr$addDevice(gr1, "dev1", sendRequest=sendRequest); devId1=dev1$id
#     dev2 <- mgr$addDevice(gr2, "dev2", sendRequest=sendRequest); devId2=dev2$id
  
#     mgr$setActive(devId1)
    
#         devs <- mgr$listDevices()
#     expected_df <- list(block=data.frame(id=c(devId1,devId2),
#                             active=c("*",""),
#                             name=c("dev1","dev2"),
#                             length=c(length(gr1),length(gr2)),
#                             connected=ifelse(sendRequest,c("*","*"),c("","")),
#                             stringsAsFactors=FALSE))
#     expect_equal(devs, expected_df)
#   },finally=mgr$stopServer())
# })

# test_that("getMeasurements works", {
#   sendRequest=sendRequest
#   gr1 <- GRanges(seqnames="chr1", ranges=IRanges(start=1:10, width=100))
#   gr2 <- GRanges(seqnames="chr2", ranges=IRanges(start=2:20, width=100))
#   gr3 <- GRanges(seqnames="chr1", ranges=IRanges(start=seq(1,100,by=25), width=1), score=rnorm(length(seq(1,100,by=25))))
#   eset <- makeEset()

#   mgr <- .startMGR(openBrowser=sendRequest)
#   tryCatch({
#     dev1 <- mgr$addDevice(gr1, "dev1", sendRequest=sendRequest); devId1=dev1$id
#     dev2 <- mgr$addDevice(gr2, "dev2", sendRequest=sendRequest); devId2=dev2$id
#     dev3 <- mgr$addDevice(gr3, "dev3", sendRequest=sendRequest, type="bp"); devId3=dev3$id
#     dev4 <- mgr$addDevice(eset, "dev4", sendRequest=sendRequest, columns=c("SAMP_1", "SAMP_2")); devId4=dev4$id  
#     res <- mgr$getMeasurements()

#     out <- list(geneMeasurements=structure(list("dev4$SAMP_1","dev4$SAMP_2"), names=paste0(devId4, c("$SAMP_1","$SAMP_2"))),
#                 bpMeasurements=structure(list("dev3$score"), names=paste0(devId3,"$score")), 
#                 blockMeasurements=structure(list("dev1","dev2"), names=c(devId1,devId2)))

#     expect_equal(res,out)
#   }, finally=mgr$stopServer())
# })