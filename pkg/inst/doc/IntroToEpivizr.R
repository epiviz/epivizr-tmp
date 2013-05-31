
## @knitr , eval=TRUE, echo=TRUE, results='hide', warning=FALSE, error=FALSE
require(epivizr)
require(antiProfilesData)


## @knitr 
data(tcga_colon_example)
data(apColonData)


## @knitr 
show(colon_blocks)


## @knitr , fig.width=4, fig.height=4, fig.align='center'
plot(colon_blocks$value, -log10(colon_blocks$p.value), main="Volcano plot", xlab="Avg. methylation difference", ylab="-log10 p-value",xlim=c(-.5,.5))


## @knitr 
show(colon_curves)


## @knitr , eval=FALSE
## mgr=startEpiviz()


## @knitr , eval=FALSE
## blocks_dev <- mgr$addDevice(colon_blocks, "450k colon_blocks")
## mgr$service()


## @knitr , eval=FALSE
## # subset to those with pvalue <0.05
## keep <- colon_blocks$p.value < 0.05
## mgr$updateDevice(blocks_dev, colon_blocks[keep,])


## @knitr , eval=FALSE
## # add low-filter smoothed methylation estimates
## means_dev <- mgr$addDevice(colon_curves, "450kMeth",type="bp",mdCols=c("cancerMean","normalMean"))
## mgr$service()


## @knitr , eval=FALSE
## diff_dev <- mgr$addDevice(colon_curves,"450kMethDiff",type="bp",mdCols=c("smooth"),ylim=matrix(c(-.5,.5),nc=1))
## mgr$service()


## @knitr , eval=FALSE
## mgr$listDevices()
## mgr$rmDevice(diff_dev)


## @knitr 
keep <- pData(apColonData)$SubType!="adenoma"
apColonData <- apColonData[keep,]
status <- pData(apColonData)$Status
Indexes <- split(seq(along=status),status)

exprMat <- exprs(apColonData)
mns <- sapply(Indexes, function(ind) rowMeans(exprMat[,ind]))
mat <- cbind(colonM=mns[,"1"]-mns[,"0"], colonA=0.5*(mns[,"1"]+mns[,"0"]))

eset <- ExpressionSet(assayData=mat, annotation=annotation(apColonData))
show(eset)


## @knitr , eval=FALSE
## expr_dev <- mgr$addDevice(eset, "MAPlot", type="gene", x="colonA", y="colonM")
## mgr$service()


## @knitr , eval=FALSE
## mgr$stopServer()


## @knitr session-info, cache=FALSE
sessionInfo()


