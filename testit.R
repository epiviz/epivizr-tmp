require(epivizr)
data(tcga_colon_example)

show(colon_blocks)
show(colon_curves)
	
mgr=startEpiviz(localURL="http://localhost/~hcorrada/epiviz",debug=TRUE,proxy=TRUE)
#mgr=startEpiviz()

#need to interrupt before continuing on
blocks_dev <- mgr$addDevice(colon_blocks, "450k colon_blocks")
mgr$service()

mgr$listDevices()

# subset to those with pvalue <0.05
keep <- colon_blocks$p.value < 0.05
mgr$updateDevice(blocks_dev, colon_blocks[keep,])

mgr$listDevices()

# add low-filter smoothed methylation estimates
means_dev <- mgr$addDevice(colon_curves, "450kMeth",type="bp",mdCols=c("cancerMean","normalMean"))
mgr$service()

# add low-filter smoothed methylation difference estimate
diff_dev <- mgr$addDevice(colon_curves,"450kMethDiff",type="bp",mdCols=c("smooth"),ylim=matrix(c(-.5,.5),nc=1))
mgr$service()

# list devices
mgr$listDevices()

# remove device
mgr$rmDevice(diff_dev)

# load expression data
require(antiProfilesData)
data(apColonData)
keep <- pData(apColonData)$SubType!="adenoma"
apColonData <- apColonData[keep,]
status <- pData(apColonData)$Status
Indexes <- split(seq(along=status),status)

exprMat <- exprs(apColonData)
mns <- sapply(Indexes, function(ind) rowMeans(exprMat[,ind]))
mat <- cbind(colonM=mns[,"1"]-mns[,"0"], colonA=0.5*(mns[,"1"]+mns[,"0"]))

eset <- ExpressionSet(assayData=mat, annotation=annotation(apColonData))
expr_dev <- mgr$addDevice(eset, "MAPlot", type="gene", x="colonA", y="colonM")
mgr$service()

mgr$listDevices()

# stop epiviz
mgr$stopServer()

