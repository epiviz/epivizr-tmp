
## ----, eval=TRUE, echo=TRUE, results='hide', warning=FALSE, error=FALSE----
require(epivizr)
require(antiProfilesData)


## ------------------------------------------------------------------------
data(tcga_colon_example)
data(apColonData)


## ------------------------------------------------------------------------
show(colon_blocks)


## ----, fig.width=4, fig.height=4, fig.align='center'---------------------
plot(colon_blocks$value, -log10(colon_blocks$p.value), main="Volcano plot", xlab="Avg. methylation difference", ylab="-log10 p-value",xlim=c(-.5,.5))


## ------------------------------------------------------------------------
show(colon_curves)


## ----, eval=FALSE, echo=TRUE---------------------------------------------
## mgr=startEpiviz(workspace="C60FA3168F34DBC763F579C1EADA8AF0")


## ----, eval=TRUE, echo=FALSE---------------------------------------------
mgr=startEpiviz(debug=TRUE, openBrowser=FALSE, nonInteractive=TRUE, tryPorts=TRUE)
mgr$startServer()


## ----,eval=TRUE----------------------------------------------------------
blocks_dev <- mgr$addDevice(colon_blocks, "450k colon_blocks")
mgr$service()


## ----, eval=TRUE---------------------------------------------------------
# subset to those with length > 250Kbp
keep <- width(colon_blocks) > 250000
mgr$updateDevice(blocks_dev, colon_blocks[keep,])


## ----, eval=TRUE---------------------------------------------------------
# add low-filter smoothed methylation estimates
means_dev <- mgr$addDevice(colon_curves, "450kMeth",type="bp",columns=c("cancerMean","normalMean"))
mgr$service()


## ----, eval=TRUE---------------------------------------------------------
diff_dev <- mgr$addDevice(colon_curves,"450kMethDiff",type="bp",columns=c("smooth"),ylim=matrix(c(-.5,.5),nc=1))
mgr$service()


## ----, eval=TRUE---------------------------------------------------------
mgr$listDevices()
mgr$rmDevice(means_dev)
mgr$listDevices()


## ------------------------------------------------------------------------
keep <- pData(apColonData)$SubType!="adenoma"
apColonData <- apColonData[,keep]
status <- pData(apColonData)$Status
Indexes <- split(seq(along=status),status)

exprMat <- exprs(apColonData)
mns <- sapply(Indexes, function(ind) rowMeans(exprMat[,ind]))
mat <- cbind(colonM=mns[,"1"]-mns[,"0"], colonA=0.5*(mns[,"1"]+mns[,"0"]))
assayDataElement(apColonData, "MA") <- mat
show(apColonData)


## ----, eval=TRUE---------------------------------------------------------
eset_dev <- mgr$addDevice(apColonData, "MAPlot", columns=c("colonA","colonM"), assay="MA")
mgr$service()


## ------------------------------------------------------------------------
data(tcga_colon_expression)
show(colonSE)


## ----, eval=TRUE---------------------------------------------------------
ref_sample <- 2 ^ rowMeans(log2(assay(colonSE) + 1))
scaled <- (assay(colonSE) + 1) / ref_sample
scaleFactor <- Biobase::rowMedians(t(scaled))
assay_normalized <- sweep(assay(colonSE), 2, scaleFactor, "/")
assay(colonSE) <- assay_normalized


## ----, eval=TRUE---------------------------------------------------------
status <- colData(colonSE)$sample_type
index <- split(seq(along = status), status)
logCounts <- log2(assay(colonSE) + 1)
means <- sapply(index, function(ind) rowMeans(logCounts[, ind]))
mat <- cbind(cancer = means[, "Primary Tumor"], normal = means[, "Solid Tissue Normal"])


## ----, eval=TRUE---------------------------------------------------------
sumexp <- SummarizedExperiment(mat, rowData=rowData(colonSE))
se_dev <- mgr$addDevice(sumexp, "Mean by Sample Type", columns=c("normal", "cancer"))
mgr$service()


## ----, eval=TRUE---------------------------------------------------------
mgr$navigate("chr11", 110000000, 120000000)


## ----, eval=TRUE---------------------------------------------------------
foldChange=mat[,"cancer"]-mat[,"normal"]
ind=order(foldChange,decreasing=TRUE)

# bounding 1Mb around each exon
slideshowRegions <- rowData(sumexp)[ind] + 1000000L
mgr$slideshow(slideshowRegions, n=5)


## ------------------------------------------------------------------------
mgr$stopServer()


## ----session-info, cache=FALSE-------------------------------------------
sessionInfo()


