
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


## ----, eval=FALSE--------------------------------------------------------
## mgr=startEpiviz(workspace="C60FA3168F34DBC763F579C1EADA8AF0")


## ----, eval=FALSE--------------------------------------------------------
## blocks_dev <- mgr$addDevice(colon_blocks, "450k colon_blocks")
## mgr$service()


## ----, eval=FALSE--------------------------------------------------------
## # subset to those with length > 25Kbp
## keep <- width(colon_blocks) > 25000
## mgr$updateDevice(blocks_dev, colon_blocks[keep,])


## ----, eval=FALSE--------------------------------------------------------
## # add low-filter smoothed methylation estimates
## means_dev <- mgr$addDevice(colon_curves, "450kMeth",type="bp",mdCols=c("cancerMean","normalMean"))
## mgr$service()


## ----, eval=FALSE--------------------------------------------------------
## diff_dev <- mgr$addDevice(colon_curves,"450kMethDiff",type="bp",mdCols=c("smooth"),ylim=matrix(c(-.5,.5),nc=1))
## mgr$service()


## ----, eval=FALSE--------------------------------------------------------
## mgr$listDevices()
## mgr$rmDevice(means_dev)


## ------------------------------------------------------------------------
keep <- pData(apColonData)$SubType!="adenoma"
apColonData <- apColonData[keep,]
status <- pData(apColonData)$Status
Indexes <- split(seq(along=status),status)

exprMat <- exprs(apColonData)
mns <- sapply(Indexes, function(ind) rowMeans(exprMat[,ind]))
mat <- cbind(colonM=mns[,"1"]-mns[,"0"], colonA=0.5*(mns[,"1"]+mns[,"0"]))

eset <- ExpressionSet(assayData=mat, annotation=annotation(apColonData))
show(eset)


## ----, eval=FALSE--------------------------------------------------------
## eset_dev <- mgr$addDevice(eset, "MAPlot", type="gene", x="colonA", y="colonM")
## mgr$service()


## ------------------------------------------------------------------------
data(tcga_colon_expression)
show(colonSE)


## ----, eval=FALSE--------------------------------------------------------
## ref_sample <- 2 ^ rowMeans(log2(assay(colonSE) + 1))
## scaled <- (assay(colonSE) + 1) / ref_sample
## scaleFactor <- matrixStats::colMedians(scaled)
## assay_normalized <- sweep(assay(colonSE), 2, scaleFactor, "/")
## assay(colonSE) <- assay_normalized


## ----, eval=FALSE--------------------------------------------------------
## status <- colData(colonSE)$sample_type
## index <- split(seq(along = status), status)
## logCounts <- log2(assay(colonSE) + 1)
## means <- sapply(index, function(ind) rowMeans(logCounts[, ind]))
## mat <- cbind(cancer = means[, "Primary Tumor"], normal = means[, "Solid Tissue Normal"])


## ----, eval=FALSE--------------------------------------------------------
## sumexp <- SummarizedExperiment(mat, rowData=rowData(colonSE))
## se_dev <- mgr$addDevice(sumexp, "Mean by Sample Type", type="gene", x="normal", y="cancer")
## mgr$service()


## ----, eval=FALSE--------------------------------------------------------
## mgr$navigate("chr2", 110000000, 120000000)


## ----, eval=FALSE--------------------------------------------------------
## foldChange=mat[,"cancer"]-mat[,"normal"]
## ind=order(foldChange,decreasing=TRUE)
## slideshowRegions <- rowData(sumexp)[ind]
## slideshowRegions <- resize(slideshowRegions, fix="center", width=width(slideshowRegions)+2000000)
## mgr$slideshow(slideshowRegions, n=5)


## ----, eval=FALSE--------------------------------------------------------
## mgr$stopServer()


## ----session-info, cache=FALSE-------------------------------------------
sessionInfo()


