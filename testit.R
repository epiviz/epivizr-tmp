library(epivizr)
data(tcga_colon_example)

show(colon_blocks)
show(colon_curves)

mgr=startEpiviz(localURL="http://localhost/~hcorrada/epiviz",debug=TRUE,proxy=TRUE)

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
diff_dev <- mgr$addDevice(colon_curves,"450kMethDiff",type="bp",mdCols=c("smooth"))
mgr$service()

# list devices
mgr$listDevices()

# remove device
mgr$rmDevice(diff_dev)

mgr$listDevices()

# stop epiviz
mgr$stopServer()

