library(devtools)
load_all("pkg",reset=TRUE)

load("thytest.rda")

localURL="http://localhost/~hcorrada/epiviz/index.php"
mgr=startEpiviz(localURL=NULL,debug=FALSE,openBrowser=TRUE,proxy=TRUE)

#need to interrupt before continuing on
thygrId=mgr$addDevice(thygr, "thyroid_blocks")

diffStat=GRanges(seqnames=as.character(thydat$chrom), 
                 ranges=IRanges(start=thydat$maploc, width=1))

diffStat$stat=thydat$Sample.1
diffStat$smooth=runmed(thydat$Sample.1,9)

mgr$addDevice(diffStat,"diff_stat",type="bp")

mgr$service()

# need to interrupt to continue
mgr$stopServer()

