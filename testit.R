library(devtools)
load_all("pkg",reset=TRUE)

library(GenomicRanges)
load("thytest.rda")

localURL="http://localhost/~hcorrada/epiviz/index.php"
mgr=startEpiviz(localURL=localURL,debug=TRUE,openBrowser=TRUE)
thygrId=mgr$addDevice(thygr, "thyroid_blocks")

diffStat=GRanges(seqnames=as.character(thydat$chrom), 
                 ranges=IRanges(start=thydat$maploc, width=1))

diffStat$stat=thydat$Sample.1
diffStat$smooth=runmed(thydat$Sample.1,9)

mgr$addTrack(diffStat,"diff_stat",type="bp")
mgr$stop()

