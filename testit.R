library(devtools)
load_all("pkg",reset=TRUE)

library(GenomicRanges)
load("thytest.rda")

localURL="http://localhost/~hcorrada/epiviz/index.php"
mgr=startEpiviz(localURL=localURL,debug=TRUE,openBrowser=TRUE)
thygrId=mgr$addDevice(thygr, "thyroid_blocks")






addTrack(mgr, thygr)

thyGR2=thygr[values(thygr)$pvalues<0.01,]
values(thyGR2)$myorder=rank(values(thyGR2)$pvalues, -abs(values(thyGR2)$seg.mean))
refresh(mgr, thyGR2,order.by="myorder")


diffStat=GRanges(seqnames=as.character(thydat$chrom), 
                 ranges=IRanges(start=thydat$maploc, width=1))

values(diffStat)$stat=thydat$Sample.1
values(diffStat)$smooth=runmed(thydat$Sample.1,9)

addTrack(mgr,diffStat,type="matrix",matcols=c("stat","smooth"),ylim=c(-5,5))
setActive(mgr,"obj_1")
listTracks(mgr)
stopCegs(mgr)

