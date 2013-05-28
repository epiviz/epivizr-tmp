makeEset <- function() {
  require(hgu133plus2.db)
  nprobeids=1000
  nsamples=6
  expr=matrix(rnorm(nprobeids*nsamples), nr=nprobeids)
  pd=data.frame(a=1:nsamples,b=10*(1:nsamples))
  rownames(pd)=paste0("SAMP_",1:nsamples)
  rownames(expr)=head(keys(hgu133plus2.db, keytype="PROBEID"), nprobeids)
  colnames(expr)=rownames(pd)

  ExpressionSet(assayData=expr, phenoData=AnnotatedDataFrame(pd),annotation="hgu133plus2")
}