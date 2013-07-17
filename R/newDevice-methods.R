setGeneric("newDevice", signature=c("object"), function(object, id, columns, ...) standardGeneric("newDevice"))

setMethod("newDevice", "GRanges",
	function(object, id, columns=NULL, type=c("block","bp")) {
		type <- match.arg(type)
		dev <- switch(type,
					  block=EpivizBlockDevice$new(gr=obj, id=id, columns=columns),
					  bp=EpivizBpDevice$new(gr=obj, id=id, columns=columns))
		return(dev)
})

setMethod("newDevice", "SummarizedExperiment",
	function(object, id, columns) {
		# verify x and y are valid samples
	if (any(!(c(x,y) %in% colnames(obj)))) {
		stop("'x' or 'y' not found as samples")
	}
	
	gr <- rowData(obj)
	
	mcols(gr)[,x] <- assay(obj)[,x]
	mcols(gr)[,y] <- assay(obj)[,y]
	
	mcols(gr)[,"SYMBOL"] = ""
	mcols(gr)[,"PROBEID"] = rownames(obj)
	
	if (missing(xlim) || is.null(xlim)) {
		rng <- range(mcols(gr)[,x])
		xlim <- range(pretty(seq(rng[1],rng[2],len=10)))
	}
	if (missing(ylim) || is.null(ylim)) {
		rng <- range(mcols(gr)[,y])
		ylim <- range(pretty(seq(rng[1],rng[2],len=10)))
	}
		dev <- EpivizFeatureDevice$new(gr=obj, id=id, columns=columns)
		return(dev)
})

setMethod("newDevice", "ExpressionSet",
	function(object, id, columns) {
		if (annotation(obj) != 'hgu133plus2') {
			stop("only 'hgu133plus2' affy chips supported for now")
		}
		
		# make GRanges object with appropriate info
		probeids = featureNames(obj)
		annoName = paste0(annotation(obj), ".db")
		if (!require(annoName, character.only=TRUE)) {
			stop("package '", annoName, "' is required")
		}
		
		res = suppressWarnings(select(get(annoName), keys=probeids, columns=c("SYMBOL", "CHR", "CHRLOC", "CHRLOCEND"), keytype="PROBEID"))
		dups = duplicated(res$PROBEID)
		res = res[!dups,]
		
		isna = is.na(res$CHR) | is.na(res$CHRLOC) | is.na(res$CHRLOCEND)
		res = res[!isna,]
		
		gr = GRanges(seqnames=paste0("chr",res$CHR),
				strand=ifelse(res$CHRLOC>0, "+","-"),
				ranges=IRanges(start=abs(res$CHRLOC), end=abs(res$CHRLOCEND)))
		
		mcols(gr)[,x] = exprs(obj)[!isna,x]
		mcols(gr)[,y] = exprs(obj)[!isna,y]
		
		mcols(gr)[,"SYMBOL"] = res$SYMBOL
		mcols(gr)[,"PROBEID"] = res$PROBEID
		
		if (missing(xlim) || is.null(xlim)) {
			rng <- range(mcols(gr)[,x])
			xlim <- range(pretty(seq(rng[1],rng[2],len=10)))
		}
		if (missing(ylim) || is.null(ylim)) {
			rng <- range(mcols(gr)[,y])
			ylim <- range(pretty(seq(rng[1],rng[2],len=10)))
		}
		dev <- EpivizFeatureDevice$new(gr=obj, id=id, columns=columns)
		return(dev)
})

