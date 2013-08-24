setGeneric("register", signature=c("object"), 
	function(object, columns=NULL, ...) standardGeneric("register"))

setMethod("register", "GenomicRanges",
	function(object, columns, type=c("block","bp")) {
		type <- match.arg(type)
		if (!is(object, "GIntervalTree")) {
			object <- as(object, "GIntervalTree")
		}
		dev <- switch(type,
					  block=EpivizBlockData$new(object=object),
					  bp=EpivizBpData$new(object=object, columns=columns))
		return(dev)
})

setMethod("register", "SummarizedExperiment",
	function(object, columns=NULL, assay=1) {
		if (!is(rowData(object), "GIntervalTree")) {
			rowData(object) <- as(rowData(object), "GIntervalTree")
		}

		mcolNames <- names(mcols(rowData(object)))

		if (!("PROBEID" %in% mcolNames)) {
			rowData(object)$PROBEID <- ""
		} 
		if (!("SYMBOL" %in% mcolNames)) {
			rowData(object)$SYMBOL <- ""
		} 
		EpivizFeatureData$new(object=object, columns=columns, assay=assay)
})

setMethod("register", "ExpressionSet",
	function(object, columns, annotation=NULL) {
		if (is.null(annotation) || missing(annotation)) 
			annotation <- annotation(object)

		if (annotation != 'hgu133plus2') {
			stop("only 'hgu133plus2' affy chips supported for now")
		}
		
		# make GRanges object with appropriate info
		probeids = featureNames(object)
		annoName = paste0(annotation, ".db")

		if (!require(annoName, character.only=TRUE)) {
			stop("package '", annoName, "' is required")
		}
		
		res = suppressWarnings(select(get(annoName), keys=probeids, columns=c("SYMBOL", "CHR", "CHRLOC", "CHRLOCEND"), keytype="PROBEID"))
		dups = duplicated(res$PROBEID)
		res = res[!dups,]

		drop = is.na(res$CHR) | is.na(res$CHRLOC) | is.na(res$CHRLOCEND)
		res = res[!drop,]
		
		gr = GRanges(seqnames=paste0("chr",res$CHR),
				strand=ifelse(res$CHRLOC>0, "+","-"),
				ranges=IRanges(start=abs(res$CHRLOC), end=abs(res$CHRLOCEND)))
		
		if (any(!(columns %in% colnames(exprs(object)))))
			stop("'columns' not found on 'exprs(object)'")

		mcols(gr)[,"SYMBOL"] = res$SYMBOL
		mcols(gr)[,"PROBEID"] = res$PROBEID

		mat <- exprs(object)[!drop,columns]
		colnames(mat) <- columns

		sumexp <- SummarizedExperiment(assays=SimpleList(mat),
									  rowData=GIntervalTree(gr),
									  colData=DataFrame(pData(object)[columns,]))

		register(sumexp, columns=columns, assay=1)
})


