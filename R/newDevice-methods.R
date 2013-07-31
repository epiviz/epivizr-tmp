setGeneric("newDevice", signature=c("object"), 
	function(object, columns=NULL, ...) standardGeneric("newDevice"))

setMethod("newDevice", "GRanges",
	function(object, columns, type=c("block","bp")) {
		type <- match.arg(type)
		dev <- switch(type,
					  block=EpivizBlockDevice$new(object=object, columns=columns),
					  bp=EpivizBpDevice$new(object=object, columns=columns))
		return(dev)
})

setMethod("newDevice", "ExpressionSet",
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


		# wanted to do this, but got error, couldn't figure out why
		# mcols(gr)[,columns] <- exprs(object)[!drop,columns]
		for (col in columns) mcols(gr)[[col]] <- exprs(object)[!drop,col]

		mcols(gr)[,"SYMBOL"] = res$SYMBOL
		mcols(gr)[,"PROBEID"] = res$PROBEID
		
		EpivizFeatureDevice$new(object=gr, columns=columns)
})


setMethod("newDevice", "SummarizedExperiment",
	function(object, columns, assay=1) {
	
	gr <- rowData(object)
	mat <- assay(object, assay)

	if (any(!columns %in% colnames(mat)))
		stop("'columns' not found on 'assay(object)'")

	for (col in columns) mcols(gr)[[col]] <- mat[,col]
	mcols(gr)[,"SYMBOL"] = ""

	if (!is.null(rownames(object))) {
		mcols(gr)[,"PROBEID"] = rownames(object)
	} else {
		mcols(gr)[,"PROBEID"] = ""
	}
	
	EpivizFeatureDevice$new(object=gr, columns=columns)
})

