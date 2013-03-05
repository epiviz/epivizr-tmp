setGeneric("isClosed",
           function(obj) standardGeneric("isClosed"),
           signature=c("obj"))

setGeneric("epivizClose",
           function(obj) standardGeneric("epivizClose"),
           signature=c("obj"))

setGeneric("addTrackData",
           function(obj, tablename, granges, order.by) standardGeneric("addTrackData"),
           signature=c("obj","tablename","granges"))

setGeneric("deleteTrackData",
           function(obj, tablename) standardGeneric("deleteTrackData"),
           signature=c("obj","tablename"))

setGeneric("updateMgrBackendData",
           function(obj, df) standardGeneric("updateMgrBackendData"),
           signature=c("obj","df"))
