#' epiviz interactive track manager
#' 
#' This class is used to add and delete interactive devices in the epiviz browser. It inherits
#' from the \link{environment} class, so \link{get} and \link{set} methods exist (but not recommended 
#' for use). Setters and getters are defined for the pertinent slots. 
#' 
#' @section Fields:
#' \describe{
#'  \item{\code{devices}:}{A list of \link{epivizDevice} objects defining currently loaded}
#'  \item{\code{idCounter}:}{id generator}
#'  \item{\code{activeId}:}{ID of currently active device}
#'  \item{\code{server}:}{An environment implementing a websockets server}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{isClosed}:}{True if websocket is closed for this manager}
#'  \item{\code{stop}:}{Stop communiation and close websocket server}
#'  \item{\code{addDevice}:}{Add a \link{epivizDevice} object to the list of devices}
#'  \item{\code{delDevice}:}{Remove device from list}
#'  \item{\code{setActive}:}{Set device as active (for navigation on browser)}
#'  \item{\code{listDevices}:}{List current devices}
#'  \item{\code{getData}:}{Get data from devices}
#'  \item{\code{refresh}:}{Refresh epiviz browser}
#'  \item{\code{navigate}:}{Navigate to given genomic region in browser}
#' }
#' 
#' @name EpivizDeviceMgr-class
#' @rdname EpivizDeviceMgr-class
#' 
#' @export
EpivizDeviceMgr <- setRefClass("EpivizDeviceMgr", 
  fields=list(
    devices="list",
    idCounter="integer",
    activeId="character",
    activeType="character",
    server="environment"),
  methods=list(
   isClosed=function() {
     'check if connection is closed'
    !exists("server_socket", server) || is.null(server$server_socket)
   },
   stop=function() {
     'stop epiviz connection'
     websocket_close(server)
     invisible(server)
   },
   addDevice=function(device, devName) {
     'add device to epiviz browser'
     if (!is(device, "EpivizDevice"))
       stop("device must be of class EpivizDevice")
     if (.self$isClosed())
       stop("manager connection is closed")
     curNames = c(names(devices$gene), names(devices$bp), names(devices$block))
     if (devName %in% curNames)
       stop("device name already in use")
     
     idCounter <<- idCounter + 1L
     devId <- sprintf("epivizDev_%d", .self$idCounter)
     .makeRequest_addDevice(devId, .self$server, device, devName)
     devRecord=list(name=devName, obj=device)
     if (is(device,"EpivizGeneDevice")) {
       devices$gene[[devId]] <<- devRecord
       activeType <<- "gene"
     } else if (is(device,"EpivizBpDevice")) {
       devices$bp[[devId]] <<- devRecord
       activeType <<- "bp"
     } else if (is(device, "EpivizBlockDevice")) {
       devices$block[[devId]] <<- devRecord
       activeType <<- "block"
     } else {
       stop("Unkown device class")
     }
     activeId <<- devId
     return(devId)
   },
   delDevice=function(devId) {
     'delete device from epiviz browser'
     if (is.null(devices[[activeType]][[devId]]))
       stop("device Id not found")
     devices[[activeType]][[devId]] <<- NULL
     if (length(devices[[activeType]])==0) {
       activeId <<- ""
       activeType <<- ""
     } else {
       ids=names(devices[[activeType]])
       activeId <<- ids[length(ids)]
     }
     invisible(NULL)
   },
   setActive=function (devId) {
     'set given device as active in browser'
     if (devId %in% names(devices$block)) {
       activeId <<- devId
       activeType <<- "block"
     } else if (devId %in% names(devices$bp)) {
       activeId <<- devId
       activeType <<- "bp"
     } else if (devId %in% names(devices$gene)) {
       activeId <<- devId
       activeType <<- "gene"
     } else {
       stop("device Id not found")  
     }
     invisible(NULL)
   },
   listDevices=function() {
     'list devices in browser'
     .doOneList <- function(devs) {
        ids=names(devs)
        nms=sapply(devs, "[[", "name")
        lens=sapply(devs, function(x) length(x$obj$gr))
        active=ifelse(ids==activeId, "*","")
        data.frame(id=ids,
                    active=active,
                    name=nms,
                    length=lens,
                    stringsAsFactors=FALSE,row.names=NULL)  
     }
     out <- list()
     if (length(devices$block)>0) {
       out$block <- .doOneList(devices$block)
     }
     if (length(devices$bp)>0) {
       out$bp <- .doOneList(devices$bp)
     }
     if (length(devices$gene)>0) {
       out$gene <- .doOneList(devices$gene)
     }
     return(out)
   },
   getData=function(devId, chr, start, end) {
     if (!is.null(devId)) {
       if (is.null(devices[[devId]]))
         stop("device Id not found")
       
       dev=devices[[devId]]
       out=list(dev$obj$getData(chr,start,end))
       names(out)=devId
     } else {
       out=lapply(devices, function(dev) dev$obj$getData(chr=chr,start=start,end=end))
       names(out)=names(devices)
     }
     out   
   },
   refresh=function() {
     'refresh browser'
     .makeRequest_refresh(server)
     invisible(NULL)
   },
   navigate=function(chr, start, end) {
     'navigate to given position'
     .makeRequest_navigate(server, chr=chr, start=start, end=end)
     invisible(NULL)
   },
   data_received=function(DATA, WS, HEADER) {
     message("mgr: data received")
     print(rawToChar(DATA))
   },
   con_established=function(WS) {
     message("mgr: connection established")
     websockets::websocket_write("Hello There!", WS)
   },
   con_closed=function(WS) {
     message("mgr: one connection closed")
   }
  )
)

#' Start the epiviz interface
#' 
#' Create an epiviz device manager which can be used to add and delete tracks in browser
#' 
#' @param port (integer) the port for the websocket server
#' @param localURL (character) use this url for the epiviz server instead of the standard remote URL
#' @param chr (character) chromosome to browse to on startup
#' @param start (integer) start position to browse to on startup
#' @param end (integer) end position to browse to on startup
#' @param debug (logical) start the epiviz browser in debug mode
#' 
#' @return an object of class \linkS4class{EpivizDeviceMgr}.
#' 
#' @examples
#' mgr <- startEpiviz()
#' mgr$stop()
#' 
#' @export
startEpiviz <- function(port=7312L, localURL=NULL, chr="chr11", start=99800000, end=103383180, debug=FALSE) {
  message("Opening websocket...")
  mgr <- EpivizDeviceMgr$new(
    #TODO: tryCatch statement trying different ports
    server=websockets::create_server(port=port),
    idCounter=0L,
    activeId="",
    activeType="",
    devices=list(gene=list(),bp=list(),block=list())
  )
  websockets::setCallback("receive", mgr$data_received, mgr$server)
  websockets::setCallback("established", mgr$con_established, mgr$server)
  websockets::setCallback("closed", mgr$con_closed, mgr$server)
  
  #TODO: add tryCatch statement?
  daemonize(mgr$server)
  
  if (missing(localURL) || is.null(localURL)) {
    url="http://epiviz.cbcb.umd.edu"
  } else {
    url=localURL
  }
  
  controllerHost=sprintf("ws://localhost:%d", port)
  url=sprintf("%s/index.php?chr=%s&start=%d&end=%d&controllerHost=%s&debug=%s&",
              url,
              chr,
              as.integer(start),
              as.integer(end),
              controllerHost,
              ifelse(debug,"true","false"))
  message("Opening browser...")
  browseURL(url)
  return(mgr)
}

