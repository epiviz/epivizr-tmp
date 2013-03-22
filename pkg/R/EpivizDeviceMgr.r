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
    typeMap="list",
    idCounter="integer",
    activeId="character",
    chartIdMap="list",
    server="environment",
    callbackArray="IndexedArray"),
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
   addDevice=function(gr, devName, sendRequest=TRUE, ...) {
     'add device to epiviz browser'
     if (!is(gr, "GenomicRanges"))
       stop("gr must be of class 'GenomicRanges'")
     if (.self$isClosed())
       stop("manager connection is closed")
     
     device = newDevice(gr, ...)
     slot=match(class(device), sapply(typeMap, "[[", "class"))
     if (is.na(slot)) {
       stop("Unkown device type class")
     }
     
     type=names(typeMap)[slot]
     idCounter <<- idCounter + 1L
     devId <- sprintf("epivizDev_%s_%d", type, .self$idCounter)
     if (type == "block") {
       measurements=devId
     } else {
       measurements=paste0(devId,"$",device$mdCols)
     }
     devRecord=list(measurements=measurements, name=devName, obj=device)
     devices[[slot]][[devId]] <<- devRecord
     activeId <<- devId
     
     if (sendRequest) {
       callback=function(data) {
         trkId = data$id
         chartIdMap[[devId]] <<- trkId
         message("Device ", devName, " added to browser and connected")
       }
       requestId=callbackArray$append(callback)
      .makeRequest_addDevice(devId, .self$server, device, devName, requestId)
     } 
     return(devId)
   },
   finishAddDevice=function(devId, devName) {
     function(data) {
       
     }
   },  
   rmDevice=function(devId) {
     'delete device from epiviz browser'
     slot=which(sapply(devices, function(x) devId %in% names(x)))
     if (length(slot)<1)
       stop("device Id not found")
     
     devType=names(typeMap)[slot]
     devName=devices[[devType]][[devId]]$name
     devices[[devType]][[devId]] <<- NULL
     
     if (activeId == devId) {
       message("removed active device, use setActive to make another device the active device")
       activeId <<- ""
     }
     
     if(!is.null(chartIdMap[[devId]])) {
       chartId=chartIdMap[[devId]]
       chartIdMap[[devId]] <<- NULL
       .makeRequest_rmDevice(chartIdMap[[devId]], mgr$finishRmDevice(devName))
     }
     invisible(NULL)
   },
   finishRmDevice=function(devName) {
     function() {
       message("device", devName, "removed and disconnected")
     }
   },
   setActive=function (devId) {
     'set given device as active in browser'
     slot=which(sapply(lapply(devices,names), function(x) devId %in% x))
     if (length(slot)<1)
       stop("device Id not found")
     activeId <<- devId
     invisible(NULL)
   },
   listDevices=function() {
     'list devices in browser'
     .doOneList <- function(devs) {
        ids=names(devs)
        nms=sapply(devs, "[[", "name")
        lens=sapply(devs, function(x) length(x$obj$gr))
        active=ifelse(ids==activeId, "*","")
        connected=ifelse(ids %in% names(chartIdMap), "*", "")
        data.frame(id=ids,
                    active=active,
                    name=nms,
                    length=lens,
                    connected=connected,
                    stringsAsFactors=FALSE,row.names=NULL)  
     }
     out <- list()
     for (i in seq_along(typeMap)) {
       curType=names(typeMap)[i]
       if (length(devices[[curType]])>0)
         out[[curType]] <- .doOneList(devices[[curType]])
     }
     return(out)
   },
   getMeasurements=function() {
     out=list()
     for (i in seq_along(.typeMap)) {
      curType=names(.typeMap)[i]
      nm=paste0(curType,"Measurements")
      measurements=list()
       
      if (length(devices[[curType]])>0) {
        if (curType=="block") {
           measurements=lapply(devices$block, "[[", "name")
           names(measurements)=names(devices$block)
        } else {
          for (i in seq_along(devices[[curType]])) {
            curMs=paste0(devices[[curType]][[i]]$name, "$", devices[[curType]][[i]]$obj$mdCols)
            measurements[devices[[curType]][[i]]$measurements]=curMs
          }
        }
      }
      out[[nm]]=measurements 
     }
     return(out)
   },
   getData=function(measurements, chr, start, end) {
     .getFromOneDevice  <- function(dev, ...) dev$obj$getData(chr=chr,start=start,end=end, ...)
     out <- list(chr=chr,start=start,end=end)
     
     for (i in seq_along(measurements)) {
       dataType=names(measurements)[i]
       dataName=gsub("Measurements","Data", dataType)
       devType = gsub("Measurements","", dataType)
       
       out[[dataName]]=list(start=start,end=end,chr=chr)
       
       if (dataType=="blockMeasurements") {
         ids=measurements$blockMeasurements
         curOut=lapply(seq_along(ids), function(i) list(start=integer(),end=integer()))
         names(curOut)=ids
         
         ids=ids[ids %in% names(devices$block)]
         if (length(ids)>0)
            curOut[ids] = lapply(devices$block[ids], .getFromOneDevice)
         out[[dataName]]$data=curOut
       } else {
         ids=measurements[[dataType]]
         out[[dataName]]$min=structure(rep(-6,length(ids)),names=ids)
         out[[dataName]]$max=structure(rep(6,length(ids)), names=ids)
         out[[dataName]]$data=structure(vector("list",length(ids)), names=ids)
         for (j in seq_along(ids)) {
           out[[dataName]]$data[[i]]=list(bp=integer(),value=numeric())
         }
         if (length(ids)>0) {
            splitIds=strsplit(ids, split="\\$")
            splitIds=data.frame(device=sapply(splitIds,"[",1), col=sapply(splitIds,"[",2),stringsAsFactors=FALSE)
            devIds=unique(splitIds$device)
            devIds=devIds[devIds %in% names(devices[[devType]])]
         
            for (j in seq_along(devIds)) {
              curDevId=devIds[j]
              dev=devices[[devType]][[curDevId]]
           
              ind=which(splitIds$device==curDevId)
              curCols=splitIds$col[ind]
              tmp = which(curCols %in% dev$obj$mdCols)
           
              ind=ind[tmp]
              curCols=curCols[tmp]
           
              tmp=.getFromOneDevice(dev, cols=curCols)
              out[[dataName]]$min[ind]=tmp$min
              out[[dataName]]$max[ind]=tmp$max
              out[[dataName]]$data[ind]=tmp$data
            }
         }
       }
       
       
     }
     return(out)
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
   data_handler=function() {
     .generate_handler(.self)
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
startEpiviz <- function(port=7312L, localURL=NULL, chr="chr11", start=99800000, end=103383180, debug=FALSE, openBrowser=TRUE) {
  message("Opening websocket...")
  devs=
  mgr <- EpivizDeviceMgr$new(
    #TODO: tryCatch statement trying different ports
    server=websockets::create_server(port=port),
    idCounter=0L,
    activeId="",
    chartIdMap=list(),
    typeMap=.typeMap,
    devices=structure(lapply(seq_along(.typeMap), function(x) list()),names=names(.typeMap))
  )
  tryCatch({
    websockets::setCallback("receive", mgr$data_handler(), mgr$server)
    websockets::setCallback("established", .con_established, mgr$server)
    websockets::setCallback("closed", .con_closed, mgr$server)
  
    #TODO: add tryCatch statement?
    daemonize(mgr$server)
  
    if (missing(localURL) || is.null(localURL)) {
      url="http://epiviz.cbcb.umd.edu/index.php"
    } else {
      url=localURL
    }
  
    controllerHost=sprintf("ws://localhost:%d", port)
    url=sprintf("%s?chr=%s&start=%d&end=%d&controllerHost=%s&debug=%s&",
                url,
                chr,
                as.integer(start),
                as.integer(end),
                controllerHost,
                ifelse(debug,"true","false"))
    
    if (openBrowser) {
      message("Opening browser...")
      browseURL(url)
    }
  }, error=function(e) {
    mgr$close()
  })
  return(mgr)
}

