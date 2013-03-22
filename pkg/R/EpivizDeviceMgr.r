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
   
     curNames = unlist(lapply(devices,names))
     if (devName %in% curNames)
       stop("device name already in use")
     
     idCounter <<- idCounter + 1L
     devId <- sprintf("epivizDev_%d", .self$idCounter)
     .makeRequest_addDevice(devId, .self$server, device, devName)
     devRecord=list(name=devName, obj=device)
     
     slot=match(class(device), sapply(typeMap, "[[", "class"))
     if (is.na(slot)) {
       stop("Unkown device type class")
     }
     
     devices[[slot]][[devId]] <<- devRecord
     activeType <<- names(typeMap)[slot]
     activeId <<- devId
     return(devId)
   },
   delDevice=function(devId) {
     'delete device from epiviz browser'
     slot=which(sapply(devices, function(x) devId %in% names(x)))
     if (length(slot)<1)
       stop("device Id not found")
     
     devType=names(typeMap)[slot]
     devices[[devType]][[devId]] <<- NULL
     
     if (activeId == devId) {
       if (length(devices[[devType]])==0) {
          activeId <<- ""
          activeType <<- ""
       } else {
         ids=names(devices[[devType]])
         activeId <<- ids[length(ids)]  
       }
     }  
     invisible(NULL)
   },
   setActive=function (devId) {
     'set given device as active in browser'
     slot=which(sapply(lapply(devices,names), function(x) devId %in% x))
     if (length(slot)<1)
       stop("device Id not found")
     activeType <<- names(typeMap)[slot]
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
        data.frame(id=ids,
                    active=active,
                    name=nms,
                    length=lens,
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
       if (length(devices[[curType]])>0) {
         nm=paste0(curType,"Measurements")
         if (curType=="block") {
           measurements=names(devices$block)
         } else {
          measurements=character()
          for (i in seq_along(devices[[curType]])) {
            curMs=paste0(names(devices[[curType]])[i], "$", devices[[curType]][[i]]$obj$mdCols)
            measurements=c(measurements,curMs)
          }
         }
         out[[nm]]=measurements
       }
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
    activeType="",
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

