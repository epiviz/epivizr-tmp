#' epiviz interactive track manager
#' 
#' This class is used to add and delete interactive devices in the epiviz browser. 
#' Setters and getters are defined for the pertinent slots. 
#' 
#' @section Fields:
#' \describe{
#'  \item{\code{devices}:}{A list of \linkS4class{EpivizDevice} objects defining currently loaded}
#'  \item{\code{idCounter}:}{id generator}
#'  \item{\code{activeId}:}{ID of currently active device}
#'  \item{\code{server}:}{An environment implementing a websockets server}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{isClosed}:}{True if websocket is closed for this manager}
#'  \item{\code{stop}:}{Stop communiation and close websocket server}
#'  \item{\code{addDevice}:}{Add a \linkS4class{EpivizDevice} object to the list of devices}
#'  \item{\code{delDevice}:}{Remove device from list}
#'  \item{\code{setActive}:}{Set device as active (for navigation on browser)}
#'  \item{\code{listDevices}:}{List current devices}
#'  \item{\code{getData}:}{Get data from devices}
#'  \item{\code{refresh}:}{Refresh epiviz browser}
#'  \item{\code{navigate}:}{Navigate to given genomic region in browser}
#' }
#' 
#' @param ... arguments passed to constructor
#' 
#' @aliases EpivizDeviceMgr
#' 
#' @name EpivizDeviceMgr-class
#' @rdname EpivizDeviceMgr-class
#' 
#' @exportClass EpivizDeviceMgr
EpivizDeviceMgr <- setRefClass("EpivizDeviceMgr", 
  fields=list(
    url="character",
    devices="list",
    msList="list",
    typeMap="list",
    msIdCounter="integer",
    activeId="character",
    chartIdMap="list",
    server="EpivizServer",
    callbackArray="IndexedArray"),
  methods=list(
    initialize=function(...) {
     msIdCounter <<- 0L
     activeId <<- ""
     chartIdMap <<- list()
     typeMap <<- .typeMap
     devices <<- structure(lapply(seq_along(.typeMap), function(x) list()),names=names(.typeMap))
     msList <<- structure(lapply(seq_along(.typeMap), function(x) list()),names=names(.typeMap))
     callSuper(...)
   })
)

# session management methods
EpivizDeviceMgr$methods(list(
   bindToServer=function() {
     server$bindManager(.self)
   },
   finalize=function() {
     stopServer()
   },
   isClosed=function() {
     'check if connection is closed'
     server$isClosed()
   },
   openBrowser=function(url=NULL) {
     if (missing(url) || is.null(url)) {
       browseURL(.self$url)
     } else {
       browseURL(url)
     }
     server$startServer()
     service()
   },
   service=function() {
     message("Serving Epiviz, escape to continue interactive session...")
     server$service()
   },
   stopService=function() {
     server$stopService()
   },
   startServer=function() {
     server$startServer()
   },
   stopServer=function() {
     'stop epiviz connection'
     .self$rmAllDevices()
     server$stopServer()
   },
   show=function() {
      cat("Epiviz device manager object.\n")
      cat("Current devices:\n")
      print(listDevices()); cat("\n")
      listTypes()
   })
)

# measurement management methods
EpivizDeviceMgr$methods(list(
   addMeasurements=function(obj, msName, sendRequest=TRUE, ...) {
    'add measurements to epiviz session'
    epivizObject <- epivizr:::register(obj, ...)
    type <- .self$getMeasurementType(class(epivizObject))

    msIdCounter <<- msIdCounter + 1L
    msId <- sprintf("epivizMs_%s_%d", type, msIdCounter)
    epivizObject$setId(msId)
    epivizObject$setMgr(.self)

    measurements <- epivizObject$getMeasurements(msName, msId)
    msRecord <- list(measurements=names(measurements), name=msName, obj=epivizObject, connected=FALSE)

    msList[[type]][[msId]] <<- msRecord

    if (sendRequest) {
      callback <- function(data) {
        msList[[type]][[msId]][["connected"]] <<- TRUE
        message("Measurement ", msName, " added to browser and connected")
      }
      requestId <- callbackArray$append(callback)

      # TODO: add addMeasurement method to server
      server$addMeasurements(requestId, type, measurements) 
    }
    return(epivizObject)
   },
   updateMeasurements=function(device, gr) {
     devId <- device$id
     slot <- which(sapply(devices, function(x) devId %in% names(x)))
     if (length(slot)<1)
       stop("device not recognized by manager")

     device$update(gr)
     
     devType <- names(typeMap)[slot]
     devName <- devices[[devType]][[devId]]$name
     
     chartId <- chartIdMap[[devId]]
     if (!is.null(chartId)) {
       callback=function(data) {
         message("cache for device ", devName, " cleared")
       }
       requestId=callbackArray$append(callback)
       server$clearCache(requestId, chartId) 
     }
     invisible()
   },
   rmDevice=function(device) {
     'delete device from epiviz browser'
     devId <- device$id
     slot=which(sapply(devices, function(x) devId %in% names(x)))
     if (length(slot)<1)
       stop("device Id not found")
     
     devType=names(typeMap)[slot]
     devName=devices[[devType]][[devId]]$name
     measurements=devices[[devType]][[devId]]$measurements
     
     devices[[devType]][[devId]] <<- NULL
     
     if (activeId == devId) {
       message("removed active device, use setActive to make another device the active device")
       activeId <<- ""
     }
     
     if(!is.null(chartIdMap[[devId]])) {
       chartId=chartIdMap[[devId]]
       chartIdMap[[devId]] <<- NULL
       callback=function(data) {
         message("device ", devName, " removed and disconnected")  
       }
       requestId=callbackArray$append(callback)
       server$rmDevice(requestId, chartId, measurements, devType)
     }
     invisible(NULL)
   },
   rmAllDevices=function() {
    'removes all current devices'
    for (i in seq_along(typeMap)) {
      curType=names(typeMap)[i]
      if (length(devices[[curType]])>0) {
        for (dev in devices[[curType]]) {
          rmDevice(dev$obj)
        }
      }
    }
   },
   listDevices=function() {
     'list devices in browser'
     .doOneList <- function(devs) {
        ids=names(devs)
        nms=sapply(devs, "[[", "name")
        lens=sapply(devs, function(x) length(x$obj$object))
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
            curMs=paste0(devices[[curType]][[i]]$name, "$", devices[[curType]][[i]]$obj$columns)
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
         
         ids=ids[ids %in% sapply(devices$block, "[[", "measurements")]
         if (length(ids)>0)
            curOut[ids] = lapply(devices$block[ids], .getFromOneDevice)
         out[[dataName]]$data=curOut
       } else if (dataType=="bpMeasurements") {
         ids=measurements[[dataType]]
         
         out[[dataName]]$min=structure(rep(-6,length(ids)),names=ids)
         out[[dataName]]$max=structure(rep(6,length(ids)), names=ids)
         out[[dataName]]$data=structure(vector("list",length(ids)), names=ids)
         for (j in seq_along(ids)) {
           out[[dataName]]$data[[j]]=list(bp=integer(),value=numeric())
         }
         if (length(ids)>0) {
           theMeasurements=lapply(devices[[devType]],"[[","measurements")
           devIndexes=sapply(ids, function(id) which(sapply(theMeasurements, function(x) id %in% x)))
           devIds=split(seq_along(ids),names(devices[[devType]])[devIndexes])
           
           for (j in seq_along(devIds)) {
              curDevId=names(devIds)[j]
              dev=devices[[devType]][[curDevId]]
           
              ind=devIds[[j]]
              curCols=dev$obj$columns[match(ids[ind],dev$measurements)]
              
              tmp=.getFromOneDevice(dev, columnsRequested=curCols)
              out[[dataName]]$min[ind]=tmp$min
              out[[dataName]]$max[ind]=tmp$max
              out[[dataName]]$data[ind]=tmp$data
            }
          }
       } else {
         ids <- measurements[[dataType]]
         out[[dataName]]$min=structure(rep(-6,length(ids)),names=ids)
         out[[dataName]]$max=structure(rep(6,length(ids)),names=ids)
         out[[dataName]]$data=structure(vector("list",length(ids)+4),names=c("gene","start","end","probe",ids))
         out[[dataName]]$data$gene=character()
         out[[dataName]]$data$start=integer()
         out[[dataName]]$data$end=integer()
         out[[dataName]]$data$probe=character()
         for (j in seq_along(ids)) {
          out[[dataName]]$data[[j+4]]=numeric()
         }
         if (length(ids)>0) {
          theMeasurements=lapply(devices[[devType]],"[[","measurements")
          devIndexes=sapply(ids, function(id) which(sapply(theMeasurements, function(x) id %in% x)))
          devIds=split(seq_along(ids), names(devices[[devType]])[devIndexes])

          for (j in seq_along(devIds)) {
            curDevId=names(devIds)[j]
            dev=devices[[devType]][[curDevId]]

            ind=devIds[[j]]
            curCols=dev$obj$columns[match(ids[ind],dev$measurements)]
            tmp=.getFromOneDevice(dev, columnsRequested=curCols)

            out[[dataName]]$min[ind]=tmp$min
            out[[dataName]]$max[ind]=tmp$max

            if (length(out[[dataName]]$data$gene)==0) {
              out[[dataName]]$data$gene=tmp$data$gene
              out[[dataName]]$data$start=tmp$data$start
              out[[dataName]]$data$end=tmp$data$end
              out[[dataName]]$data$probe=tmp$data$probe
            }
            out[[dataName]]$data[ind+4]=tmp$data[-(1:4)]
          }
         }
       }
     }
    return(out)
   }
   )
)

EpivizDeviceMgr$methods(list(
  getMeasurementType=function(objClass) {
    m <- match(objClass, sapply(typeMap, "[[", "class"))
    if (is.na(m))
      stop("Class ", objClass, " not found in 'typeMap'")
    names(typeMap)[m]
  })
)

.typeMap <- list(gene=list(class="EpivizFeatureDevice",
                           description="Scatterplot indexed by probeid",
                           input_class="ExpressionSet"),
              bp=list(class="EpivizBpDevice",
                      description="Basepair resolution line plot",
                      input_class="GRanges"),
              block=list(class="EpivizBlockDevice",
                         description="Region plot",
                         input_class="GRanges"))


# chart management methods
EpivizDeviceMgr$methods(list(
   addChart=function(msObject, ...) {
     # TODO: move this code to an addDevice method
     # device <- epivizr:::newDevice(obj, ...)
     # type = getDeviceType(class(device))

     # idCounter <<- idCounter + 1L
     # devId <- sprintf("epivizDev_%s_%d", type, idCounter)
     # device$setId(devId)
     # device$setMgr(.self)
     
     # measurements = device$getMeasurements(devName, devId)
     
     # devRecord=list(measurements=names(measurements), name=devName, obj=device)
     # devices[[type]][[devId]] <<- devRecord
     # activeId <<- devId
     
     # if (sendRequest) {
     #   callback=function(data) {
     #     trkId = data$id
     #     chartIdMap[[devId]] <<- trkId
     #     message("Device ", devName, " added to browser and connected")
     #   }
     #   requestId=callbackArray$append(callback)
     #   server$addDevice(requestId, type, measurements)
     # } 
     # return(device)
   },  
   setActive=function (devId) {
     'set given device as active in browser'
     slot=which(sapply(lapply(devices,names), function(x) devId %in% x))
     if (length(slot)<1)
       stop("device Id not found")
     activeId <<- devId
     invisible(NULL)
   },
   plot=function(obj, devName, sendRequest=TRUE, ...) {
     'add device to epiviz browser'
     msObject <- addMeasurement(obj, devName, sendRequest=sendRequest, ...)

     # TODO: implement EpivizDataTypes class hierarchy
     # TODO: to mirror the JS data type class hierarchy
     # TODO: implement plot method for EpivzDataTypes
     devObject <- msObject$plot(sendRequest=sendRequest, ...)

     # TODO: implement EpivizDevice class hierarchy
     # TODO: to mirror JS chart type class hierarchy
     devId <- devObject$getId()
     activeId <<- devId
     return(devObject)
   }
   )
)

.chartTypeMap <- list()

# navigation methods
EpivizDeviceMgr$methods(list(
   refresh=function() {
     'refresh browser'
     server$refresh()
   },
   navigate=function(chr, start, end) {
     'navigate to given position'
     server$navigate(chr=chr,start=start,end=end)
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
#' @param proxy (logical) start the epiviz browser in proxy mode
#' @param openBrowser (logical) browse to the epiviz URL
#' 
#' @return an object of class \linkS4class{EpivizDeviceMgr}.
#' 
#' @examples
#' 
#' mgr <- startEpiviz(openBrowser=FALSE)
#' mgr$startServer()
#' mgr$stopServer()
#' 
#' @export
startEpiviz <- function(port=7312L, localURL=NULL, chr="chr11", start=99800000, end=103383180, 
                        debug=FALSE, proxy=TRUE, workspace=NULL, openBrowser=TRUE) {
  message("Opening websocket...")
  server <- epivizr:::createServer(port=port)
  
  if (missing(localURL) || is.null(localURL)) {
    url="http://epiviz.cbcb.umd.edu/index.php"
  } else {
    url=localURL
  }
  
  controllerHost=sprintf("ws://localhost:%d", port)
  url=sprintf("%s?controllerHost=%s&debug=%s&proxy=%s&", 
              url,
              controllerHost,
              ifelse(debug,"true","false"),
              ifelse(proxy,"true","false"))
  
  if (!is.null(workspace)) {
    url=paste0(url,"workspace=",workspace,"&")
  } else {
    url=paste0(url,
               sprintf("chr=%s&start=%d&end=%d&",
                       chr,
                       as.integer(start),
                       as.integer(end)))
  }
  tryCatch({
    mgr <- EpivizDeviceMgr$new(server=server, url=url)
    mgr$bindToServer()
  }, error=function(e) {
    server$stopServer()
    stop("Error starting Epiviz: ", e)
  })
  
  if (openBrowser) {
  tryCatch({
      message("Opening browser...")
      mgr$openBrowser(url)
    }, error=function(e) {
              mgr$stopServer()
              stop("Error starting Epiviz: ", e)
    }, interrupt=function(e) {NULL})
  }
  return(mgr)
}

