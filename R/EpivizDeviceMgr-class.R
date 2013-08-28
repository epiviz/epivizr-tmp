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
    chartList="list",
    chartIdCounter="integer",
    activeId="character",
    chartIdMap="list",
    deviceList="list",
    deviceIdCounter="integer",
    server="EpivizServer",
    verbose="logical",
    callbackArray="IndexedArray"),
  methods=list(
    initialize=function(...) {
     msIdCounter <<- 0L
     chartIdCounter <<- 0L
     deviceIdCounter <<- 0L
     activeId <<- ""
     chartIdMap <<- list()
     typeMap <<- .typeMap
     devices <<- structure(lapply(seq_along(.typeMap), function(x) list()),names=names(.typeMap))
     msList <<- structure(lapply(seq_along(.typeMap), function(x) list()),names=names(.typeMap))
     chartList <<- list()
     deviceList <<- list()
     verbose <<- FALSE
     callSuper(...)
   },
   show=function() {
      cat("Epiviz device manager object.\n")
      cat("Charts:\n")
      print(listCharts()); cat("\n")
      cat("Measurements:\n")
      print(listMeasurements()); cat("\n")
      cat("Devices:\n")
      print(listDevices()); cat("\n")
#      listTypes()
   }
  )
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
     .self$rmAllCharts(which="all")
     .self$rmAllMeasurements(which="all")
     server$stopServer()
   }
  )
)

# navigation methods
EpivizDeviceMgr$methods(list(
   refresh=function() {
     'refresh browser'
     server$refresh()
   },
   navigate=function(chr, start, end) {
     'navigate to given position'
     callback <- function(data) {
      invisible(NULL)
     }
     requestId <- callbackArray$append(callback)
     server$navigate(requestId=requestId,chr=chr,start=start,end=end)
   },
   slideshow=function(granges, n=10) {
    'navidate to successive positions'
    if (!is(granges, "GenomicRanges"))
      stop(("'granges' must be a 'GenomicRanges' object"))

    ind <- seq(len=n)
    chr <- as.character(seqnames(granges)[ind])
    start <- start(granges)[ind]
    end <- end(granges)[ind]
    for (i in ind) {
      cat("Region", i, "of", n, ". Press key to continue (ESC to stop)...\n")
      readLines(n=1)
      navigate(chr=chr[i], start=start[i], end=end[i])
      tryCatch(service(), interrupt=function(int) invisible(NULL))
    }
    invisible(NULL)
   }
  )
)

# measurement management methods
.typeMap <- list(gene=list(class="EpivizFeatureData",
                           description="Data indexed by feature",
                           input_class="SummarizedExperiment"),
              bp=list(class="EpivizBpData",
                      description="Basepair resolution data",
                      input_class="GRanges"),
              block=list(class="EpivizBlockData",
                         description="Genomic region data",
                         input_class="GRanges"))

EpivizDeviceMgr$methods(
   addMeasurements=function(obj, msName, sendRequest=TRUE, ...) {
    'add measurements to epiviz session'
    epivizObject <- epivizr:::register(obj, ...)
    type <- .self$getMeasurementType(epivizObject)

    msIdCounter <<- msIdCounter + 1L
    msId <- sprintf("epivizMs_%s_%d", type, msIdCounter)
    epivizObject$setId(msId)
    epivizObject$setName(msName)
    epivizObject$setMgr(.self)

    measurements <- epivizObject$getMeasurements()
    msRecord <- list(measurements=names(measurements), 
      name=msName, obj=epivizObject, connected=FALSE)
    msList[[type]][[msId]] <<- msRecord

    if (sendRequest) {
      callback <- function(data) {
        msList[[type]][[msId]][["connected"]] <<- TRUE
        message("Measurement ", msName, " added to browser and connected")
      }
      requestId <- callbackArray$append(callback)
      server$addMeasurements(requestId, type, measurements) 
    }
    return(epivizObject)
   },
   .findMeasurements=function(msType, ms) {
      typeList <- msList[[msType]]
      allMeasurements <- lapply(typeList, "[[", "measurements")
      m <- sapply(ms, function(curMs) {
        isFound <- sapply(allMeasurements, function(x) curMs %in% x)
        if (any(isFound)) which(isFound) else NA
      })  
   },
   .checkMeasurements=function(msType, ms, sendRequest=TRUE, ...) {
    if (!is.character(ms)) return(FALSE)
    if (!(msType %in% names(msList))) return(FALSE)

    m <- .findMeasurements(msType, names(ms))
    if (any(is.na(m)))
      return(FALSE)

    if (sendRequest) {
      typeList <- msList[[msType]]
      isConnected <- sapply(typeList, "[[", "connected")[m]
      all(isConnected)
    } else {
      TRUE
    }
   },
   .clearChartCaches=function(msObj, sendRequest=TRUE) {
     if(!is(msObj, "EpivizData")) {
      stop("'msObj' must be an 'EpivizData' object")
     }
     msType <- getMeasurementType(msObj)
     msIndex <- match(msObj$getId(), names(msList[[msType]]))
     if (is.na(msIndex)) 
      stop("did not find object")

     if (msList[[msType]][[msIndex]]$connected) {
       chartIds <- c()
       for (chart in chartList) {
        if (is.null(chartIdMap[[chart$getId()]]))
          next

        m <- .findMeasurements(msType, names(chart$measurements))
        if (!(msIndex %in% m))
          next

        chartId <- chartIdMap[[chart$getId()]]
        if (!(chartId %in% chartIds))
          chartIds <- c(chartIds, chartId)
       }

       if (sendRequest && length(chartIds)>0) {
        callback=function(data) {
          message("Chart caches cleared")
        }
        requestId <- callbackArray$append(callback)
        server$clearChartCaches(requestId, chartIds)
       }
     }
     invisible()
   },
   updateMeasurements=function(oldObject, newObject, sendRequest=TRUE) {
     if (is.character(oldObject))
       oldObject <- .getMsObject(oldObject)
     if (!is(oldObject, "EpivizData"))
      stop("oldObject must be of class 'EpivizData'")
     oldObject$update(newObject, sendRequest=sendRequest)
     invisible()
   },
   .getMsObject=function(msObjId) {
      slot <- sapply(msList, function(typeList) msObjId %in% names(typeList))
      if (!any(slot)) {
        stop("could not find measurement object")
      }
      slot <- which(slot)
      typeList <- msList[[slot]]
      m <- match(msObjId, names(typeList))
      typeList[[m]]$obj
   },
   rmMeasurements=function(msObj) {
    if (is.character(msObj)) {
      # passed the id instead of the object
      msObj <- .self$.getMsObject(msObj)
    }

    if (!is(msObj, "EpivizData"))
      stop("'msObj' must be an 'EpivizData' object")

    msType <- .self$getMeasurementType(msObj)
    typeList <- msList[[msType]]

    slot <- match(msObj$getId(), names(typeList))
    if (is.na(slot))
      stop("object not found")
     
    objRecord <- typeList[[slot]]
    msName <- objRecord$name
    ms <- objRecord$measurements

    msList[[msType]][[msObj$getId()]] <<- NULL
    if(objRecord$connected) {
      callback=function(data) {
        message("measurement object ", msName, " removed and disconnected")  
      }
      requestId=callbackArray$append(callback)
      server$rmMeasurements(requestId, ms, msType)
    }
    invisible(NULL)
   },
   rmAllMeasurements=function(which=c("noDevice", "onlyDevice", "all")) {
    which <- match.arg(which)
    for (i in seq_along(msList)) {
      curType=names(msList)[i]
      if (length(msList[[curType]])>0) {
        for (objRecord in msList[[curType]]) {
          if((!objRecord$obj$inDevice && (which %in% c("noDevice", "all")) ||
             (objRecord$obj$inDevice && (which %in% c("onlyDevice", "all")))))
            rmMeasurements(objRecord$obj)
        }
      }
    }
   },
   listMeasurements=function(onlyLocal=TRUE) {
    if (!onlyLocal) {
      stop("'onlyLocal=FALSE' not implemented yet")
    }

    .doOneList <- function(ms) {
      ids <- names(ms)
      nms <- sapply(ms, "[[", "name")
      lens <- sapply(ms, function(x) length(x$obj$object))
      connected <- ifelse(sapply(ms, "[[", "connected"), "*", "")
      columns <- sapply(ms, function(x) paste0(x$obj$columns,collapse=","))

      data.frame(id=ids,
                 name=nms,
                 length=lens,
                 connected=connected,
                 columns=columns,
                 stringsAsFactors=FALSE,row.names=NULL)  
   }
   out <- list()
   for (i in seq_along(msList)) {
     curType=names(msList)[i]
     if (length(msList[[curType]])>0)
       out[[curType]] <- .doOneList(msList[[curType]])
   }
   return(out)
   },
   getMeasurements=function() {
     out <- list()
     for (i in seq_along(.typeMap)) {
      curType <- names(.typeMap)[i]
      nm <- paste0(curType,"Measurements")
      measurements <- list()
       
      if (length(msList[[curType]])>0) {
        measurements <- lapply(msList[[curType]], function(x) as.list(x$obj$getMeasurements()))
        names(measurements) <- NULL
        measurements <- unlist(measurements, recursive=FALSE)
      }
      out[[nm]] <- measurements 
     }
     return(out)
   },
  getMeasurementType=function(x) {
    if (!is.character(x)) {
      if (!is(x, "EpivizData")) {
        stop("'x' must be 'character' or an 'EpivizData' object")
      }
      x <- class(x)
    }
    m <- match(x, sapply(typeMap, "[[", "class"))
    if (is.na(m))
      stop("Class ", x, " not found in 'typeMap'")
    names(typeMap)[m]
  }
)

#####
# fetch data method
EpivizDeviceMgr$methods(
  .initPack=function(msType, length=0L) {
      if (!(msType %in% names(typeMap))) {
        stop("cannot find 'msType'")
      }
      get(typeMap[[msType]]$class)$new()$.initPack(length=length)
  },
  getData=function(measurements, chr, start, end) {
     out <- list(chr=chr,start=start,end=end) 
     query <- GRanges(chr, ranges=IRanges(start, end))

     for (typeIndex in seq_along(measurements)) {
       dataType <- names(measurements)[typeIndex]
       dataName <- gsub("Measurements","Data", dataType)
       msType <- gsub("Measurements","", dataType)
       
       out[[dataName]] <- list(start=start,end=end,chr=chr)
       curMeasurements <- measurements[[typeIndex]]
       if (length(curMeasurements)==0) {
        out[[dataName]] <- list()
        next
       }

       msMap <- .self$.findMeasurements(msType, curMeasurements)
       if (any(is.na(msMap))) {
        stop("could not find measurement")
       }
       objList <- msList[[msType]]

       dataPack <- .initPack(msType, length(msMap))

       for (msIndex in seq_along(curMeasurements)) {
        msObj <- objList[[msMap[msIndex]]]$obj
        curData <- msObj$getData(query, curMeasurements[msIndex])
        dataPack$set(curData, curMeasurements[msIndex], msIndex)
       }
       out[[dataName]] <- c(out[[dataName]], dataPack$getData())
     }
     return(out)
   }
)

# chart management methods
EpivizDeviceMgr$methods(
   addChart=function(chartObject, sendRequest=TRUE, ...) {
    chartIdCounter <<- chartIdCounter + 1L
    chartId <- sprintf("epivizChart_%d", chartIdCounter)
    chartObject$setId(chartId)
    chartList[[chartId]] <<- chartObject

    if (sendRequest) {
      callback=function(data) {
        appChartId = data$id
        chartIdMap[[chartId]] <<- appChartId
        activeId <<- chartId
        message("Chart ", chartId, " added to browser and connected")  
      }
      requestId=callbackArray$append(callback)
      server$addChart(requestId, chartObject$type, chartObject$measurements)
    }
    invisible(NULL)
   }, 
   .getChartObject=function(chartId) {
    obj <- chartList[[chartId]]
    if (is.null(obj))
      stop("cannot find object")
    obj
   },
   rmChart=function(chartObj) {
    if (is.character(chartObj)) {
      # passed the id instead of the object
      chartObj <- .self$.getChartObject(chartObj)
    }

    if (!is(chartObj, "EpivizChart"))
      stop("'chartObj' must be an 'EpivizChart' object")

    slot <- match(chartObj$getId(), names(chartList))
    if (is.na(slot))
      stop("object not found")

    chartId <- chartObj$getId()
    chartList[[chartId]] <<- NULL

    if(!is.null(chartIdMap[[chartId]])) {
      callback=function(data) {
        message("chart ", chartId, " removed and disconnected")  
      }
      requestId=callbackArray$append(callback)
      server$rmChart(requestId, chartIdMap[[chartId]])
    }
    invisible(NULL)
   },
   rmAllCharts=function(which=c("noDevice", "onlyDevice", "all")) {
    which = match.arg(which)
    for (obj in chartList) {
      if ((!obj$inDevice && (which %in% c("noDevice", "all"))) ||
          (obj$inDevice && (which %in% c("onlyDevice", "all"))))
        rmChart(obj)
    }
    invisible()
   }, 
   listCharts=function() {
    ids <- names(chartList)
    type <- sapply(chartList, function(x) x$type)
    ms <- sapply(chartList, function(x) paste0(names(x$measurements), collapse=","))
    connected <- ifelse(sapply(names(chartList), function(x) x %in% names(chartIdMap)), "*", "")
    out <- data.frame(id=ids, 
                      type=type, 
                      measurements=ms, 
                      connected=connected,
                      stringsAsFactors=FALSE)
    rownames(out) <- NULL
    out
   },
   setActive=function (devId) {
     'set given device as active in browser'
     slot=which(sapply(lapply(devices,names), function(x) devId %in% x))
     if (length(slot)<1)
       stop("device Id not found")
     activeId <<- devId
     invisible(NULL)
   }
)

# device management methods
EpivizDeviceMgr$methods(
   addDevice=function(obj, devName, sendRequest=TRUE, ...) {
     'add device to epiviz browser'
      deviceIdCounter <<- deviceIdCounter + 1L
      deviceId <- sprintf("epivizDevice_%d", deviceIdCounter)
 
      msObject <- .self$addMeasurements(obj, devName, sendRequest=sendRequest, ...)
      msObject$setInDevice(TRUE)

      chartObject <- msObject$plot(sendRequest=sendRequest, inDevice=TRUE, ...)
      chartObject$setInDevice(TRUE)

      deviceObj <- EpivizDevice$new(msObject=msObject, chartObject=chartObject)
      deviceObj$setId(deviceId)
      deviceList[[deviceId]] <<- deviceObj
      deviceObj
   },
   # TODO: turn this into a rmMeasurement method
   rmDevice=function(deviceObj) {
     'delete device from epiviz browser'
     if (is.character(deviceObj)) {
      deviceObj <- deviceList[[deviceObj]]
      if (is.null(deviceObj)) 
        stop("did not find object")
     }

     if (!is(deviceObj, "EpivizDevice"))
      stop("'deviceObj' must be an 'EpivizDevice' object")

     devId <- deviceObj$getId()
     if (is.null(deviceList[[devId]]))
      stop("did not find obejct")

     rmChart(deviceObj$getChartObject())
     rmMeasurements(deviceObj$getMsObject())
     deviceList[[devId]] <<- NULL
     invisible()
   },
   rmAllDevices=function() {
    for (obj in deviceList) {
      rmDevice(obj)
    }
   },
  updateDevice=function(oldObject, newObject, sendRequest=TRUE) {
     if (is.character(oldObject))
       oldObject <- deviceList[[oldObject]]

     if (!is(oldObject, "EpivizDevice"))
      stop("oldObject must be of class 'EpivizDevice'")

     oldObject$update(newObject, sendRequest=sendRequest)
     invisible()
   },
   listDevices=function() {
     'list devices in browser'
    ids <- names(deviceList)
    type <- sapply(deviceList, function(x) x$getChartObject()$type)
    ms <- sapply(deviceList, function(x) paste0(names(x$getChartObject()$measurements), collapse=","))
    connected <- ifelse(sapply(deviceList, function(x) x$getChartId() %in% names(chartIdMap)), "*", "")
    out <- data.frame(id=ids, 
                      type=type, 
                      measurements=ms, 
                      connected=connected,
                      stringsAsFactors=FALSE)
    rownames(out) <- NULL
    out

   }
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
startEpiviz <- function(port=7312L, localURL=NULL, 
                        chr="chr11", start=99800000, end=103383180, 
                        debug=FALSE, proxy=TRUE, workspace=NULL, 
                        openBrowser=TRUE,
                        verbose=FALSE) {
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
    mgr <- EpivizDeviceMgr$new(server=server, url=url, verbose=verbose)
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

