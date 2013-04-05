EpivizServer <- setRefClass("EpivizServer",
  contains="VIRTUAL",
  fields=list(
    websocket="environment",
    requestQueue="Queue"
  ),
  methods=list(
    initizalize=function(port=7312L, mgr=NULL, ...) {
      if (!is(mgr, "EpivizDeviceMgr")) {
        stop("Error in initialize EpivizServer, mgr should be of class EpivizDeviceMgr")
      }
      
      websocket <<- websockets::create_server(port=port)
      websockets::setCallback("receive", wrapDataCallback(mgr), websocket)
      websockets::setCallback("established", connectionEstablishedCallback, websocket)
      websockets::setCallback("closed", connectionClosedCallback, websocket)
      
      requestQueue <<- Queue$new()
    },
    wrapDataCallback=function(mgr) {
        function(DATA, WS, HEADER) {
          message("server: data received")
          print(rawToChar(DATA))
        
          msg = rjson::fromJSON(rawToChar(DATA))
          if (msg$type == "request") {
            out=list(type="response",id=msg$id)
          
            if (msg$action=="getMeasurements") {
              out$data=mgr$getMeasurements()
            } else if(msg$action=="getAllData") {
              out$data=mgr$getData(msg$measurements,msg$chr,msg$start,msg$end)
            }
            response=rjson::toJSON(out)
            websockets::websocket_write(response, WS)
          } else if (msg$type == "response") {
            callback = mgr$callbackArray$get(msg$id)
            if (!is.null(callback)) {
              callback(msg$data)
            }
            .self$unblock()
          }
        }
    },
    connectionEstablishedCallback=function(WS) {
      message("mgr: connection established")
      sendRequestsInQueue(WS)
    },
    connectionClosedCallback=function(WS) {
      message("mgr: one connection closed")
    },
    isClosed=function() {
      !exists("server_socket", websocket) || is.null(websocket$server_socket)
    },
    stop=function() {
      emptyRequestQueue()
      websockets::websocket_close(websocket)
      invisible(NULL)
    },
    connect=function() {
      invisible(NULL)
    },
    listen=function() {
      invisible(NULL)
    },
    block=function() {
      invisible(NULL)
    },
    unblock=function() {
      invisible(NULL)
    },
    sendRequest <- function(request) {
      request=rjson::toJSON(request)
      
      if (length(websocket$client_sockets)<1) {
        requestQueue$push(request)
      } else {
        websocket_write(request, websocket$client_sockets[[1]])
        .self$block()
      }
      invisible(NULL)
    },
    addDevice=function(requestId, type, measurements) {
      request=list(type="request",
                   id=requestId,
                   action="addDevice",
                   data=list(measurements=measurements,
                             type=devType))
      sendRequest(request)
    },
    rmDevice=function(requestId, chartId, measurements, devType) {
      request=list(type="request",
                   id=requestId,
                   action="rmDevice",
                   data=list(id=chartId,
                             measurements=measurements,
                             type=devType))
      sendRequest(request)
    },
    refresh=function() {
      request=list(action="refresh")
      invisible(NULL)
    },
    navigate=function(chr,start,end) {
      request=list(action="navigate",
                   data=list(chr=chr,start=start,end=end))
      invisible(NULL)
    },
    sendRequestsInQueue=function(WS) {
      while (!is.null(request <- requestQueue$pop())) {
        websockets::websocket_write(request, WS)
      }
    },
    emptyRequestQueue=function() {
      while (!is.null(requestQueue$pop())) {}
    }
  )                           
)

createServer <- function(mgr, port=7312L) {
  if (.Platform$OS == "unix") {
    return(EpivizNonBlockingServer$new(port, mgr))
  } else {
    return(EpivizBlockingServer$new(port, mgr))
  }
}
