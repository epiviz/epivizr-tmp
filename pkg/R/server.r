.callbackArray=IndexedArray$new()


.requestQueue=Queue$new()
.sendRequestsInQueue=function(WS) {
  while (!is.null(request <- .requestQueue$pop()))
    websockets::websocket_write(request, WS)
}

.generate_handler=function(mgr) {
  function(DATA, WS, HEADER) {
    message("mgr: data received")
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
    }
  }
}

.con_established=function(WS) {
  message("mgr: connection established")
  websockets::websocket_write("Hello There!", WS)
  .sendRequestsInQueue(WS)
}

.con_closed=function(WS) {
  message("mgr: one connection closed")
}

.makeRequest_addDevice <- function(server, requestId, devType, measurements) {
  request=list(type="request",
               id=requestId,
               action="addDevice",
               data=list(measurements=measurements,
                         type=devType))
  
  request=rjson::toJSON(request)
  
  if (length(server$client_sockets)<1) {
    .requestQueue$push(request)
  } else {
    websocket_write(request, server$client_sockets[[1]])
  }
  invisible(NULL)
}

.makeRequest_refresh <- function(server) {
  request=list(action="refresh")
  #dummy_websocket_write(request, server$client_sockets[[1]])
  invisible(NULL)
}

.makeRequest_navigate <- function(server, chr, start, end) {
  request=list(action="navigate",
               data=list(chr=chr,start=start,end=end))
}