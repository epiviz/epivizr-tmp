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
    }
  }
}

.con_established=function(WS) {
  message("mgr: connection established")
  websockets::websocket_write("Hello There!", WS)
}

.con_closed=function(WS) {
  message("mgr: one connection closed")
}

.makeRequest_addDevice <- function(devId, server, device, devName) {
  type=switch(class(device),
              EpivizBlockDevice="block",
              EpivizBpDevice="bp",
              EpivizGeneDevice="gene")
  request=list(action="addDevice",
               data=list(name=devName,
                         type=type,
                         id=devId))
  #dummy_websocket_write(request, server$client_sockets[[1]])
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