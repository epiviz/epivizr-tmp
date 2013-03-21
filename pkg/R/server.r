.generate_handler=function(mgr) {
  function(DATA, WS, HEADER) {
    message("mgr: data received")
    print(rawToChar(DATA))
    
    msg = RJSONIO::fromJSON(rawToChar(DATA))
    if (msg$type == "request") {
      data = switch(msg$action,
                    getMeasurements=mgr$getMeasurements())
      
      response=RJSONIO::toJSON(list(type="response", id=msg$id, data=data))
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
  dummy_websocket_write(request, server$client_sockets[[1]])
}

.makeRequest_refresh <- function(server) {
  request=list(action="refresh")
  dummy_websocket_write(request, server$client_sockets[[1]])
}

.makeRequest_navigate <- function(server, chr, start, end) {
  request=list(action="navigate",
               data=list(chr=chr,start=start,end=end))
}