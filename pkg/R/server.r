create_dummy_server=function(port=7681L) {
  env=new.env()
  env$socket_server=port
  return(env)
}

close_dummy_server=function(server) {  
  server$socket_server=NULL
  invisible(server)
}

dummy_websocket_write=function(DATA, WS) {
  
}

.makeRequest_addDevice <- function(devId, server, device, devName) {
  request=list(action="addDevice",
               data=list(name=devName,
                         type=device$type,
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