EpivizNonBlockingServer <- setRefClass("EpivizNonBlockingServer", 
  contains="EpivizServer",
  methods=list(
    initialized=function(port=7312L, mgr=NULL, ...) {
      callSuper(port, mgr, ...)
      daemonize(websocket)
    }
  )
)