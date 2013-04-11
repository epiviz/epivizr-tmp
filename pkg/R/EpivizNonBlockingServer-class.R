EpivizNonBlockingServer <- setRefClass("EpivizNonBlockingServer", 
  contains="EpivizServer",
  methods=list(
    openSocket=function() {
      callSuper()
      daemonize(websocket)
    }
  )
)