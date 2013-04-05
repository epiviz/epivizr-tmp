EpivizBlockingServer <- setRefClass("EpivizBlockingServer", 
  contains="EpivizServer",
  fields=list(
    isConnected="logical",
    waitingForResponse="logical"
  ),
  methods=list(
    initialize=function(port=7312L, mgr=NULL) {
      callSuper(port, mgr, ...)
      isConnected <<- FALSE
      waitingForResponse <<- FALSE
    }
    connectionEstablishedCallback=function(WS) {
      callSuper(WS)
      isConnected <<- TRUE
    },
    connect=function() {
        while (!isConnected) {
          service(websocket) 
        }
      },
    listen=function() {
      #TODO: add tryCatch statement?
      message("Blocking server, interrupt to continue...")
      while(TRUE) service(websocket)   
    },
    block=function() {
      waitingForResponse <<- TRUE
      while (waitingForResponse) {
        service(server)
      }
    },
    unblock=function() {
      if (waitingForResponse) {
        waitingForResponse <<- FALSE
      }
    }
  )
)