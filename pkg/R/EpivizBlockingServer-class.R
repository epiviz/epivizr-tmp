EpivizBlockingServer <- setRefClass("EpivizBlockingServer", 
  contains="EpivizServer",
  fields=list(
    isConnected="logical",
    waitingForResponse="logical"
  ),
  methods=list(
    initialize=function(...) {
      isConnected <<- FALSE
      waitingForResponse <<- FALSE
      callSuper(...)
    },
    connectionEstablishedCallback=function(WS) {
      isConnected <<- TRUE
      callSuper(WS)
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
        service(websocket)
      }
    },
    unblock=function() {
      if (waitingForResponse) {
        waitingForResponse <<- FALSE
      }
    }
  )
)