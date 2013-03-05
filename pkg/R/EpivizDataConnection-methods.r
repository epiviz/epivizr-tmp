#' Initialize a data connection with websockets backend
#' 
#' @param port port to use by websocket
#' 
#' @return Object of class \link{CegsWSDataConenction-class}
setMethod("initialize", "EpivizDataConnection",
          function(.Object, port=7681L) {
            obj=callNextMethod(.Object)
            obj@backend=websockets::create_server(port=port)
            obj
          })

#' close a data connection to cegs browser
#' 
#' @param obj object of class \link{EpivizDataConnection}
setMethod("epivizClose", "EpivizDataConnection",
          function(obj) {
            websockets::websocket_close(obj@backend)
          })

#' check if data connection is closed
#' 
#' @param obj Object of class \link{EpivizDataConnection}
#' 
#' @return True if data connection is closed
setMethod("isClosed", "EpivizDataConnection",
          function(obj) {
            is.null(obj@backend$server_socket)
          })


