#' epiviz interactive track manager
#' 
#' This class is used to add and delete interactive devices in the epiviz browser. It inherits
#' from the \link{environment} class, so \link{get} and \link{set} methods exist (but not recommended 
#' for use). Setters and getters are defined for the pertinent slots. 
#' 
#' @section Fields:
#' \describe{
#'  \item{\code{devices}:}{A list of \link{epivizDevice} objects defining currently loaded}
#'  \item{\code{idCounter}:}{id generator}
#'  \item{\code{activeID}:}{ID of currently active device}
#'  \item{\code{server}:}{An environment implementing a websockets server}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{isClosed}:}{True if websocket is closed for this manager}
#'  \item{\code{stop}:}{Stop communiation and close websocket server}
#'  \item{\code{addDevice}:}{Add a \link{epivizDevice} object to the list of devices}
#'  \item{\code{delDevice}:}{Remove device from list}
#'  \item{\code{setActive}:}{Set device as active (for navigation on browser)}
#'  \item{\code{listDevices}:}{List current devices}
#'  \item{\code{refresh}:}{Refresh epiviz browser}
#'  \item{\code{navigate}:}{Navigate to given genomic region in browser}
#' }
#' 
#' @name EpivizDeviceMgr-class
#' @rdname EpivizDeviceMgr-class
#' 
#' @export
EpivizDeviceMgr <- setRefClass("EpivizDeviceMgr", 
  fields=list(
    devices="list",
    idCounter="integer",
    activeID="character",
    server="environment"),
  methods=list(
   isClosed=function() {
     'check if connection is closed'
    !exists("socket_server", server) || is.null(server$socket_server)
   },
   stop=function() {
     'stop epiviz connection'
     close_dummy_server(server)
     invisible(server)
   },
   addDevice=function(device, devName) {
     'add device to epiviz browser'
     if (!is(device, "EpivizDevice"))
       stop("device must be of class EpivizDevice")
     if (.self$isClosed())
       stop("manager connection is closed")
     if (devName %in% names(.self$devices))
       stop("device name already in use")
     
     idCounter <<- idCounter + 1L
     devId <- sprintf("epivizDev_%d", .self$idCounter)
     .makeRequest_addDevice(devId, .self$server, device, devName)
     devRecord=list(name=devName, obj=device)
     devices[[devId]] <<- devRecord
     return(devId)
   },
   delDevice=function() {
                                 
   },
   setActive=function () {
     
   },
   listDevices=function() {
     
   },
   refresh=function() {
     
   },
   navigate=function() {
     
   }
  )
)

#' Start the epiviz interface
#' 
#' Create an epiviz device manager which can be used to add and delete tracks in browser
#' 
#' @param port the port for the websocket server
#' 
#' @return an object of class \link{EpivizDeviceMgr-class}.
#' 
#' @examples
#' mgr <- startEpiviz()
#' mgr$stop()
#' 
#' @export
startEpiviz <- function(port=7681L) {
  mgr <- EpivizDeviceMgr$new(
    server=create_dummy_server(port=port),
    idCounter=0L
  )
  mgr
}

