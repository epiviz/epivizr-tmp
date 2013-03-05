
#' Connection with websockets backend
#' 
#' @export
setClass("EpivizDataConnection", 
          representation=representation(backend="environment"))
         
#' epiviz interactive track manager
#' 
#' This class is used to add and delete interactive devices in the epiviz browser. It inherits
#' from the \link{environment} class, so \link{get} and \link{set} methods exist (but not recommended 
#' for use). Setters and getters are defined for the pertinent slots. 
#' 
#' @section Slots:
#' \describe{
#'  \item{\code{devices}:}{A list of \link{epivizDevice} objects defining currently loaded}
#'  \item{\code{activeID}:}{ID of currently active device}
#'  \item{\code{conn}:}{An object of class \link{EpivizDataConnection}}
#' }
#' 
#' @name EpivizDeviceMgr-class
#' @rdname EpivizDeviceMgr-class
#' 
#' @export
setClass("EpivizDeviceMgr", contains="environment",
         validity=function(object) {
           if (!exists("devices", object)) return(FALSE)
           if (length(object$devices) != object$nDevices) return(FALSE)
           if (!exists("conn", object)) return(FALSE)
           if (!is_a(object$conn, "EpivizDataConnection")) return(FALSE)
           if (!exists("activeID", object)) return(FALSE)
           if (object$activeID > length(object$nDevices)) return(FALSE)
           return(TRUE)
         })

#' epiviz interactive device
#' 
#' This class wraps a GenomicRanges object to be plotted in an interactive epiviz device
#' 
#' @section Slots:
#' \describe{
#'  \item{\code{id}:}{Id given by the device manager}
#'  \item{\code{objname}:}{The name of the \link{GRanges-class} object bound to this track}
#'  \item{\code{type}:}{Tracks display type, currently "regions", "line" or "gene"}
#'  \item{\code{matcols}:}{vector of column names to use as measurements in non-region devices}
#'  \item{\code{minValue}:}{minimum value for y axis}
#'  \item{\code{maxValue}:}{maximum value for y axis}
#' }
#' 
#' @name EpivizDevice-class
#' @rdname EpivizDevice-class
#' 
#' @export
setClass("EpivizDevice",
         representation=representation(
           objname="character",
           type="character",
           matcols="character",
           minValue="numeric",
           maxValue="numeric"))
