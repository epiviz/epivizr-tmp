#' epiviz interactive track manager
#' 
#' This class is used to add and delete interactive devices in the epiviz browser. It inherits
#' from the \link{environment} class, so \link{get} and \link{set} methods exist (but not recommended 
#' for use). Setters and getters are defined for the pertinent slots. 
#' 
#' @section Fields:
#' \describe{
#'  \item{\code{devices}:}{A list of \link{epivizDevice} objects defining currently loaded}
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
                        fields=list(devices="list",
                                    activeID="character",
                                    server="environment"),
                        methods=list(isClosed=function() {},
                                     stop=function() {},
                                     addDevice=function() {},
                                     delDevice=function() {},
                                     setActive=function() {},
                                     listDevices=function() {},
                                     refresh=function() {},
                                     navigate=function() {}))

#' epiviz interactive device
#' 
#' This class wraps a GenomicRanges object to be plotted in an interactive epiviz device
#' 
#' @section Fields:
#' \describe{
#'  \item{\code{gr}:}{The the \link{GRanges-class} object bound to this track}
#'  \item{\code{type}:}{Display type, currently "regions", "line" or "gene"}
#'  \item{\code{mdCols}:}{vector of elementMetaData column names to use as measurements in plots}
#'  \item{\code{minValue}:}{minimum value for y axis}
#'  \item{\code{maxValue}:}{maximum value for y axis}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{getData}:}{Subset data from the GRanges object within given region}
#' }
#' 
#' @name EpivizDevice-class
#' @rdname EpivizDevice-class
#' 
#' @export
EpivizDevice <- setRefClass("EpivizDevice",
         fields=list(gr="GenomicRanges",
            type="character",
            mdCols="character",
            minValue="numeric",
            maxValue="numeric"),
        methods=list(getData=function() {}))
