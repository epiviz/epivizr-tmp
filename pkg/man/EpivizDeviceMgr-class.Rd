\name{EpivizDeviceMgr-class}
\alias{EpivizDeviceMgr-class}
\title{epiviz interactive track manager}
\description{
  This class is used to add and delete interactive devices
  in the epiviz browser. It inherits from the
  \link{environment} class, so \link{get} and \link{set}
  methods exist (but not recommended for use). Setters and
  getters are defined for the pertinent slots.
}
\section{Fields}{
  \describe{ \item{\code{devices}:}{A list of
  \link{epivizDevice} objects defining currently loaded}
  \item{\code{idCounter}:}{id generator}
  \item{\code{activeId}:}{ID of currently active device}
  \item{\code{server}:}{An environment implementing a
  websockets server} }
}

\section{Methods}{
  \describe{ \item{\code{isClosed}:}{True if websocket is
  closed for this manager} \item{\code{stop}:}{Stop
  communiation and close websocket server}
  \item{\code{addDevice}:}{Add a \link{epivizDevice} object
  to the list of devices} \item{\code{delDevice}:}{Remove
  device from list} \item{\code{setActive}:}{Set device as
  active (for navigation on browser)}
  \item{\code{listDevices}:}{List current devices}
  \item{\code{getData}:}{Get data from devices}
  \item{\code{refresh}:}{Refresh epiviz browser}
  \item{\code{navigate}:}{Navigate to given genomic region
  in browser} }
}

