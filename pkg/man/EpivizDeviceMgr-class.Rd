\name{EpivizDeviceMgr-class}
\alias{EpivizDeviceMgr}
\alias{EpivizDeviceMgr-class}
\title{epiviz interactive track manager}
\usage{
  EpivizDeviceMgr(...)
}
\arguments{
  \item{...}{arguments passed to constructor}
}
\description{
  This class is used to add and delete interactive devices
  in the epiviz browser. Setters and getters are defined
  for the pertinent slots.
}
\section{Fields}{
  \describe{ \item{\code{devices}:}{A list of
  \linkS4class{EpivizDevice} objects defining currently
  loaded} \item{\code{idCounter}:}{id generator}
  \item{\code{activeId}:}{ID of currently active device}
  \item{\code{server}:}{An environment implementing a
  websockets server} }
}

\section{Methods}{
  \describe{ \item{\code{isClosed}:}{True if websocket is
  closed for this manager} \item{\code{stop}:}{Stop
  communiation and close websocket server}
  \item{\code{addDevice}:}{Add a \linkS4class{EpivizDevice}
  object to the list of devices}
  \item{\code{delDevice}:}{Remove device from list}
  \item{\code{setActive}:}{Set device as active (for
  navigation on browser)} \item{\code{listDevices}:}{List
  current devices} \item{\code{getData}:}{Get data from
  devices} \item{\code{refresh}:}{Refresh epiviz browser}
  \item{\code{navigate}:}{Navigate to given genomic region
  in browser} }
}

