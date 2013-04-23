\name{EpivizDevice-class}
\alias{EpivizDevice}
\alias{EpivizDevice-class}
\title{epiviz interactive device}
\usage{
  EpivizDevice(...)
}
\arguments{
  \item{...}{arguments passed to constructor}
}
\description{
  This class wraps a GenomicRanges object to be plotted in
  an interactive epiviz device
}
\section{Fields}{
  \describe{ \item{\code{gr}:}{The the \link{GRanges-class}
  object bound to this track} \item{\code{tree}:}{A
  \link{GIntervalTree-class} object used for querying}
  \item{\code{id}:}{The id for this device assigned by the
  epiviz manager object} }
}

\section{Methods}{
  \describe{ \item{\code{getData}:}{Subset data from the
  GRanges object within given region} }
}

