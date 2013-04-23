\name{startEpiviz}
\alias{startEpiviz}
\title{Start the epiviz interface}
\usage{
  startEpiviz(port = 7312L, localURL = NULL, chr = "chr11",
    start = 99800000, end = 103383180, debug = FALSE,
    proxy = TRUE, openBrowser = TRUE)
}
\arguments{
  \item{port}{(integer) the port for the websocket server}

  \item{localURL}{(character) use this url for the epiviz
  server instead of the standard remote URL}

  \item{chr}{(character) chromosome to browse to on
  startup}

  \item{start}{(integer) start position to browse to on
  startup}

  \item{end}{(integer) end position to browse to on
  startup}

  \item{debug}{(logical) start the epiviz browser in debug
  mode}

  \item{proxy}{(logical) start the epiviz browser in proxy
  mode}

  \item{openBrowser}{(logical) browse to the epiviz URL}
}
\value{
  an object of class \linkS4class{EpivizDeviceMgr}.
}
\description{
  Create an epiviz device manager which can be used to add
  and delete tracks in browser
}
\examples{
\dontrun{
mgr <- startEpiviz()
mgr$stopServer()
}
}

