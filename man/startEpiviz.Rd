\name{startEpiviz}
\alias{startEpiviz}

\title{Start the epiviz interface}

\description{
  Create an epiviz session manager which can be used to add
  and delete tracks and plots in the browser web app.
}

\usage{
  startEpiviz(port = 7312L, localURL = NULL, useDevel = FALSE,
    chr = "chr11", start = 99800000, end = 103383180, debug = FALSE,
    proxy = TRUE, workspace = NULL, scripts=NULL, openBrowser = TRUE,
    daemonized = .epivizrCanDaemonize(), verbose = FALSE, 
    nonInteractive = FALSE, tryPorts = FALSE)
}

\arguments{
  \item{port}{(integer) port for websocket server}

  \item{localURL}{(character) use this url for the epiviz
  server instead of the standard URL (http://epiviz.cbcb.umd.edu).
  For example \code{localURL="http://localhost/epiviz"} when serving
  from the local host}
  
  \item{useDevel}{(logical) use the devel branch of the epiviz server (http://epiviz-dev.cbcb.umd.edu)}

  \item{chr}{(character) chromosome to browse to on
  startup}

  \item{start}{(integer) start position to browse to on
  startup}

  \item{end}{(integer) end position to browse to on
  startup}

  \item{debug}{(logical) start the epiviz browser in debug
  mode}

  \item{proxy}{(logical) start the epiviz browser using the data
  proxy. This is useful when hosting the web app locally, but
  want to use data hosted in the epiviz server}

  \item{workspace}{(character) a workspace id to load in the 
  epiviz web app on startup}

  \item{scripts}{(character vector) URLs for JavaScript plugin scripts to be imported when epiviz is loaded}

  \item{openBrowser}{(logical) browse to the epiviz URL before exiting function}
  
  \item{daemonized}{(logical) use non-blocking version if available}

  \item{verbose}{(logical) display log messages on websocket requests}

  \item{nonInteractive}{(logical) run in non-interactive mode, for development purposes only}

  \item{tryPorts}{(logical) try more ports if port number given by argument \code{port} is in use.}
}

\value{
  an object of class \linkS4class{EpivizDeviceMgr}.
}

\author{Hector Corrada Bravo}
\seealso{
  \link{EpivizDeviceMgr-class}
}

\examples{
\dontrun{
mgr <- startEpiviz(openBrowser=FALSE)
mgr$startServer()
mgr$stopServer()
}
}

