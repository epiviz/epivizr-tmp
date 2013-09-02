#' Start the epiviz interface
#' 
#' Create an epiviz device manager which can be used to add and delete tracks in browser
#' 
#' @param port (integer) the port for the websocket server
#' @param localURL (character) use this url for the epiviz server instead of the standard remote URL
#' @param chr (character) chromosome to browse to on startup
#' @param start (integer) start position to browse to on startup
#' @param end (integer) end position to browse to on startup
#' @param debug (logical) start the epiviz browser in debug mode
#' @param proxy (logical) start the epiviz browser in proxy mode
#' @param openBrowser (logical) browse to the epiviz URL
#' 
#' @return an object of class \linkS4class{EpivizDeviceMgr}.
#' 
#' @examples
#' 
#' mgr <- startEpiviz(openBrowser=FALSE)
#' mgr$startServer()
#' mgr$stopServer()
#' 
#' @export
startEpiviz <- function(port=7312L, localURL=NULL, 
                        chr="chr11", start=99800000, end=103383180, 
                        debug=FALSE, proxy=TRUE, workspace=NULL, 
                        openBrowser=TRUE,
                        verbose=FALSE) {
  message("Opening websocket...")
  server <- epivizr:::createServer(port=port)
  
  if (missing(localURL) || is.null(localURL)) {
    url="http://epiviz.cbcb.umd.edu/index.php"
  } else {
    url=localURL
  }
  
  controllerHost=sprintf("ws://localhost:%d", port)
  url=sprintf("%s?controllerHost=%s&debug=%s&proxy=%s&", 
              url,
              controllerHost,
              ifelse(debug,"true","false"),
              ifelse(proxy,"true","false"))
  
  if (!is.null(workspace)) {
    url=paste0(url,"workspace=",workspace,"&")
  } else {
    url=paste0(url,
               sprintf("chr=%s&start=%d&end=%d&",
                       chr,
                       as.integer(start),
                       as.integer(end)))
  }
  tryCatch({
    mgr <- EpivizDeviceMgr$new(server=server, url=url, verbose=verbose)
    mgr$bindToServer()
  }, error=function(e) {
    server$stopServer()
    stop("Error starting Epiviz: ", e)
  })
  
  if (openBrowser) {
    tryCatch({
      message("Opening browser...")
      mgr$openBrowser(url)
    }, error=function(e) {
      mgr$stopServer()
      stop("Error starting Epiviz: ", e)
    }, interrupt=function(e) {NULL})
  }
  return(mgr)
}
