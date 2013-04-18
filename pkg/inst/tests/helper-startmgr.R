localURL="http://localhost/~hcorrada/epiviz/test_socket.php"
.startMGR=function(openBrowser=FALSE,...) {
  if (openBrowser) {
    tryCatch(startEpiviz(localURL=localURL, debug=TRUE, proxy=TRUE, openBrowser=TRUE, ...), interrupt=function(e) invisible())
  } else {
    startEpiviz(localURL=localURL,debug=TRUE,openBrowser=FALSE, ...)
  }
}
