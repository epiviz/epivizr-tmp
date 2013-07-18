EpivizServer <- setRefClass("EpivizServer",
  fields=list(
    port="integer",
    websocket="ANY",
    server="ANY",
    interrupted="logical",
    socketConnected="logical",
    msgCallback="function",
    requestQueue="Queue"
  ),
  methods=list(
    initialize=function(...) {
      port <<- 7546L
      interrupted <<- FALSE
      socketConnected <<- FALSE
      server <<- NULL
      callSuper(...)
    },
    startServer=function(...) {
      callbacks <- list(
        call=.self$.dummyTestPage,
        onWSOpen=function(ws) {
          websocket <<- ws
          socketConnected <<- TRUE
          websocket$onMessage(.self$msgCallback)
          websocket$onClose(function() {
            socketConnected <<- FALSE
            invisible()
          })
          sendRequestsInQueue()
          invisible()
        }
      )
      
      tryCatch({
        server <<- httpuv::startServer("0.0.0.0", port, callbacks)  
      }, error=function(e) {
        cat("Error starting epivizServer:\n")
        print(e)
        stop(e)
      })
      invisible()
    },
    runServer=function(...) {
      startServer(...)
      on.exit(stopServer())
      service()
    },
    stopServer=function() {
      interrupted <<- TRUE
      
      if (!isClosed()) {
        httpuv::stopServer(server)
      }
      server <<- NULL
      socketConnected <<- FALSE
      interrupted <<- TRUE
      invisible()
    },
    isClosed=function() {
      is.null(server)
    },
    service=function() {
      if (isClosed()) {
        stop("Can't listen, socket is closed")
      }
      
      interrupted <<- FALSE
      while(!interrupted) {
        httpuv::service(250)
        Sys.sleep(0.001)
      }
      invisible()
    },
    stopService=function() {
      interrupted <<- TRUE
      invisible()
    },
    bindManager=function(mgr) {
      msgCallback <<- function(binary, msg) {
        cat("server: data received")
        #print(rawToChar(DATA))
        if (binary) {
          msg <- rawToChar(msg)
        }
        print(msg)
        msg = rjson::fromJSON(msg)
        if (msg$type == "request") {
          out=list(type="response",id=msg$id)
          
          if (msg$action=="getMeasurements") {
            out$data=mgr$getMeasurements()
          } else if(msg$action=="getAllData") {
            out$data=mgr$getData(msg$measurements,msg$chr,msg$start,msg$end)
          }
          response=rjson::toJSON(out)
          websocket$send(response)
        } else if (msg$type == "response") {
          callback = mgr$callbackArray$get(msg$id)
          if (!is.null(callback)) {
            callback(msg$data)
          }
          stopService()
        }
      }
      invisible()
    },
    sendRequest=function(request) {
      request=rjson::toJSON(request)
      
      if (!socketConnected) {
        requestQueue$push(request)
      } else {
        websocket$send(request)
        service()
      }
      invisible()
    },
    addDevice=function(requestId, devType, measurements) {
      request=list(type="request",
                   id=requestId,
                   action="addDevice",
                   data=list(measurements=measurements,
                             type=devType))
      sendRequest(request)
    },
    rmDevice=function(requestId, chartId, measurements, devType) {
      request=list(type="request",
                   id=requestId,
                   action="rmDevice",
                   data=list(id=chartId,
                             measurements=measurements,
                             type=devType))
      sendRequest(request)
    },
    clearCache=function(requestId, chartId) {
      request=list(type="request",
                   id=requestId,
                   action="clearDeviceCache",
                   data=list(id=chartId))
      sendRequest(request)
    },
    refresh=function() {
      request=list(action="refresh")
      # TODO: finish implementation
      # sendRequest(request)
      invisible()
    },
    navigate=function(requestId,chr,start,end) {
      request=list(type="request",
                   action="navigate",
                   id=requestId,
                   data=list(chr=chr,start=start,end=end))
      sendRequest(request)
    },
    sendRequestsInQueue=function(WS) {
      while (!is.null(request <- requestQueue$pop())) {
        websocket$send(request)
      }
    },
    emptyRequestQueue=function() {
      requestQueue$empty()
      inivisible()
    },
    .dummyTestPage=function(req) {
      wsUrl = paste(sep='',
                    '"',
                    "ws://",
                    ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
                    '"')
      
      list(
        status = 200L,
        headers = list(
          'Content-Type' = 'text/html'
        ),
        body = paste(
          sep = "\r\n",
          "<!DOCTYPE html>",
          "<html>",
          "<head>",
          '<style type="text/css">',
          'body { font-family: Helvetica; }',
          'pre { margin: 0 }',
          '</style>',
          "<script>",
          sprintf("var ws = new WebSocket(%s);", wsUrl),
          "ws.onmessage = function(msg) {",
          '  var data = JSON.parse(msg.data)',
          '  msgDiv = document.createElement("pre");',
          '  msgDiv.innerHTML = data.data.replace(/&/g, "&amp;").replace(/\\</g, "&lt;");',
          '  document.getElementById("output").appendChild(msgDiv);',
          "}",
          "function sendInput() {",
          "  var input = document.getElementById('input');",
          "  ws.send(JSON.stringify({type: 'request', id: 0, action: 'getAllData', measurements: {}, chr: input.value, start: 0, end: 0}));",
          "  input.value = '';",
          "}",
          "</script>",
          "</head>",
          "<body>",
          '<h3>Send Message</h3>',
          '<form action="" onsubmit="sendInput(); return false">',
          '<input type="text" id="input"/>',
          '<h3>Received</h3>',
          '<div id="output"/>',
          '</form>',
          "</body>",
          "</html>"
        )
      )
    }
  )                           
)

createServer <- function(port=7312L, nonBlocking = .Platform$OS == "unix") {
  server <- EpivizServer$new(port=port)
  return(server)
}
