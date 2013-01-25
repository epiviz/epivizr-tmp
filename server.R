require("websockets");

data_received <- function(DATA, WS, HEADER) {
  # serve data here
  websocket_write("You've been served grandma", WS)
  #show(DATA);
  print(paste("Received:", rawToChar(DATA)));
}

con_established <- function(DATA, WS, HEADER) {
  # serve data here
  websocket_write("You've been served baby", WS)
  #show(DATA);
  print("Established:");
}

con_closed <- function(DATA, WS, HEADER) {
  # serve data here
  #websocket_write("You've been served", WS)
  #show(DATA);
  print("Closed:");
  stop <<- TRUE;
}

initializeServer <- function(port=7312L) {
  server = websockets::create_server(port=port);
  #tryCatch(
    #{
      websockets::setCallback("receive", data_received, server);
      websockets::setCallback("established", con_established, server);
      websockets::setCallback("closed", con_closed, server);
      #demonize(server);
    #},
    #error = function(e) {
      #websockets::websocket_close(server);
      #return(NULL);
    #});

    return(server);
}

server = initializeServer();
print(server);

stop = FALSE;

while(!stop)
{
    #print("Listening...");
    websockets::service(server);
}

websocket_close(server);