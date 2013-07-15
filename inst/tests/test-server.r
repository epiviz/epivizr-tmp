context("server")

mgr<-list(getData=function(measurements, chr, start, end) {
  return(chr)
})

test_that("createServer creates a proper object", {
  server <- epivizr:::createServer(port=7123L)
  expect_is(server, "EpivizServer")
  expect_true(server$isClosed())
})

test_that("startServer and stopServer work appropriately", {
  server <- epivizr:::createServer(port=7123L)
  expect_true(server$isClosed())
  
  server$startServer()
  expect_false(server$isClosed())
  
  server$stopServer()
  expect_true(server$isClosed())
})

test_that("socket messaging works", {
  server <- epivizr:::createServer(port=7123L)
  server$bindManager(mgr)
  server$startServer()
  
  browseURL("http://localhost:7123/")
  tryCatch(server$service(), interrupt=function(int) invisible())
  
  expect_false(server$isClosed())
  
  server$stopServer()
  expect_true(server$isClosed())
})
  

test_that("runServer works", {
  server <- epivizr:::createServer(port=7123L)
  server$bindManager(mgr)
  
  browseURL("http://localhost:7123/")
  tryCatch(server$runServer(), interrupt=function(int) invisible())
  
  expect_true(server$isClosed())
})


