context("server")

test_that("stop shuts down the socket connection", {
  server=epivizr::createServer(port=7000L)
  server$stop()
  expect_true(server$isClosed())
})

test_that("createServer creates a proper object", {
  server <- epivizr::createServer()
  expect_is(server, "EpivizServer")

  expect_is(server$websocket, "environment")
  server$stop()
})

