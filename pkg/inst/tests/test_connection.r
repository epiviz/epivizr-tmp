context("Connection management")

test_that("creating websocket conection works", {
  conn = new("EpivizDataConnection")
  expect_that(conn, is_a("EpivizDataConnection"))
  expect_that(conn@backend, is_a("environment"))
  expect_that(conn@backend$server_socket, is_a("integer"))
  expect_that(isClosed(conn), is_false())
  
  epivizClose(conn)
  expect_that(is.null(conn@backend$server_socket), is_true())
  expect_that(isClosed(conn), is_true())
})

