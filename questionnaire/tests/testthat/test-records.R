box::use(
  testthat[expect_equal, test_that],
  utils[read.csv, write.csv],
)
box::use(
  app/logic/records[append_row, read_table],
)

test_that("read_table gives an empty table with the columns when there is no file", {
  table <- read_table(tempfile(), c("a", "b"))
  expect_equal(names(table), c("a", "b"))
  expect_equal(nrow(table), 0)
})

test_that("read_table reads everything as text and adds missing columns", {
  path <- tempfile(fileext = ".csv")
  write.csv(data.frame(a = c("01", NA)), path, row.names = FALSE)
  table <- read_table(path, c("a", "b"))
  expect_equal(table$a, c("01", ""))
  expect_equal(table$b, c("", ""))
})

test_that("append_row creates the file, then upgrades an older header", {
  path <- tempfile(fileext = ".csv")
  append_row(data.frame(a = "1"), path)
  append_row(data.frame(a = "2", b = "x"), path)
  saved <- read.csv(path, colClasses = "character")
  expect_equal(names(saved), c("a", "b"))
  expect_equal(saved$b, c("", "x"))
  expect_equal(read_table(path, c("a", "b"))$b, c("", "x"))
})
