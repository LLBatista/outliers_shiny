box::use(
  testthat[expect_equal, expect_error, test_that],
)
box::use(
  app/logic/options[read_options],
)

test_that("read_options returns sorted, unique, trimmed options", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("user", "Max Mustermann", " Leticia Batista", "Max Mustermann"), path)
  expect_equal(read_options(path, "user"), c("Leticia Batista", "Max Mustermann"))
})

test_that("read_options complains about a missing column or file", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("name", "Max"), path)
  expect_error(read_options(path, "user"), "must have a column called 'user'")
  expect_error(read_options(tempfile(), "user"), "File not found")
})
