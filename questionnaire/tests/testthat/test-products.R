box::use(
  testthat[expect_equal, expect_error, expect_true, test_that],
)
box::use(
  app/logic/products[read_products],
)

test_that("read_products returns sorted, unique, non-empty products", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("product", "Sunscreen", "Shampoo", "", "Shampoo"), path)
  expect_equal(read_products(path), c("Shampoo", "Sunscreen"))
})

test_that("read_products complains about a missing 'product' column", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("name", "Shampoo"), path)
  expect_error(read_products(path), "column called 'product'")
})

test_that("the bundled products file can be read", {
  # Tests run from tests/testthat; `box.path` (set in .Rprofile) is the project root.
  path <- file.path(getOption("box.path"), "data", "products.csv")
  expect_true(length(read_products(path)) > 0)
})
