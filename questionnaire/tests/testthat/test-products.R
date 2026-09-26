box::use(
  testthat[expect_equal, expect_error, expect_true, test_that],
)
box::use(
  app/logic/products[lots_for_product, read_products],
)

test_that("read_products reads the products and their lots", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("product, lot", "ORG200, A2", "ORG200, A1", "ORG210, B1"), path)
  products <- read_products(path)
  expect_equal(products$product, c("ORG200", "ORG200", "ORG210"))
  expect_equal(products$lot, c("A2", "A1", "B1"))
})

test_that("read_products complains about a missing column or file", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("name", "ORG200"), path)
  expect_error(read_products(path), "columns 'product' and 'lot'")
  expect_error(read_products(tempfile()), "Products file not found")
})

test_that("lots_for_product returns the sorted, unique lots of one product", {
  products <- data.frame(product = c("ORG200", "ORG200", "ORG200", "ORG210"),
                         lot = c("A2", "A1", "A2", "B1"))
  expect_equal(lots_for_product(products, "ORG200"), c("A1", "A2"))
  expect_equal(lots_for_product(products, "nope"), character(0))
})

test_that("the bundled products file can be read", {
  # Tests run from tests/testthat; `box.path` (set in .Rprofile) is the project root.
  path <- file.path(getOption("box.path"), "data", "products.csv")
  expect_true(nrow(read_products(path)) > 0)
})
