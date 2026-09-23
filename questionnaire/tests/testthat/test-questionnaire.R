box::use(
  shiny[testServer],
  testthat[expect_equal, expect_error, test_that],
)
box::use(
  app/view/questionnaire[server],
)

test_that("submitting the form returns the chosen product and date", {
  testServer(server, args = list(products = c("Shampoo", "Sunscreen")), {
    session$setInputs(product = "Sunscreen", date = as.Date("2026-09-10"), submit = 1)
    expect_equal(session$returned(), list(product = "Sunscreen", date = as.Date("2026-09-10")))
  })
})

test_that("submitting without a product is rejected", {
  testServer(server, args = list(products = c("Shampoo", "Sunscreen")), {
    session$setInputs(product = "", date = as.Date("2026-09-10"), submit = 1)
    expect_error(session$returned(), "Please choose a product.")
  })
})
