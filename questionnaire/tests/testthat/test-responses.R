box::use(
  testthat[expect_equal, test_that],
)
box::use(
  app/logic/responses[load_responses, new_response, save_response],
)

test_that("answers are appended to the CSV and read back", {
  path <- tempfile(fileext = ".csv")
  expect_equal(nrow(load_responses(path)), 0)

  save_response(new_response("Shampoo", "2026-09-01"), path)
  save_response(new_response("Sunscreen", as.Date("2026-09-02")), path)

  saved <- load_responses(path)
  expect_equal(saved$product, c("Shampoo", "Sunscreen"))
  expect_equal(saved$date, c("2026-09-01", "2026-09-02"))
})
