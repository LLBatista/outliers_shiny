box::use(
  testthat[expect_equal, expect_true, test_that],
  utils[write.csv],
)
box::use(
  app/logic/records[append_row],
  app/logic/responses[load_responses, new_response],
)

test_that("answers are appended to the CSV and read back", {
  path <- tempfile(fileext = ".csv")
  expect_equal(nrow(load_responses(path)), 0)

  append_row(new_response("2026-09-01", "Ana", "Linearity", "Analyzer 01", "P1", "L1"), path)
  second <- new_response(as.Date("2026-09-02"), "Ben", "Linearity", "Analyzer 02", "P2", "L2")
  append_row(second, path)

  saved <- load_responses(path)
  expect_equal(saved$product, c("P1", "P2"))
  expect_equal(saved$date, c("2026-09-01", "2026-09-02"))
  expect_equal(saved$instrument, c("Analyzer 01", "Analyzer 02"))
  expect_true(all(saved$saved_at != ""))
})

test_that("a file from an older version (no instrument column) is upgraded", {
  path <- tempfile(fileext = ".csv")
  old <- data.frame(date = "2026-08-01", user = "Ana", experiment = "Linearity",
                    product = "P1", lot = "L1")
  write.csv(old, path, row.names = FALSE)

  append_row(new_response("2026-09-01", "Ana", "Linearity", "Analyzer 01", "P2", "L2"), path)

  saved <- load_responses(path)
  expect_equal(saved$product, c("P1", "P2"))
  expect_equal(saved$instrument, c("", "Analyzer 01"))
})
