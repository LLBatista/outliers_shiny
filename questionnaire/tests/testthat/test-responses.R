box::use(
  testthat[expect_equal, expect_true, test_that],
  utils[write.csv],
)
box::use(
  app/logic/records[append_row],
  app/logic/responses[load_responses, new_response],
)

answer <- function(product, lot, ...) {
  list(instrument = "Analyzer 01", product = product, lot = lot, ...)
}

test_that("answers are appended to the CSV and read back", {
  path <- tempfile(fileext = ".csv")
  expect_equal(nrow(load_responses(path)), 0)

  first <- answer("P1", "L1",
    system_buffer_lot = "SB1", system_buffer_expiry = "2027-01-31",
    samples_vortexed = "yes", samples_thawed = "yes", thaw_minutes = 30
  )
  append_row(new_response("2026-09-01", "Ana", "Linearity", "Regular", first), path)
  second <- answer("P2", "L2", samples_thawed = "no")
  append_row(new_response(as.Date("2026-09-02"), "Ben", "Linearity", "Retest", second), path)

  saved <- load_responses(path)
  expect_equal(saved$product, c("P1", "P2"))
  expect_equal(saved$date, c("2026-09-01", "2026-09-02"))
  expect_equal(saved$run_type, c("Regular", "Retest"))
  expect_equal(saved$system_buffer_lot, c("SB1", ""))
  expect_equal(saved$thaw_minutes, c("30", ""))
  expect_true(all(saved$saved_at != ""))
})

test_that("a file from an older version (fewer columns) is upgraded", {
  path <- tempfile(fileext = ".csv")
  old <- data.frame(date = "2026-08-01", user = "Ana", experiment = "Linearity",
                    product = "P1", lot = "L1")
  write.csv(old, path, row.names = FALSE)

  append_row(new_response("2026-09-01", "Ana", "Linearity", "Pre-test", answer("P2", "L2")), path)

  saved <- load_responses(path)
  expect_equal(saved$product, c("P1", "P2"))
  expect_equal(saved$instrument, c("", "Analyzer 01"))
  expect_equal(saved$run_type, c("", "Pre-test"))
})
