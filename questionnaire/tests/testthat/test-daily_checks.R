box::use(
  testthat[expect_equal, expect_false, expect_true, test_that],
)
box::use(
  app/logic/daily_checks[
    already_checked, instrument_versions, load_daily_checks, lots_for_control, new_daily_checks
  ],
  app/logic/records[append_row],
)

control <- function(lot, valid = "yes") list(lot = lot, valid = valid, rerun_valid = "not needed")

test_that("instrument versions and control lots are looked up", {
  instruments <- data.frame(instrument = c("A1", "A2"), software_version = c("s1", "s2"),
                            firmware_version = c("f1", "f2"))
  expect_equal(instrument_versions(instruments, "A2"),
               list(software_version = "s2", firmware_version = "f2"))
  controls <- data.frame(control = c("Positive Control", "Positive Control", "Negative Control"),
                         lot = c("P2", "P1", "N1"))
  expect_equal(lots_for_control(controls, "Positive Control"), c("P1", "P2"))
})

test_that("a daily check is saved, and counts once per instrument per day", {
  path <- tempfile(fileext = ".csv")
  expect_equal(nrow(load_daily_checks(path)), 0)

  append_row(new_daily_checks("2026-09-01", "Ana", "A1", "s1", "f1",
                              control("P1"), control("N1")), path)
  checks <- load_daily_checks(path)
  expect_equal(checks$positive_lot, "P1")
  expect_true(checks$saved_at != "")

  expect_true(already_checked(checks, as.Date("2026-09-01"), "A1"))
  expect_false(already_checked(checks, "2026-09-01", "A2"))
  expect_false(already_checked(checks, "2026-09-02", "A1"))
})
