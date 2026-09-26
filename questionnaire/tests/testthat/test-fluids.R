box::use(
  testthat[expect_equal, expect_null, test_that],
)
box::use(
  app/logic/changes[log_change, read_changes],
  app/logic/fluids[
    expiry_of, fluid_lot_item, fluids_with_changes, last_expiry_correction, lots_for_fluid
  ],
)

fluids <- data.frame(
  fluid = c("System buffer", "System fluid"),
  lot = c("SB1", "SF1"),
  expiry_date = c("2027-03-31", "2027-01-31")
)

test_that("lots and expiry dates come from the fluids list", {
  expect_equal(lots_for_fluid(fluids, "System buffer"), "SB1")
  expect_equal(expiry_of(fluids, "System fluid", "SF1"), "2027-01-31")
  expect_equal(expiry_of(fluids, "System fluid", "nope"), "")
})

test_that("a fluid lot added in the app keeps its logged expiry date", {
  path <- tempfile(fileext = ".csv")
  log_change(path, "Ana", "fluid", "System buffer", "lot", "", "SB2", change_id = "x")
  log_change(path, "Ana", "fluid", fluid_lot_item("System buffer", "SB2"), "expiry_date", "",
    "2027-05-31",
    change_id = "x"
  )

  updated <- fluids_with_changes(fluids, read_changes(path))
  expect_equal(lots_for_fluid(updated, "System buffer"), c("SB1", "SB2"))
  expect_equal(expiry_of(updated, "System buffer", "SB2"), "2027-05-31")
  expect_equal(expiry_of(updated, "System buffer", "SB1"), "2027-03-31")
})

test_that("a corrected expiry date is applied and can be found again", {
  path <- tempfile(fileext = ".csv")
  item <- fluid_lot_item("System fluid", "SF1")
  log_change(path, "Ana", "fluid", item, "expiry_date", "2027-01-31", "2027-02-28")

  changes <- read_changes(path)
  expect_equal(expiry_of(fluids_with_changes(fluids, changes), "System fluid", "SF1"), "2027-02-28")
  expect_equal(last_expiry_correction(changes, "System fluid", "SF1")$user, "Ana")
  expect_null(last_expiry_correction(changes, "System buffer", "SB1"))
})
