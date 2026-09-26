box::use(
  testthat[expect_equal, test_that],
)
box::use(
  app/logic/changes[log_change, read_changes],
  app/logic/fluids[expiry_of, fluid_lot_item, fluids_with_changes, lots_for_fluid],
)

fluids <- data.frame(
  fluid = c("System Buffer", "System Fluid"),
  lot = c("SB1", "SF1"),
  expiry_date = c("2027-03-31", "2027-01-31")
)

test_that("lots and expiry dates come from the fluids list", {
  expect_equal(lots_for_fluid(fluids, "System Buffer"), "SB1")
  expect_equal(expiry_of(fluids, "System Fluid", "SF1"), "2027-01-31")
  expect_equal(expiry_of(fluids, "System Fluid", "nope"), "")
})

test_that("a fluid lot added in the app keeps its logged expiry date", {
  path <- tempfile(fileext = ".csv")
  log_change(path, "Ana", "fluid", "System Buffer", "lot", "", "SB2", change_id = "x")
  log_change(path, "Ana", "fluid", fluid_lot_item("System Buffer", "SB2"), "expiry_date", "",
    "2027-05-31",
    change_id = "x"
  )

  updated <- fluids_with_changes(fluids, read_changes(path))
  expect_equal(lots_for_fluid(updated, "System Buffer"), c("SB1", "SB2"))
  expect_equal(expiry_of(updated, "System Buffer", "SB2"), "2027-05-31")
  expect_equal(expiry_of(updated, "System Buffer", "SB1"), "2027-03-31")
})
