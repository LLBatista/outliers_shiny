box::use(
  testthat[expect_equal, expect_null, test_that],
)
box::use(
  app/logic/changes[
    add_logged_lots, apply_version_changes, last_version_change, log_change, lot_added_in_app,
    read_changes
  ],
)

test_that("changes are logged with user and time, and applied to the lists", {
  path <- tempfile(fileext = ".csv")
  expect_equal(nrow(read_changes(path)), 0)

  log_change(path, "Ana", "instrument", "A1", "firmware_version", "v1", "v2")
  log_change(path, "Ben", "instrument", "A1", "firmware_version", "v2", "v3")
  log_change(path, "Ana", "product", "P1", "lot", "", "L9")
  changes <- read_changes(path)
  expect_equal(changes$user, c("Ana", "Ben", "Ana"))
  expect_equal(sum(changes$changed_at == ""), 0)

  instruments <- data.frame(instrument = c("A1", "A2"), software_version = c("s1", "s1"),
                            firmware_version = c("v1", "v1"))
  expect_equal(apply_version_changes(instruments, changes)$firmware_version, c("v3", "v1"))

  products <- data.frame(product = "P1", lot = "L1")
  expect_equal(add_logged_lots(products, changes, "product", "product")$lot, c("L1", "L9"))
})

test_that("lots added in the app can be removed, and re-added", {
  path <- tempfile(fileext = ".csv")
  log_change(path, "Ana", "control", "Positive Control", "lot", "", "B7")
  log_change(path, "Ben", "control", "Positive Control", "lot_removed", "B7", "")
  controls <- data.frame(control = "Positive Control", lot = "A1")

  changes <- read_changes(path)
  expect_equal(add_logged_lots(controls, changes, "control", "control")$lot, "A1")
  expect_null(lot_added_in_app(changes, "control", "Positive Control", "B7"))
  expect_null(lot_added_in_app(changes, "control", "Positive Control", "A1"))

  log_change(path, "Ana", "control", "Positive Control", "lot", "", "B7")
  changes <- read_changes(path)
  expect_equal(add_logged_lots(controls, changes, "control", "control")$lot, c("A1", "B7"))
  expect_equal(lot_added_in_app(changes, "control", "Positive Control", "B7")$user, "Ana")
})

test_that("the last version change groups the rows logged together", {
  path <- tempfile(fileext = ".csv")
  log_change(path, "Ana", "instrument", "A1", "software_version", "s1", "s2", change_id = "x")
  log_change(path, "Ana", "instrument", "A1", "firmware_version", "v1", "v2", change_id = "x")
  log_change(path, "Ben", "instrument", "A2", "firmware_version", "v1", "v9", change_id = "y")

  last <- last_version_change(read_changes(path), "A1")
  expect_equal(last$field, c("software_version", "firmware_version"))
  expect_null(last_version_change(read_changes(path), "A3"))
})

test_that("a note can be logged with a change", {
  path <- tempfile(fileext = ".csv")
  log_change(path, "Ana", "control", "Positive Control", "lot", "", "B7", note = "new box")
  log_change(path, "Ben", "control", "Positive Control", "lot", "", "B8")
  expect_equal(read_changes(path)$note, c("new box", ""))
})
