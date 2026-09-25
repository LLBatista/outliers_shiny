box::use(
  testthat[expect_equal, test_that],
)
box::use(
  app/logic/changes[add_logged_lots, apply_version_changes, log_change, read_changes],
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
