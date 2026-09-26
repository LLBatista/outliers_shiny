box::use(
  testthat[expect_equal, expect_match, expect_null, test_that],
)
box::use(
  app/view/inputs[clean_notes, limited_text, max_length, too_long],
)

test_that("values longer than the limit are reported", {
  expect_null(too_long("v0105", max_length$version))
  expect_equal(too_long("v01056", max_length$version), "Please use at most 5 characters.")
  expect_null(too_long("ABCDEFGHIJKL", max_length$lot))
  expect_equal(too_long("ABCDEFGHIJKLM", max_length$lot), "Please use at most 12 characters.")
})

test_that("the browser is told the limit too", {
  expect_match(as.character(limited_text("lot", "Lot", 12)), 'maxlength="12"')
})

test_that("notes are trimmed and cut at the limit", {
  expect_equal(clean_notes("  bottle was cold  "), "bottle was cold")
  expect_equal(clean_notes(NULL), "")
  expect_equal(nchar(clean_notes(strrep("a", 900))), max_length$notes)
})
