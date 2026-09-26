box::use(
  testthat[expect_equal, expect_warning, test_that],
  utils[read.csv],
)
box::use(
  app/logic/i18n[format_date, format_date_time, language_from_query, tr, with_language],
)

test_that("texts come in English and German, with placeholders filled in", {
  expect_equal(tr("lot.added", lot = "B7", lang = "en"), "Lot B7 added and logged.")
  expect_equal(tr("lot.added", lot = "B7", lang = "de"), "Charge B7 hinzugefügt und protokolliert.")
  expect_equal(with_language("de", tr("common.yes")), "Ja")
  expect_warning(expect_equal(tr("no.such.key"), "no.such.key"))
})

test_that("dates are written the way each language writes them", {
  expect_equal(format_date("2026-09-26", lang = "en"), "26 Sep 2026")
  expect_equal(format_date("2026-03-05", lang = "de"), "5. März 2026")
  expect_equal(format_date_time("2026-09-26 14:05:12 UTC", lang = "de"), "26. Sept. 2026 um 14:05")
})

test_that("the language comes from the page address, else the default", {
  expect_equal(language_from_query("?lang=de"), "de")
  expect_equal(language_from_query("?lang=fr", default = "en"), "en")
  expect_equal(language_from_query("", default = "de"), "de")
})

test_that("every text has a German version", {
  path <- file.path(getOption("box.path"), "app", "i18n", "translations.csv")
  texts <- read.csv(path, colClasses = "character", encoding = "UTF-8")
  expect_equal(texts$key[texts$de == ""], character(0))
  expect_equal(sum(duplicated(texts$key)), 0)
})
