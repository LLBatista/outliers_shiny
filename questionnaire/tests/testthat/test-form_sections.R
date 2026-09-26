# Server tests of two sections of the experiment form, with simulated inputs.
box::use(
  shiny[isolate, reactive, testServer],
  testthat[expect_equal, expect_null, test_that],
)
box::use(
  app/view/insert_product,
  app/view/sample_preparation,
)

test_that("sample preparation needs all answers, and a plausible thaw time", {
  testServer(sample_preparation$server, {
    collect <- function() isolate(session$returned$collect(focus = FALSE))
    expect_null(collect())

    session$setInputs(vortexed = "yes", thawed = "yes", thaw_minutes = 5000)
    expect_null(collect())

    session$setInputs(thaw_minutes = 30)
    expect_equal(collect(), list(samples_vortexed = "yes", samples_thawed = "yes",
                                 thaw_minutes = 30))

    session$setInputs(thawed = "no")
    expect_equal(collect()$thaw_minutes, "")
  })
})

test_that("an experiment can only use an instrument with a daily check", {
  products <- data.frame(product = "ORG200", lot = c("A1", "A2"))
  testServer(insert_product$server, args = list(
    product_id = reactive(products),
    checked_instruments = reactive("Analyzer 01"),
    changes = reactive(data.frame()),
    add_product_lot = function(...) NULL,
    remove_product_lot = function(...) NULL
  ), {
    collect <- function() isolate(session$returned$collect(focus = FALSE))

    session$setInputs(instrument = "Analyzer 02", product = "ORG200", lot = "A1")
    expect_null(collect())

    session$setInputs(instrument = "Analyzer 01")
    expect_equal(collect(), list(instrument = "Analyzer 01", product = "ORG200", lot = "A1"))
  })
})
