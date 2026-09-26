# Shiny module: the form every experiment starts with (Detection Capability, Linearity,
# and experiments to come). Sections:
#   1. instrument, product and lot    (insert_product.R)
#   2. instrument fluids              (fluid_lots.R, once per fluid)
#   3. sample preparation             (sample_preparation.R)
# "Save answer" checks all sections and returns everything as one answer.
box::use(
  shiny,
  stats[setNames],
)

box::use(
  app/logic/fluids[fluid_names],
  app/view/fluid_lots,
  app/view/insert_product,
  app/view/sample_preparation,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$tagList(
    insert_product$ui(ns("product")),
    shiny$div(
      class = "app-card",
      shiny$h3(shiny$icon("droplet"), "Instrument fluids"),
      fluid_lots$ui(ns("system_buffer"), "System buffer"),
      fluid_lots$ui(ns("system_fluid"), "System fluid")
    ),
    sample_preparation$ui(ns("samples")),
    shiny$div(
      class = "save-row",
      shiny$actionButton(ns("save"), "Save answer",
        class = "btn-primary", icon = shiny$icon("check")
      ),
      # Confirmation after saving; screen readers read it out (it is a live region).
      shiny$div(class = "form-success", shiny$textOutput(ns("saved")))
    )
  )
}

#' Arguments:
#' - `product_id()`, `checked_instruments()`, `add_product_lot`, `remove_product_lot`:
#'   see insert_product.R
#' - `fluids()`, `add_fluid_lot`, `remove_fluid_lot`: see fluid_lots.R
#' - `experiment_date()`: the date chosen on the landing page
#' - `changes()`: the change log
#' Returns a reactive with the latest saved answer (a named list, see responses.R).
#' @export
server <- function(id, product_id, checked_instruments, changes, add_product_lot,
                   remove_product_lot, fluids, experiment_date, add_fluid_lot,
                   remove_fluid_lot) {
  shiny$moduleServer(id, function(input, output, session) {
    product <- insert_product$server("product",
      product_id = product_id,
      checked_instruments = checked_instruments,
      changes = changes,
      add_product_lot = add_product_lot,
      remove_product_lot = remove_product_lot
    )
    fluid_sections <- lapply(names(fluid_names), function(key) {
      fluid_lots$server(key,
        fluid = fluid_names[[key]],
        fluids = fluids,
        experiment_date = experiment_date,
        changes = changes,
        add_lot = add_fluid_lot,
        remove_lot = remove_fluid_lot
      )
    })
    names(fluid_sections) <- names(fluid_names)
    samples <- sample_preparation$server("samples")

    # Checks every section, top to bottom; focus goes to the first problem only.
    submission <- shiny$eventReactive(input$save, {
      answer <- product$collect(focus = TRUE)
      complete <- !is.null(answer)
      for (key in names(fluid_sections)) {
        fluid <- fluid_sections[[key]]$collect(focus = complete)
        complete <- complete && !is.null(fluid)
        if (!is.null(fluid)) answer <- c(answer, setNames(fluid, paste0(key, "_", names(fluid))))
      }
      preparation <- samples$collect(focus = complete)
      shiny$req(complete, preparation)
      c(answer, preparation)
    })

    # After a save: confirm it and empty product and lot. Fluids and sample preparation
    # stay, as they are usually the same for the next answer of this run.
    saved_message <- shiny$reactiveVal("")
    shiny$observeEvent(product$product(), if (product$product() != "") saved_message(""))
    output$saved <- shiny$renderText(saved_message())

    shiny$observeEvent(submission(), {
      answer <- submission()
      saved_message(paste0(
        "Answer saved: ", answer$product, ", lot ", answer$lot, ", on ", answer$instrument, "."
      ))
      product$clear()
    })

    submission
  })
}
