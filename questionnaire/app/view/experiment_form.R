# Shiny module: the steps every experiment starts with (Detection Capability,
# Linearity, and experiments to come), one at a time like the daily check:
#   1. instrument, product and lot    (insert_product.R)     -> Next
#   2. instrument fluids              (fluid_lots.R)         -> Next
#   3. sample preparation + notes     (sample_preparation.R) -> Save answer
#   done: a summary, and "Add another product" (instrument, fluids and sample
#   preparation stay filled in, and are shown again before the next save)
box::use(
  shiny,
)

box::use(
  app/logic/i18n[format_date, tr],
  app/logic/responses[describe_samples],
  app/view/fluid_lots,
  app/view/inputs,
  app/view/insert_product,
  app/view/sample_preparation,
  app/view/steps,
)

step_labels <- function() {
  c(
    product = tr("form.step_product"), fluids = tr("form.step_fluids"),
    samples = tr("form.step_samples")
  )
}

step_buttons <- function(ns, back = NULL, next_id, next_label, next_icon = "arrow-right") {
  shiny$div(
    class = "step-buttons",
    if (!is.null(back)) {
      shiny$actionButton(ns(back), tr("common.back"), icon = shiny$icon("arrow-left"))
    },
    shiny$actionButton(ns(next_id), next_label, class = "btn-primary", icon = shiny$icon(next_icon))
  )
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$div(
    class = "app-card experiment-steps",
    shiny$uiOutput(ns("progress")),
    # One hidden tab per step; the server decides which one is shown.
    shiny$tabsetPanel(
      id = ns("steps"),
      type = "hidden",
      shiny$tabPanel(
        "product",
        insert_product$ui(ns("product")),
        step_buttons(ns, next_id = "next_product", next_label = tr("common.next"))
      ),
      shiny$tabPanel(
        "fluids",
        fluid_lots$ui(ns("fluids")),
        step_buttons(ns,
          back = "back_fluids", next_id = "next_fluids", next_label = tr("common.next")
        )
      ),
      shiny$tabPanel(
        "samples",
        sample_preparation$ui(ns("samples")),
        inputs$notes(ns("notes"), tr("form.notes")),
        step_buttons(ns,
          back = "back_samples", next_id = "save", next_label = tr("form.save"),
          next_icon = "check"
        )
      ),
      shiny$tabPanel(
        "done",
        # role = "status": screen readers read the summary out when it appears.
        shiny$div(role = "status", shiny$uiOutput(ns("summary"))),
        shiny$p(
          class = "save-hint",
          tr("form.next_hint")
        ),
        step_buttons(ns,
          next_id = "another", next_label = tr("form.another"), next_icon = "plus"
        )
      )
    )
  )
}

# The summary after saving: what was saved, and any expiry date corrected in the list.
summary_ui <- function(answer, corrections, title_id) {
  lots <- function(prefix) {
    lot <- function(suffix) {
      value <- answer[[paste0(prefix, "_lot", suffix)]]
      if (length(value) == 0 || value == "") {
        return(NULL)
      }
      expiry <- answer[[paste0(prefix, "_expiry", suffix)]]
      tr("summary.lot_expires", lot = value, date = format_date(expiry))
    }
    paste(c(lot(""), lot("_2")), collapse = tr("common.and"))
  }
  rows <- list(
    list(tr("common.instrument"), answer$instrument),
    list(
      tr("product.product"),
      tr("summary.product_lot", product = answer$product, lot = answer$lot)
    ),
    list(tr("fluid.system_fluid"), lots("system_fluid")),
    list(tr("fluid.system_buffer"), lots("system_buffer")),
    list(tr("summary.samples"), describe_samples(
      answer$samples_vortexed, answer$samples_thawed, answer$thaw_minutes
    )),
    if (answer$notes != "") list(tr("summary.notes"), answer$notes)
  )
  rows <- Filter(Negate(is.null), rows)
  shiny$div(
    class = "done-summary",
    shiny$h3(id = title_id, class = "step-title", shiny$icon("circle-check"), tr("summary.saved")),
    shiny$tags$dl(lapply(rows, function(row) {
      shiny$tagList(shiny$tags$dt(row[[1]]), shiny$tags$dd(row[[2]]))
    })),
    lapply(corrections, function(c) {
      shiny$p(
        class = "change-note", shiny$icon("clock-rotate-left"),
        tr("summary.corrected", lot = c$lot, date = format_date(c$new))
      )
    })
  )
}

#' Arguments:
#' - `product_id()`, `checked_instruments()`, `add_product_lot`, `remove_product_lot`:
#'   see insert_product.R
#' - `fluids()`, `add_fluid_lot`, `remove_fluid_lot`, `undo_expiry`: see fluid_slot.R
#' - `correct_expiry(fluid, lot, old, new, note)`: corrects a lot's expiry date in the list
#' - `experiment_date()`: the date chosen on the landing page
#' - `changes()`: the change log
#' - `start()`: changes when the user starts from the landing page (back to step 1)
#' Returns a reactive with the latest saved answer (a named list, see responses.R).
#' @export
server <- function(id, product_id, checked_instruments, changes, add_product_lot,
                   remove_product_lot, fluids, experiment_date, add_fluid_lot,
                   remove_fluid_lot, correct_expiry, undo_expiry, start) {
  shiny$moduleServer(id, function(input, output, session) {
    product <- insert_product$server("product",
      product_id = product_id,
      checked_instruments = checked_instruments,
      changes = changes,
      add_product_lot = add_product_lot,
      remove_product_lot = remove_product_lot
    )
    fluid_section <- fluid_lots$server("fluids",
      fluids = fluids,
      experiment_date = experiment_date,
      changes = changes,
      add_lot = add_fluid_lot,
      remove_lot = remove_fluid_lot,
      undo_expiry = undo_expiry
    )
    samples <- sample_preparation$server("samples")

    # --- Moving between steps ---------------------------------------------------------
    current_step <- shiny$reactiveVal("product")
    step_headings <- c(
      product = "product-title", fluids = "fluids-title", samples = "samples-title",
      done = "done_title"
    )
    go_to <- function(step, focus = TRUE) {
      current_step(step)
      shiny$updateTabsetPanel(session, "steps", selected = step)
      if (focus) session$sendCustomMessage("focus-element", session$ns(step_headings[[step]]))
    }
    output$progress <- shiny$renderUI(steps$progress(step_labels(), current_step()))

    # Coming from the landing page, focus goes to the page title instead (main.R).
    shiny$observeEvent(start(), go_to("product", focus = FALSE))

    shiny$observeEvent(input$next_product, {
      if (!is.null(product$collect(focus = TRUE))) go_to("fluids")
    })
    shiny$observeEvent(input$next_fluids, {
      if (!is.null(fluid_section$collect(focus = TRUE))) go_to("samples")
    })
    shiny$observeEvent(input$back_fluids, go_to("product"))
    shiny$observeEvent(input$back_samples, go_to("fluids"))

    # --- Saving ------------------------------------------------------------------------
    # The earlier steps are checked again (a list may have changed meanwhile): if one
    # is no longer complete, the user is taken back to it.
    submission <- shiny$eventReactive(input$save, {
      preparation <- samples$collect(focus = TRUE)
      shiny$req(preparation)
      answer <- product$collect(focus = FALSE)
      if (is.null(answer)) {
        go_to("product", focus = FALSE)
        product$collect(focus = TRUE)
        shiny$req(FALSE)
      }
      fluid <- fluid_section$collect(focus = FALSE)
      if (is.null(fluid)) {
        go_to("fluids", focus = FALSE)
        fluid_section$collect(focus = TRUE)
        shiny$req(FALSE)
      }
      for (c in fluid$corrections) correct_expiry(c$fluid, c$lot, c$old, c$new, c$note)
      answer <- c(answer, fluid$answer, preparation, list(notes = inputs$clean_notes(input$notes)))
      list(answer = answer, corrections = fluid$corrections)
    })

    output$summary <- shiny$renderUI({
      saved <- submission()
      summary_ui(saved$answer, saved$corrections, session$ns("done_title"))
    })
    shiny$observeEvent(submission(), go_to("done"))

    # The next product: product, lot and notes are emptied; the rest stays.
    shiny$observeEvent(input$another, {
      product$clear()
      shiny$updateTextAreaInput(session, "notes", value = "")
      go_to("product")
    })

    shiny$reactive(submission()$answer)
  })
}
