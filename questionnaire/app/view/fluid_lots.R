# Shiny module: the "Instrument fluids" section of the experiment form.
# One lot of system fluid and one of system buffer are always needed (fluid_slot.R).
# Rarely a second lot of either is used: "Another lot used in this run?" offers
# System fluid / System buffer and opens a second slot for it.
box::use(
  shiny,
  stats[setNames],
)

box::use(
  app/logic/fluids[fluid_names],
  app/view/fluid_slot,
)

keys <- names(fluid_names) # "system_fluid", "system_buffer"

extra_id <- function(key) paste0("extra_", key)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  extras <- lapply(keys, function(key) {
    label <- fluid_names[[key]]
    shiny$conditionalPanel(
      condition = paste0("output.", extra_id(key)),
      ns = ns,
      shiny$div(
        class = "second-lot",
        fluid_slot$ui(ns(extra_id(key)), paste("Second", tolower(label))),
        shiny$actionButton(ns(paste0("remove_", key)), paste("Remove the second", tolower(label)),
          class = "btn-link link-action", icon = shiny$icon("xmark")
        )
      )
    )
  })
  add_buttons <- lapply(keys, function(key) {
    shiny$conditionalPanel(
      condition = paste0("!output.", extra_id(key)),
      ns = ns,
      shiny$actionButton(ns(paste0("add_", key)), fluid_names[[key]], icon = shiny$icon("plus"))
    )
  })
  shiny$div(
    class = "app-card",
    shiny$h3(shiny$icon("droplet"), "Instrument fluids"),
    lapply(keys, function(key) fluid_slot$ui(ns(key), fluid_names[[key]])),
    extras,
    # Hidden once both second lots are open.
    shiny$conditionalPanel(
      condition = paste(paste0("!output.", extra_id(keys)), collapse = " || "),
      ns = ns,
      shiny$div(
        class = "add-extra",
        shiny$span(class = "add-extra-label", "Another lot used in this run? (rare)"),
        shiny$div(class = "add-extra-buttons", add_buttons)
      )
    )
  )
}

#' Arguments: `fluids()`, `experiment_date()`, `changes()`, `add_lot`, `remove_lot`,
#' `undo_expiry` - see fluid_slot.R.
#'
#' Returns a list:
#' - `collect(focus)`: NULL after showing what is missing, or a list with
#'   `answer` (system_fluid_lot, system_fluid_expiry, system_fluid_lot_2, ...) and
#'   `corrections` (expiry dates to correct in the list, see fluid_slot.R)
#' @export
server <- function(id, fluids, experiment_date, changes, add_lot, remove_lot, undo_expiry) {
  shiny$moduleServer(id, function(input, output, session) {
    slot <- function(slot_id, key, other_lot = shiny$reactive(NULL)) {
      fluid_slot$server(slot_id,
        fluid = shiny$reactive(fluid_names[[key]]),
        fluids = fluids,
        experiment_date = experiment_date,
        changes = changes,
        add_lot = add_lot,
        remove_lot = remove_lot,
        undo_expiry = undo_expiry,
        other_lot = other_lot
      )
    }
    main <- lapply(setNames(keys, keys), function(key) slot(key, key))
    extra <- lapply(setNames(keys, keys), function(key) {
      slot(extra_id(key), key, other_lot = main[[key]]$lot)
    })

    # Which second lots are open.
    open <- lapply(setNames(keys, keys), function(key) shiny$reactiveVal(FALSE))
    lapply(keys, function(key) {
      output[[extra_id(key)]] <- shiny$reactive(open[[key]]())
      shiny$outputOptions(output, extra_id(key), suspendWhenHidden = FALSE)
      shiny$observeEvent(input[[paste0("add_", key)]], {
        open[[key]](TRUE)
        session$sendCustomMessage("focus-element", session$ns(paste0(extra_id(key), "-title")))
      })
      shiny$observeEvent(input[[paste0("remove_", key)]], {
        open[[key]](FALSE)
        extra[[key]]$reset()
      })
    })

    # Top to bottom: the two fluids, then the open second lots. Focus goes to the
    # first problem only.
    collect <- function(focus = TRUE) {
      slots <- c(
        lapply(setNames(keys, keys), function(key) main[[key]]),
        lapply(setNames(keys, paste0(keys, "_2")), function(key) {
          if (open[[key]]()) extra[[key]]
        })
      )
      slots <- Filter(Negate(is.null), slots)
      results <- list()
      complete <- TRUE
      for (name in names(slots)) {
        result <- slots[[name]]$collect(focus && complete)
        complete <- complete && !is.null(result)
        if (!is.null(result)) results[[name]] <- result
      }
      if (!complete) {
        return(NULL)
      }
      answer <- list()
      for (name in names(results)) {
        # "system_fluid" -> system_fluid_lot / _expiry; "system_fluid_2" -> _lot_2 / _expiry_2
        key <- sub("_2$", "", name)
        suffix <- if (grepl("_2$", name)) "_2" else ""
        answer[[paste0(key, "_lot", suffix)]] <- results[[name]]$lot
        answer[[paste0(key, "_expiry", suffix)]] <- results[[name]]$expiry
      }
      corrections <- Filter(Negate(is.null), lapply(results, function(r) r$correction))
      list(answer = answer, corrections = unname(corrections))
    }

    list(collect = collect)
  })
}
