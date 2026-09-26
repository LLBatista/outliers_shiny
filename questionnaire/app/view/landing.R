# Shiny module: the landing page (who, when, first run of the day, which experiment),
# with the language switch (EN | DE).
box::use(
  shiny,
)

box::use(
  app/logic/i18n[current_language, format_date, languages, tr],
  app/view/field_errors,
)

# Saved with every experiment answer (column run_type in data/responses.csv). The saved
# value is always the English code ("Regular", "Retest", "Pre-test"); only the label
# shown is translated.
run_type_labels <- function() {
  stats::setNames(
    c("Regular", "Retest", "Pre-test"),
    c(tr("run.regular"), tr("run.retest"), tr("run.pretest"))
  )
}

# "EN | DE": links that reload the page in the other language (the choice is also
# remembered in the browser, see main.R).
language_switch <- function() {
  current <- current_language()
  shiny$tags$nav(
    class = "lang-switch", `aria-label` = tr("landing.language"),
    lapply(names(languages), function(code) {
      shiny$tags$a(
        href = paste0("?lang=", code), lang = code, hreflang = code,
        title = languages[[code]],
        `aria-current` = if (code == current) "true",
        class = if (code == current) "active",
        toupper(code)
      )
    })
  )
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  german <- current_language() == "de"
  shiny$div(
    class = "landing-card",
    language_switch(),
    shiny$div(class = "landing-icon", shiny$icon("flask")),
    shiny$h2(id = ns("title"), tr("landing.welcome")),
    shiny$p(class = "subtitle", tr("landing.subtitle")),
    shiny$selectInput(ns("name"), tr("landing.name"),
      choices = NULL, selectize = FALSE, width = "100%"
    ),
    field_errors$message_ui(ns("name")),
    # No future dates: records can only be for today or earlier.
    shiny$dateInput(ns("experiment_date"), tr("landing.date"),
      value = Sys.Date(), max = Sys.Date(), width = "100%",
      format = if (german) "dd.mm.yyyy" else "yyyy-mm-dd",
      language = current_language(), weekstart = if (german) 1 else 0
    ),
    field_errors$message_ui(ns("experiment_date")),
    # Which instruments already had their daily check on the chosen date.
    shiny$uiOutput(ns("daily_status")),
    shiny$radioButtons(ns("first_run"), tr("landing.first_run"),
      choices = stats::setNames(c("yes", "no"), c(tr("common.yes"), tr("common.no"))),
      selected = character(0), inline = TRUE
    ),
    field_errors$message_ui(ns("first_run")),
    # Only shown when this is NOT the first run of the day.
    shiny$conditionalPanel(
      condition = "input.first_run == 'no'",
      ns = ns,
      shiny$selectInput(ns("experiment_type"), tr("landing.experiment_type"),
        choices = NULL, selectize = FALSE, width = "100%"
      ),
      field_errors$message_ui(ns("experiment_type")),
      # Once the experiment is chosen. "Regular" is chosen already (the usual case).
      shiny$conditionalPanel(
        condition = "input.experiment_type != ''",
        ns = ns,
        # Three options: stacked, one per line, so none wraps on its own.
        shiny$div(
          class = "stacked-options",
          shiny$radioButtons(ns("run_type"), tr("landing.run_type"),
            choices = run_type_labels(), selected = "Regular", inline = TRUE
          )
        ),
        field_errors$message_ui(ns("run_type"))
      )
    ),
    shiny$actionButton(ns("start"), tr("landing.start"),
      class = "btn-primary", icon = shiny$icon("arrow-right")
    )
  )
}

# The messages shown when Start is pressed (NULL = no problem with that field).
start_errors <- function(input) {
  experiment <- identical(input$first_run, "no")
  date <- input$experiment_date
  list(
    name = if (input$name == "") tr("landing.error_name"),
    experiment_date = if (!shiny$isTruthy(date)) {
      tr("landing.error_date")
    } else if (date > Sys.Date()) {
      tr("landing.error_future")
    },
    first_run = if (!shiny$isTruthy(input$first_run)) tr("landing.error_first_run"),
    experiment_type = if (experiment && input$experiment_type == "") {
      tr("landing.error_experiment")
    },
    run_type = if (experiment && !shiny$isTruthy(input$run_type)) tr("landing.error_run_type")
  )
}

#' `checked_on(date)`: returns the instruments that already had a daily check on
#' that date. It reads reactive data, so the list updates when a check is saved.
#' `first_run_answer()`: when it changes, "Is this the first run of the day?" is set to
#' its `answer` ("yes", "no", or NULL to clear it); with "no", focus goes to the
#' experiment list.
#' @export
server <- function(id, people, experiment_type, checked_on, first_run_answer) {
  shiny$moduleServer(id, function(input, output, session) {
    shiny$updateSelectInput(session, "name",
      choices = c(stats::setNames("", tr("landing.choose_name")), people)
    )
    shiny$updateSelectInput(session, "experiment_type",
      choices = c(stats::setNames("", tr("landing.choose_experiment")), experiment_type)
    )

    shiny$observeEvent(first_run_answer(), ignoreInit = TRUE, {
      answer <- first_run_answer()$answer
      shiny$updateRadioButtons(session, "first_run",
        selected = if (is.null(answer)) character(0) else answer
      )
      # The type of run goes back to "Regular", so a retest is not reused by mistake.
      shiny$updateRadioButtons(session, "run_type", selected = "Regular")
      if (identical(answer, "no")) {
        session$sendCustomMessage("focus-element", session$ns("experiment_type"))
      }
    })

    # Each message is shown under its field and removed when the field changes.
    field_errors$clear_on_change(input, session,
      c("name", "experiment_date", "first_run", "experiment_type", "run_type")
    )

    submission <- shiny$eventReactive(input$start, {
      shiny$req(field_errors$show(session, start_errors(input)))
      experiment <- identical(input$first_run, "no")
      list(
        name = input$name,
        experiment_date = input$experiment_date,
        first_run = input$first_run == "yes",
        experiment_type = input$experiment_type,
        run_type = if (experiment) input$run_type else ""
      )
    })

    # "Checked on 25 Sep 2026: Analyzer 01" - only the instruments already checked.
    output$daily_status <- shiny$renderUI({
      shiny$req(input$experiment_date)
      checked <- sort(checked_on(input$experiment_date))
      shiny$div(
        class = "daily-status",
        shiny$p(
          class = "daily-status-title",
          tr("landing.checked_on", date = format_date(input$experiment_date))
        ),
        if (length(checked) == 0) {
          shiny$p(class = "daily-status-empty", tr("landing.none_checked"))
        } else {
          shiny$tags$ul(lapply(checked, function(instrument) {
            shiny$tags$li(class = "done", shiny$icon("circle-check"), shiny$span(instrument))
          }))
        }
      )
    })

    submission
  })
}

#' How a saved run type is shown (e.g. "Retest" -> "Wiederholungstest").
#' @export
run_type_label <- function(code) {
  labels <- run_type_labels()
  if (code %in% labels) names(labels)[labels == code] else code
}
