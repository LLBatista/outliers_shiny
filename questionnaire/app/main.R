# Entry point of the app: puts the pages and modules together.
#
# How the app flows:
#   1. Landing page: the user picks their name, the date and whether this is the
#      first run of the day (if not, they also pick the experiment).
#   2a. First run of the day -> "daily_check" page (instrument + control lots),
#       saved to data/daily_checks.csv (one check per instrument per day).
#   2b. Otherwise -> "questionnaire" page, showing the page of the chosen
#       experiment, saved to data/responses.csv.

box::use(
  bslib,
  config,
  shiny,
)

box::use(
  app/logic/daily_checks[already_checked, load_daily_checks, new_daily_checks, read_controls],
  app/logic/options[read_options],
  app/logic/products[read_products],
  app/logic/responses[load_responses, new_response, save_response],
  app/view/daily_check,
  app/view/detection_capability,
  app/view/landing,
  app/view/linearity,
  app/view/responses_table,
)

# ============================================================================
# UI
# ============================================================================

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$fluidPage(
    title = "Lab Documentation Prototype",

    # Colours and font for the whole app (card styles live in app/styles/main.scss)
    theme = bslib$bs_theme(
      version = 5,
      primary = "#2f6f73", # buttons and accents
      bg = "#f4f6f8", # page background
      fg = "#1f2933", # text colour
      base_font = bslib$font_collection(
        "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
      )
    ),

    # The pages of the app. `type = "hidden"` hides the tab buttons:
    # the server decides which page is shown (see "Routing" below).
    shiny$tabsetPanel(
      id = ns("pages"),
      type = "hidden",

      # --- Page 1: landing -------------------------------------------------
      shiny$tabPanel(
        "landing",
        landing$ui(ns("landing"))
      ),

      # --- Page 2a: first run of the day -----------------------------------
      shiny$tabPanel(
        "daily_check",
        shiny$div(
          class = "questionnaire-page",
          shiny$div(
            class = "page-header",
            shiny$h2("Lab Documentation Prototype"),
            shiny$uiOutput(ns("daily_info")) # chips: user, date, "first run"
          ),
          daily_check$ui(ns("daily_check")),
          responses_table$ui(ns("daily_checks_table"))
        )
      ),

      # --- Page 2b: experiments --------------------------------------------
      shiny$tabPanel(
        "questionnaire",
        shiny$div(
          class = "questionnaire-page",
          shiny$div(
            class = "page-header",
            shiny$h2("Lab Documentation Prototype"),
            shiny$uiOutput(ns("experiment_info")) # chips: user, date, experiment
          ),
          # One page per experiment. The tab names must match data/assays.csv exactly.
          shiny$tabsetPanel(
            id = ns("experiment_pages"),
            type = "hidden",
            shiny$tabPanel(
              "Detection Capability",
              detection_capability$ui(ns("detection_capability"))
            ),
            shiny$tabPanel(
              "Linearity",
              linearity$ui(ns("linearity"))
            )
          ),
          responses_table$ui(ns("responses"))
        )
      )
    )
  )
}

# ============================================================================
# Server
# ============================================================================

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    # ------------------------------------------------------------------------
    # 1. Settings and options (file paths come from config.yml)
    # ------------------------------------------------------------------------
    responses_file <- config$get("responses_file")
    daily_checks_file <- config$get("daily_checks_file")

    people <- read_options(config$get("users_file"), "user")
    experiment_types <- read_options(config$get("assays_file"), "assay")
    instruments <- read_options(config$get("instruments_file"), "instrument")
    controls <- read_controls(config$get("controls_file")) # columns: control, lot
    products <- read_products(config$get("products_file")) # columns: product, lot

    # ------------------------------------------------------------------------
    # 2. Landing page and routing
    # ------------------------------------------------------------------------
    # `experiment()` holds what the user chose on the landing page:
    # name, experiment_date, first_run (TRUE/FALSE) and experiment_type.
    experiment <- landing$server("landing",
      people = people,
      experiment_type = experiment_types
    )

    # When the user clicks Start, show the right second page.
    shiny$observeEvent(experiment(), {
      info <- experiment()
      if (info$first_run) {
        shiny$updateTabsetPanel(session, "pages", selected = "daily_check")
      } else {
        shiny$updateTabsetPanel(session, "experiment_pages", selected = info$experiment_type)
        shiny$updateTabsetPanel(session, "pages", selected = "questionnaire")
      }
    })

    # ------------------------------------------------------------------------
    # 3. Page headers: the "chips" under the title
    # ------------------------------------------------------------------------
    output$daily_info <- shiny$renderUI({
      info <- experiment()
      shiny$div(
        class = "info-chips",
        shiny$span(class = "chip", shiny$icon("user"), info$name),
        shiny$span(class = "chip", shiny$icon("calendar"), format(info$experiment_date)),
        shiny$span(class = "chip", shiny$icon("sun"), "First run of the day")
      )
    })

    output$experiment_info <- shiny$renderUI({
      info <- experiment()
      shiny$div(
        class = "info-chips",
        shiny$span(class = "chip", shiny$icon("user"), info$name),
        shiny$span(class = "chip", shiny$icon("calendar"), format(info$experiment_date)),
        shiny$span(class = "chip", shiny$icon("flask"), info$experiment_type)
      )
    })

    # ------------------------------------------------------------------------
    # 4. First run of the day (daily check)
    # ------------------------------------------------------------------------
    # All saved daily checks; starts with what is already in the CSV.
    daily_checks <- shiny$reactiveVal(load_daily_checks(daily_checks_file))

    # `daily_submission()` holds the form values after the user clicks Save.
    daily_submission <- daily_check$server("daily_check",
      instruments = instruments,
      controls = controls
    )

    # Save the check, unless this instrument was already checked on this date.
    shiny$observeEvent(daily_submission(), {
      check <- daily_submission() # from the daily check form
      info <- experiment() # from the landing page
      if (already_checked(daily_checks(), info$experiment_date, check$instrument)) {
        shiny$showNotification(
          paste("A daily check for", check$instrument, "was already saved on this date."),
          type = "error"
        )
      } else {
        row <- new_daily_checks(
          date = info$experiment_date,
          user = info$name,
          instrument = check$instrument,
          positive_lot = check$positive_lot,
          negative_lot = check$negative_lot,
          controls_valid = check$controls_valid,
          rerun_valid = check$rerun_valid
        )
        save_response(row, daily_checks_file)
        daily_checks(rbind(daily_checks(), row))
        shiny$showNotification("Daily check saved.", type = "message")
      }
    })

    responses_table$server("daily_checks_table", responses = daily_checks)

    # ------------------------------------------------------------------------
    # 5. Experiments
    # ------------------------------------------------------------------------
    # All saved experiment answers; starts with what is already in the CSV.
    responses <- shiny$reactiveVal(load_responses(responses_file))

    # Saves one answer, whichever experiment page it came from.
    save_answer <- function(answer) {
      info <- experiment() # from the landing page
      response <- new_response(
        experiment_date = info$experiment_date,
        user = info$name,
        experiment = info$experiment_type,
        product = answer$product,
        lot = answer$lot
      )
      save_response(response, responses_file)
      responses(rbind(responses(), response))
      shiny$showNotification("Thanks! Your answer was saved.", type = "message")
    }

    # One module per experiment; each returns its answer after Submit.
    detection_submission <- detection_capability$server("detection_capability",
      product_id = products
    )
    linearity_submission <- linearity$server("linearity",
      product_id = products
    )

    shiny$observeEvent(detection_submission(), save_answer(detection_submission()))
    shiny$observeEvent(linearity_submission(), save_answer(linearity_submission()))

    responses_table$server("responses", responses = responses)
  })
}
