# Entry point of the app: puts the pages and modules together.
#
# How the app flows:
#   1. Landing page: the user picks their name, the date and whether this is the
#      first run of the day (if not, they also pick the experiment).
#   2a. First run of the day -> "daily_check" page: instrument, then positive and
#       negative control, step by step. Saved to data/daily_checks.csv
#       (one check per instrument per day).
#   2b. Otherwise -> "questionnaire" page, showing the page of the chosen
#       experiment, saved to data/responses.csv.

box::use(
  bslib,
  config,
  shiny,
)

box::use(
  app/logic/daily_checks[
    already_checked, load_daily_checks, new_daily_checks, read_controls, read_instruments
  ],
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

# Moves keyboard focus to an element (e.g. the heading of a page that just
# appeared), so keyboard and screen-reader users continue in the right place.
# From the server: session$sendCustomMessage("focus-element", "<element id>").
focus_script <- shiny$tags$script(shiny$HTML("
  Shiny.addCustomMessageHandler('focus-element', function(id) {
    setTimeout(function() {
      var el = document.getElementById(id);
      if (el) { el.setAttribute('tabindex', '-1'); el.focus(); }
    }, 60);
  });
"))

# The header of the second pages: title, "Start over" button and the chips.
page_header <- function(heading_id, info_id, start_over_id) {
  shiny$div(
    class = "page-header",
    shiny$div(
      class = "page-header-row",
      shiny$h2(id = heading_id, "Lab Documentation Prototype"),
      shiny$actionButton(start_over_id, "Start over",
        class = "btn-outline-secondary", icon = shiny$icon("rotate-left")
      )
    ),
    shiny$uiOutput(info_id)
  )
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$fluidPage(
    title = "Lab Documentation Prototype",
    # Tells screen readers which language to read in. (fluidPage(lang = ...) has no
    # effect here, because Rhino wraps this UI in its own page.)
    shiny$tags$head(shiny$tags$script("document.documentElement.lang = 'en';")),

    # Colours and font for the whole app (card styles live in app/styles/main.scss)
    theme = bslib$bs_theme(
      version = 5,
      primary = "#004195", # buttons and accents (Sebia blue)
      bg = "#f4f6f8", # page background
      fg = "#1f2933", # text colour
      base_font = bslib$font_collection(
        "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
      )
    ),

    focus_script,

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
          class = "questionnaire-page narrow",
          page_header(
            heading_id = ns("daily_heading"),
            info_id = ns("daily_info"), # chips: user, date, "first run"
            start_over_id = ns("start_over_daily")
          ),
          daily_check$ui(ns("daily_check"))
        )
      ),

      # --- Page 2b: experiments --------------------------------------------
      shiny$tabPanel(
        "questionnaire",
        shiny$div(
          class = "questionnaire-page narrow",
          page_header(
            heading_id = ns("experiment_heading"),
            info_id = ns("experiment_info"), # chips: user, date, experiment
            start_over_id = ns("start_over_experiment")
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
          responses_table$ui(ns("responses"), title = "Your answers for this date")
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
    instruments <- read_instruments(config$get("instruments_file")) # with software/firmware version
    controls <- read_controls(config$get("controls_file")) # columns: control, lot
    products <- read_products(config$get("products_file")) # columns: product, lot

    # ------------------------------------------------------------------------
    # 2. Landing page and routing
    # ------------------------------------------------------------------------
    # The saved daily checks as they are on disk, re-read when the file changes
    # (checked every 5 seconds), so checks saved by colleagues show up too.
    daily_checks_on_disk <- shiny$reactiveFileReader(
      5000, session, daily_checks_file, load_daily_checks
    )
    checked_on <- function(date) {
      checks <- daily_checks_on_disk()
      unique(checks$instrument[checks$date == format(as.Date(date), "%Y-%m-%d")])
    }

    # `experiment()` holds what the user chose on the landing page:
    # name, experiment_date, first_run (TRUE/FALSE) and experiment_type.
    experiment <- landing$server("landing",
      people = people,
      experiment_type = experiment_types,
      instruments = instruments$instrument,
      checked_on = checked_on
    )

    # When the user clicks Start, show the right second page.
    shiny$observeEvent(experiment(), {
      info <- experiment()
      if (info$first_run) {
        shiny$updateTabsetPanel(session, "pages", selected = "daily_check")
        session$sendCustomMessage("focus-element", session$ns("daily_heading"))
      } else {
        shiny$updateTabsetPanel(session, "experiment_pages", selected = info$experiment_type)
        shiny$updateTabsetPanel(session, "pages", selected = "questionnaire")
        session$sendCustomMessage("focus-element", session$ns("experiment_heading"))
      }
    })

    # "Start over" (on both second pages) goes back to the landing page, where
    # the user can change their name, the date or the experiment.
    shiny$observeEvent(list(input$start_over_daily, input$start_over_experiment),
      {
        shiny$updateTabsetPanel(session, "pages", selected = "landing")
        session$sendCustomMessage("focus-element", session$ns("landing-title"))
      },
      ignoreInit = TRUE
    )

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

    # Was this instrument already checked on the date chosen on the landing page?
    is_already_checked <- function(instrument) {
      already_checked(daily_checks(), experiment()$experiment_date, instrument)
    }

    # Saves one finished daily check; returns TRUE if it was saved.
    save_daily_check <- function(check) {
      info <- experiment() # from the landing page
      if (is_already_checked(check$instrument)) {
        shiny$showNotification(
          paste("A daily check for", check$instrument, "was already saved on this date."),
          type = "error"
        )
        return(FALSE)
      }
      row <- new_daily_checks(
        date = info$experiment_date,
        user = info$name,
        instrument = check$instrument,
        software_version = check$software_version,
        firmware_version = check$firmware_version,
        positive = check$positive,
        negative = check$negative
      )
      save_response(row, daily_checks_file)
      daily_checks(rbind(daily_checks(), row))
      TRUE
    }

    # The step-by-step form. It uses the two functions above to check and save.
    daily_check$server("daily_check",
      instruments = instruments,
      controls = controls,
      is_already_checked = is_already_checked,
      save_check = save_daily_check,
      start = experiment
    )

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
      # The confirmation is shown in the form itself (see insert_product.R).
    }

    # One module per experiment; each returns its answer after "Save answer".
    detection_submission <- detection_capability$server("detection_capability",
      product_id = products
    )
    linearity_submission <- linearity$server("linearity",
      product_id = products
    )

    shiny$observeEvent(detection_submission(), save_answer(detection_submission()))
    shiny$observeEvent(linearity_submission(), save_answer(linearity_submission()))

    # The table shows only this user's answers for the chosen date (without the
    # date and user columns, which the chips above already show).
    my_answers <- shiny$reactive({
      info <- experiment()
      all <- responses()
      mine <- all$date == format(as.Date(info$experiment_date), "%Y-%m-%d") & all$user == info$name
      all[mine, c("experiment", "product", "lot"), drop = FALSE]
    })
    responses_table$server("responses",
      responses = my_answers,
      empty_text = "No answers saved for this date yet. They will appear here after you save one."
    )
  })
}
