# Entry point of the app: puts the modules together.
box::use(
  bslib,
  config,
  shiny,
)

box::use(
  app/logic/options[read_options],
  app/logic/products[read_products],
  app/logic/responses[load_responses, new_response, save_response],
  app/logic/daily_checks[load_daily_checks, new_daily_checks],
  app/view/daily_check,
  app/view/landing,
  app/view/insert_product,
  app/view/responses_table,
  app/view/detection_capability,
  app/view/linearity,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$fluidPage(
    # to add to css file 
    theme = bslib$bs_theme(
      version = 5,
      primary = "#2f6f73", # buttons and accents: try your own colour!
      bg = "#f4f6f8", # page background
      fg = "#1f2933", # text colour
      base_font = bslib$font_collection(
        "system-ui", "-apple-system",
        "Segoe UI", "Roboto", "sans-serif"
      )
    ),
    title = "Lab Documentation Prototype",
    shiny$tabsetPanel(
      id = ns("pages"),
      type = "hidden",
      shiny$tabPanel(
        "landing",
        landing$ui(ns("landing"))
      ),
      shiny$tabPanel(
        "daily_check",
        shiny$div(
          class = "questionnaire-page",
          shiny$div(
            class = "page-header",
            shiny$h2("Lab Documentation Prototype"),
            shiny$uiOutput(ns("daily_info"))
          ),
          daily_check$ui(ns("daily_check")),
          responses_table$ui(ns("daily_checks_table"))    # 👈 the table module, reused!
        )
      ),
      shiny$tabPanel(
        "questionnaire",
        shiny$div(
          class = "questionnaire-page",
          shiny$div(
            class = "page-header",
            shiny$h2("Lab Documentation Prototype"),
            shiny$uiOutput(ns("experiment_info")) # 👈 the chips go here
          ),
          shiny$tabsetPanel(
            id = ns("experiment_pages"),
            type = "hidden",
            shiny$tabPanel("Detection Capability", detection_capability$ui(ns("detection_capability"))),
            shiny$tabPanel("Linearity", linearity$ui(ns("linearity")))
          ),
          responses_table$ui(ns("responses"))
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    products_file <- config$get("products_file")
    responses_file <- config$get("responses_file")

    # ui
    people <- read_options(config$get("users_file"), "user")
    experiment_type <- read_options(config$get("assays_file"), "assay")

    experiment <- landing$server("landing",
      people = people,
      experiment_type = experiment_type
    )

    output$experiment_info <- shiny$renderUI({
      info <- experiment()
      shiny$div(
        class = "info-chips",
        shiny$span(class = "chip", shiny$icon("user"), info$name),
        shiny$span(class = "chip", shiny$icon("calendar"), format(info$experiment_date)),
        shiny$span(class = "chip", shiny$icon("flask"), info$experiment_type)
      )
    })

    output$daily_info <- shiny$renderUI({
      info <- experiment()
      shiny$div(
        class = "info-chips",
        shiny$span(class = "chip", shiny$icon("user"), info$name),
        shiny$span(class = "chip", shiny$icon("calendar"), format(info$experiment_date)),
        shiny$span(class = "chip", shiny$icon("sun"), "First run of the day")
      )
    })
    
    daily_checks_file <- config$get("daily_checks_file")
    daily_checks <- shiny$reactiveVal(load_daily_checks(daily_checks_file))
    daily_submission <- daily_check$server("daily_check",
                                           instruments = read_options(config$get("instruments_file"), "instrument"),
                                           controls = read_options(config$get("controls_file"), "control")
                                    )
    
    shiny$observeEvent(daily_submission(), {
      check <- daily_submission()
      info <- experiment()
      if (already_checked(daily_checks(), info$experiment_date, info$name)) {
        shiny$showNotification("You already saved a daily check for this date.", type = "error")
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
    
    
    shiny$observeEvent(experiment(), {
      info <- experiment()
      if (info$first_run) {
        shiny$updateTabsetPanel(session, "pages", selected = "daily_check")
      } else {
        shiny$updateTabsetPanel(session, "experiment_pages", selected = info$experiment_type)
        shiny$updateTabsetPanel(session, "pages", selected = "questionnaire")
      }
    })
    

    # Holds every answer; starts with whatever was saved on previous runs.
    responses <- shiny$reactiveVal(load_responses(responses_file))

    product_id <- read_products(products_file)
    
    save_answer <- function(answer) {
      info <- experiment()
      response <- new_response(
        experiment_date = info$experiment_date,
        user = info$name,
        experiment = info$experiment_type,
        product = answer$product,
        lot = answer$lot)
      
      save_response(response, responses_file)
      responses(rbind(responses(), response))
      shiny$showNotification("Thanks! Your answer was saved.", type = "message")
    }
    
    detection_submission <- detection_capability$server("detection_capability", product_id = product_id)
    linearity_submission <- linearity$server("linearity", product_id = product_id)
    
    shiny$observeEvent(detection_submission(), save_answer(detection_submission()))
    shiny$observeEvent(linearity_submission(), save_answer(linearity_submission()))

    responses_table$server("responses", responses = responses)
  })
}
