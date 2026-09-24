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
  app/view/landing,
  app/view/questionnaire,
  app/view/responses_table,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$fluidPage(
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
    title = "Product usage questionnaire",
    shiny$tabsetPanel(
      id = ns("pages"),
      type = "hidden",
      shiny$tabPanel(
        "landing",
        landing$ui(ns("landing"))
      ),
      shiny$tabPanel(
        "questionnaire",
        shiny$div(
          class = "questionnaire-page",
          shiny$div(
            class = "page-header",
            shiny$h2("Product usage questionnaire"),
            shiny$uiOutput(ns("experiment_info")) # 👈 the chips go here
          ),
          shiny$fluidRow(
            shiny$column(5, questionnaire$ui(ns("form"))), # left: form
            shiny$column(7, responses_table$ui(ns("responses"))) # right: table
          )
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

    shiny$observeEvent(experiment(), {
      shiny$updateTabsetPanel(session,
        "pages",
        selected = "questionnaire"
      )
    })

    # Holds every answer; starts with whatever was saved on previous runs.
    responses <- shiny$reactiveVal(load_responses(responses_file))

    submission <- questionnaire$server("form",
      product_id = read_products(products_file)
    )

    shiny$observeEvent(submission(), {
      answer <- submission()
      response <- new_response(answer$product, answer$lot, answer$date)
      save_response(response, responses_file)
      responses(rbind(responses(), response))
      shiny$showNotification("Thanks! Your answer was saved.", type = "message")
    })

    responses_table$server("responses", responses = responses)
  })
}
