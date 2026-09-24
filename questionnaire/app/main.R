# Entry point of the app: puts the modules together.
box::use(
  config,
  shiny,
)
box::use(
  app/logic/products[read_products],
  app/logic/responses[load_responses, new_response, save_response],
  app/view/questionnaire,
  app/view/responses_table,
  app/logic/options[read_options],
  app/view/landing,
)

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)
  shiny$fluidPage(
    shiny$tabsetPanel(
      id = ns("pages"),
      type = "hidden",
      shiny$tabPanel("landing",
                     landing$ui(ns("landing"))),
      shiny$tabPanel(
        "questionnaire",
        shiny$div(
          class = "questionnaire",
          shiny$titlePanel("Product usage questionnaire"),
          questionnaire$ui(ns("form")),
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
    experiment_type <- read_options(config$get("assays_file"),"assay")
    
    experiment <- landing$server("landing", 
                                 people = people, 
                                 experiment_type = experiment_type)
    
    shiny$observeEvent(experiment(), {
      shiny$updateTabsetPanel(session, 
                              "pages", 
                              selected = "questionnaire")
    })
    
    # Holds every answer; starts with whatever was saved on previous runs.
    responses <- shiny$reactiveVal(load_responses(responses_file))

    submission <- questionnaire$server("form", 
                                       product_id = read_products(products_file))

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
