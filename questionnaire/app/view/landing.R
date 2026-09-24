box::use(
  shiny,
)

#' @export
ui <- function(id){
  ns <- shiny$NS(id)
  shiny$div(
    class = 'landing-card',
    shiny$h2('Welcome!'),
    shiny$p('Telll us about your experiment.'),
    shiny$selectInput(ns("name"), 
                      "Your name", choices = NULL),
    shiny$dateInput(ns("experiment_date"), 
                    "Experiment date", 
                    value = Sys.Date()),
    shiny$selectInput(ns("experiment_type"), 
                      "Type of experiment", 
                      choices = NULL),
    shiny$actionButton(ns("start"), "Start",
                       class = "btn-primary")
  )
}

#' @export
server <- function(id, people, experiment_type){
  shiny$moduleServer(id, function(input, output, session) {
    shiny$updateSelectInput(session,
                            'name', 
                            choices = c('Choose your name...' = "", 
                                        people))
    shiny$updateSelectInput(session,
                            "experiment_type",
                            choices = c("Choose an experiment..." = "",
                                        experiment_type))
    
    shiny$eventReactive(input$start, {
      shiny$validate(
        shiny$need(input$name != "", 
                   "Please choose your name"),
        shiny$need(input$experiment_type != "", 
                   "Please choose your experiment type.")
      )
      list(
        name = input$name,
        experiment_date = input$experiment_date,
        experiment_type = input$experiment_type
      )
    })
  })
}