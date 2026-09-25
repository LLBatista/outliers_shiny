# Plain R code (no Shiny): building and saving questionnaire answers.
box::use(
  utils[read.csv, write.table],
)

#' Build a one-row data frame with a single answer.
#' @export
new_daily_checks <- function(date, 
                            user, 
                            instrument, 
                            controls, 
                            controls_valid, 
                            rerun_valid) {
  data.frame(
    date = format(as.Date(experiment_date), "%Y-%m-%d"),
    user = user,
    instrument = instrument,
    controls = paste(controls, collapse = "; "),
    controls_valid = controls_valid,
    rerun_valid = rerun_valid,
    stringsAsFactors = FALSE
  )
}

#' Read all saved daily checks 
#' @export
load_daily_checks <- function(path){
  if(!file.exists(path)){
    return(data.frame(
      date = character(0), user = character(0), 
      instrument = character(0),
      controls = character(0), 
      controls_valid = character(0), 
      rerun_valid = character(0)
    ))
  }
  read.csv(path, 
           stringsAsFactors = FALSE, 
           colClasses = "character")
}