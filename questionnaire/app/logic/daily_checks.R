# Plain R code (no Shiny): building and saving questionnaire answers.
box::use(
  utils[read.csv, write.table],
)



#' Read the controls and lots from csv files 
#' @export
read_controls <- function(path){
  read.csv(path, stringsAsFactors = FALSE, strip.white = TRUE)
}

#' Read the lots for each control 
#' @export
lots_for_control <- function(controls, control) {
  sort(unique(controls$lot[controls$control == control]))
}

#' Build a one-row data frame with a single answer.
#' @export
new_daily_checks <- function(date, 
                            user, 
                            instrument, 
                            positive_lot,
                            negative_lot,
                            controls_valid, 
                            rerun_valid) {
  data.frame(
    date = format(as.Date(date), "%Y-%m-%d"),
    user = user,
    instrument = instrument,
    positive_lot = positive_lot,
    negative_lot = negative_lot,
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
      positive_lot = character(0), 
      negative_lot = character(0),
      controls_valid = character(0), 
      rerun_valid = character(0)
    ))
  }
  read.csv(path, 
           stringsAsFactors = FALSE, 
           colClasses = "character")
}

#' Has this user already saved a daily check on this date?
#' @export
already_checked <- function(checks, date, user) {
  any(checks$date == format(as.Date(date), "%Y-%m-%d") & checks$user == user)
}