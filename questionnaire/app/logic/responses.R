# Plain R code (no Shiny): building and saving questionnaire answers.
box::use(
  utils[read.csv, write.table],
)

#' Build a one-row data frame with a single answer.
#' @export
new_response <- function(product, lot, date, submitted_at = Sys.time()) {
  data.frame(
    product = product,
    lot = lot,
    date = format(as.Date(date), "%Y-%m-%d"),
    submitted_at = format(submitted_at, "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
}

#' Append one answer to the responses CSV, creating the file if needed.
#' @export
save_response <- function(response, path) {
  file_exists <- file.exists(path)
  write.table(
    response,
    file = path,
    sep = ",",
    row.names = FALSE,
    col.names = !file_exists,
    append = file_exists
  )
  invisible(response)
}

#' Read all saved answers (an empty data frame if there are none yet).
#' @export
load_responses <- function(path) {
  if (!file.exists(path)) {
    return(new_response(character(0), character(0), as.Date(character(0)), as.POSIXct(character(0))))
  }
  read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
}
