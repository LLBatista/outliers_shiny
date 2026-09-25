# Plain R code (no Shiny): reading and appending the app's CSV tables.
box::use(
  stats[setNames],
  utils[read.csv, write.table],
)

#' The current time as text, for "saved at" / "changed at" columns.
#' The time zone is included because the server (Docker) may not run in local time.
#' @export
now_text <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
}

#' Read a CSV table as text. Returns an empty table with `columns` if the file
#' does not exist yet, and adds any of `columns` an older file does not have yet
#' (as empty text), so old files keep working after a new column is introduced.
#' @export
read_table <- function(path, columns) {
  data <- if (file.exists(path)) {
    read.csv(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
  } else {
    as.data.frame(setNames(rep(list(character(0)), length(columns)), columns))
  }
  for (column in setdiff(columns, names(data))) data[[column]] <- rep("", nrow(data))
  data[is.na(data)] <- ""
  data[, union(columns, names(data)), drop = FALSE]
}

#' Append one row to a CSV table, creating the file if needed.
#' If the file was written with other columns (an older version of the app), it is
#' first rewritten with the row's columns, so every line keeps the same columns.
#' @export
append_row <- function(row, path) {
  if (!file.exists(path)) {
    write.table(row, path, sep = ",", row.names = FALSE, col.names = TRUE)
    return(invisible(row))
  }
  header <- names(read.csv(path, nrows = 1, check.names = FALSE))
  if (!identical(header, names(row))) {
    upgraded <- read_table(path, names(row))[, names(row), drop = FALSE]
    write.table(upgraded, path, sep = ",", row.names = FALSE, col.names = TRUE)
  }
  write.table(row, path, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
  invisible(row)
}
