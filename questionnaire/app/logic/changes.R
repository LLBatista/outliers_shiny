# Plain R code (no Shiny): the change log.
#
# The base lists (instruments.csv, controls.csv, products.csv) are never edited by
# the app. When a user corrects an instrument's versions or adds a lot, the change
# is appended to the change log, with who made it and when. The app then shows the
# base lists with the logged changes applied on top.
box::use(
  app/logic/records[append_row, now_text, read_table],
)

# The columns of data/change_log.csv, in order.
change_columns <- c("changed_at", "user", "what", "item", "field", "old_value", "new_value")

#' Read the change log (an empty table if nothing was changed yet).
#' @export
read_changes <- function(path) {
  read_table(path, change_columns)
}

#' Log one change, e.g. what = "instrument", item = "Analyzer 01",
#' field = "firmware_version", old_value = "v01", new_value = "v02".
#' @export
log_change <- function(path, user, what, item, field, old_value, new_value) {
  row <- data.frame(
    changed_at = now_text(),
    user = user,
    what = what,
    item = item,
    field = field,
    old_value = old_value,
    new_value = new_value,
    stringsAsFactors = FALSE
  )
  append_row(row, path)
}

#' The instruments table with any logged version changes applied (latest wins).
#' @export
apply_version_changes <- function(instruments, changes) {
  is_version <- changes$what == "instrument" &
    changes$field %in% c("software_version", "firmware_version")
  version_changes <- changes[is_version, ]
  for (i in seq_len(nrow(version_changes))) {
    change <- version_changes[i, ]
    rows <- instruments$instrument == change$item
    instruments[rows, change$field] <- change$new_value
  }
  instruments
}

#' A lots table (`key` column + lot) with any logged lot additions for `what`
#' ("control" or "product") added.
#' @export
add_logged_lots <- function(table, changes, what, key) {
  added <- changes[changes$what == what & changes$field == "lot", ]
  if (nrow(added) == 0) {
    return(table)
  }
  new_rows <- data.frame(added$item, added$new_value, stringsAsFactors = FALSE)
  names(new_rows) <- c(key, "lot")
  combined <- rbind(table[, c(key, "lot")], new_rows)
  combined[!duplicated(combined), ]
}
