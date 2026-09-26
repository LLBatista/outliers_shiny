# Plain R code (no Shiny): the change log.
#
# The base lists (instruments.csv, controls.csv, products.csv) are never edited by
# the app. When a user corrects an instrument's versions, adds a lot or removes a lot
# that was added in the app, the change is appended to the change log, with who made
# it and when. The app then shows the base lists with the logged changes applied.
#
# Nothing is ever deleted from the log: undoing a change is logged as a new change.
box::use(
  app/logic/records[append_row, now_text, read_table],
)

# The columns of data/change_log.csv, in order. Rows logged by one action (e.g. new
# software AND firmware versions) share a change_id, so they can be undone together.
change_columns <- c(
  "changed_at", "user", "what", "item", "field", "old_value", "new_value", "change_id"
)

version_fields <- c("software_version", "firmware_version")

#' A new id for one action in the change log.
#' @export
new_change_id <- function() {
  paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", sample.int(1e6, 1))
}

#' Read the change log (an empty table if nothing was changed yet).
#' @export
read_changes <- function(path) {
  changes <- read_table(path, change_columns)
  # Rows from before change_id existed: group them by time instead.
  no_id <- changes$change_id == ""
  changes$change_id[no_id] <- changes$changed_at[no_id]
  changes
}

#' Log one change, e.g. what = "instrument", item = "Analyzer 01",
#' field = "firmware_version", old_value = "v01", new_value = "v02".
#' @export
log_change <- function(path, user, what, item, field, old_value, new_value,
                       change_id = new_change_id()) {
  row <- data.frame(
    changed_at = now_text(),
    user = user,
    what = what,
    item = item,
    field = field,
    old_value = old_value,
    new_value = new_value,
    change_id = change_id,
    stringsAsFactors = FALSE
  )
  append_row(row, path)
}

#' The instruments table with any logged version changes applied (latest wins).
#' @export
apply_version_changes <- function(instruments, changes) {
  is_version <- changes$what == "instrument" & changes$field %in% version_fields
  version_changes <- changes[is_version, ]
  for (i in seq_len(nrow(version_changes))) {
    change <- version_changes[i, ]
    rows <- instruments$instrument == change$item
    instruments[rows, change$field] <- change$new_value
  }
  instruments
}

#' The latest version change of an instrument (one row per changed field), or NULL.
#' @export
last_version_change <- function(changes, instrument) {
  is_mine <- changes$what == "instrument" & changes$item == instrument
  mine <- changes[is_mine & changes$field %in% version_fields, ]
  if (nrow(mine) == 0) {
    return(NULL)
  }
  mine[mine$change_id == mine$change_id[nrow(mine)], ]
}

#' A lots table (`key` column + lot) with the lots added and removed in the app
#' applied, in the order they happened. `what` is "control" or "product".
#' @export
add_logged_lots <- function(table, changes, what, key) {
  lot_changes <- changes[changes$what == what & changes$field %in% c("lot", "lot_removed"), ]
  table <- table[, c(key, "lot")]
  for (i in seq_len(nrow(lot_changes))) {
    change <- lot_changes[i, ]
    if (change$field == "lot") {
      new_row <- data.frame(change$item, change$new_value, stringsAsFactors = FALSE)
      names(new_row) <- c(key, "lot")
      table <- rbind(table, new_row)
    } else {
      table <- table[!(table[[key]] == change$item & table$lot == change$old_value), ]
    }
  }
  table[!duplicated(table), ]
}

#' If `lot` of `item` is currently in the list because it was added in the app,
#' the row of the change log that added it; otherwise NULL (a lot from the base
#' list, or one that was removed again).
#' @export
lot_added_in_app <- function(changes, what, item, lot) {
  is_item <- changes$what == what & changes$item == item
  added <- changes$field == "lot" & changes$new_value == lot
  removed <- changes$field == "lot_removed" & changes$old_value == lot
  mine <- changes[is_item & (added | removed), ]
  if (nrow(mine) == 0 || mine$field[nrow(mine)] != "lot") {
    return(NULL)
  }
  mine[nrow(mine), ]
}

#' "Leticia Batista on 26 Sep 2026 at 14:05": who made a change and when.
#' @export
changed_by_text <- function(change) {
  when <- as.POSIXct(substr(change$changed_at[1], 1, 19), format = "%Y-%m-%d %H:%M:%S")
  paste(change$user[1], "on", format(when, "%d %b %Y at %H:%M"))
}
