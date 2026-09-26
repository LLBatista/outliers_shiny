# Plain R code (no Shiny): the instrument fluids (system buffer and system fluid),
# their lots and expiry dates.
box::use(
  utils[read.csv],
)

box::use(
  app/logic/changes[add_logged_lots],
)

#' The fluids asked for in every experiment: the name used for the columns of
#' data/responses.csv -> the name in data/fluids.csv.
#' @export
fluid_names <- c(system_fluid = "System fluid", system_buffer = "System buffer")

#' Read the fluids, their lots and expiry dates (YYYY-MM-DD) from a CSV file.
#' @export
read_fluids <- function(path) {
  fluids <- read.csv(path, stringsAsFactors = FALSE, strip.white = TRUE, colClasses = "character")
  if (!all(c("fluid", "lot", "expiry_date") %in% names(fluids))) {
    stop("The fluids file must have the columns 'fluid', 'lot' and 'expiry_date'.")
  }
  fluids
}

#' Lots available for one fluid.
#' @export
lots_for_fluid <- function(fluids, fluid) {
  sort(unique(fluids$lot[fluids$fluid == fluid]))
}

#' The expiry date of one lot ("" if it is not known).
#' @export
expiry_of <- function(fluids, fluid, lot) {
  expiry <- fluids$expiry_date[fluids$fluid == fluid & fluids$lot == lot]
  if (length(expiry) == 0) "" else expiry[length(expiry)]
}

#' How a fluid lot is named in the change log when its expiry date is logged,
#' e.g. "System buffer lot SB2603".
#' @export
fluid_lot_item <- function(fluid, lot) {
  paste(fluid, "lot", lot)
}

#' The fluids table with the lots added and removed in the app, and their logged
#' expiry dates, applied.
#' @export
fluids_with_changes <- function(fluids, changes) {
  fluids <- add_logged_lots(fluids, changes, "fluid", "fluid")
  expiry <- changes[changes$what == "fluid" & changes$field == "expiry_date", ]
  for (i in seq_len(nrow(expiry))) {
    rows <- fluid_lot_item(fluids$fluid, fluids$lot) == expiry$item[i]
    fluids$expiry_date[rows] <- expiry$new_value[i]
  }
  fluids
}

#' The latest correction of a lot's expiry date (a row of the change log), or NULL.
#' Setting the date of a lot added in the app (old value "") is not a correction.
#' @export
last_expiry_correction <- function(changes, fluid, lot) {
  is_expiry <- changes$what == "fluid" & changes$field == "expiry_date"
  rows <- changes[is_expiry & changes$item == fluid_lot_item(fluid, lot), ]
  if (nrow(rows) == 0 || rows$old_value[nrow(rows)] == "") {
    return(NULL)
  }
  rows[nrow(rows), ]
}
