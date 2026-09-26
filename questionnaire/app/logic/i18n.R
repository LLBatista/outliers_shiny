# Plain R code: the app's texts in English and German.
#
# Every text shown on screen comes from app/i18n/translations.csv (columns key, en,
# de), through `tr("key")`. Placeholders like {lot} are filled in: tr("lot.added",
# lot = "B7"). The texts can be improved in that file without changing code.
#
# Which language: the page address says it (?lang=de). While the page is built, the
# language is set with `with_language()`; in the server it is kept in
# session$userData$lang (see main.R), so `tr()` needs no language argument.
box::use(
  shiny[getDefaultReactiveDomain, parseQueryString],
  utils[read.csv],
)

#' The languages the app speaks: code -> name shown in the switch.
#' @export
languages <- c(en = "English", de = "Deutsch")

translations <- read.csv(box::file("../i18n/translations.csv"),
  stringsAsFactors = FALSE, colClasses = "character", encoding = "UTF-8"
)

# The language used while the page is built (outside a session).
building <- new.env()
building$lang <- "en"

#' The language of the current session (or of the page being built).
#' @export
current_language <- function() {
  session <- getDefaultReactiveDomain()
  lang <- if (!is.null(session)) session$userData$lang
  if (is.null(lang)) building$lang else lang
}

#' Build part of the page in one language.
#' @export
with_language <- function(lang, expr) {
  old <- building$lang
  building$lang <- lang
  on.exit(building$lang <- old)
  force(expr)
}

#' The language asked for in a query string like "?lang=de" (or `default`).
#' @export
language_from_query <- function(query, default = "en") {
  lang <- parseQueryString(if (is.null(query)) "" else query)$lang
  if (!is.null(lang) && lang %in% names(languages)) lang else default
}

#' The text for `key` in the current language, with {placeholders} filled in.
#' Unknown keys are shown as they are, so a missing text is easy to spot.
#' @export
tr <- function(key, ..., lang = current_language()) {
  row <- translations[translations$key == key, ]
  if (nrow(row) == 0) {
    warning("No translation for '", key, "'")
    return(key)
  }
  text <- row[[lang]][1]
  if (is.null(text) || text == "") text <- row$en[1]
  values <- list(...)
  for (name in names(values)) {
    text <- gsub(paste0("{", name, "}"), values[[name]], text, fixed = TRUE)
  }
  text
}

#' A date as people read it: "26 Sep 2026" / "26. Sept. 2026".
#' @export
format_date <- function(date, lang = current_language()) {
  date <- as.Date(date)
  months <- strsplit(tr("date.months", lang = lang), ",", fixed = TRUE)[[1]]
  month <- months[as.integer(format(date, "%m"))]
  tr("date.format", day = as.integer(format(date, "%d")), month = month,
    year = format(date, "%Y"), lang = lang
  )
}

#' A date and time: "26 Sep 2026 at 14:05" / "26. Sept. 2026 um 14:05".
#' `time` is text like "2026-09-26 14:05:12 UTC".
#' @export
format_date_time <- function(time, lang = current_language()) {
  tr("date.at", date = format_date(substr(time, 1, 10), lang = lang),
    time = substr(time, 12, 16), lang = lang
  )
}
