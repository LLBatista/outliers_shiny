# Entry point of the app: puts the pages and modules together.
#
# How the app flows:
#   1. Landing page: the user picks their name, the date and whether this is the
#      first run of the day (if not, they also pick the experiment).
#   2a. First run of the day -> "daily_check" page: instrument, then positive and
#       negative control, step by step. Saved to data/daily_checks.csv
#       (one check per instrument per day).
#   2b. Otherwise -> "questionnaire" page, showing the page of the chosen
#       experiment, saved to data/responses.csv. Only instruments that had their
#       daily check on the chosen date can be used.
#
# Corrections to the lists (instrument versions, lots not listed) are never written
# into the base CSVs: they go to data/change_log.csv with who made them and when,
# and the app applies them on top of the base lists.

box::use(
  bslib,
  config,
  shiny,
)

box::use(
  app/logic/changes[
    add_logged_lots, apply_version_changes, log_change, new_change_id, read_changes
  ],
  app/logic/daily_checks[
    already_checked, load_daily_checks, new_daily_checks, read_controls, read_instruments
  ],
  app/logic/fluids[fluid_lot_item, fluids_with_changes, read_fluids],
  app/logic/options[read_options],
  app/logic/products[read_products],
  app/logic/records[append_row],
  app/logic/responses[answers_for_table, load_responses, new_response],
  app/view/answers_list,
  app/view/daily_check,
  app/view/detection_capability,
  app/view/landing,
  app/view/linearity,
)

# ============================================================================
# UI
# ============================================================================

# Moves keyboard focus to an element (e.g. the heading of a page that just
# appeared), so keyboard and screen-reader users continue in the right place.
# From the server: session$sendCustomMessage("focus-element", "<element id>").
focus_script <- shiny$tags$script(shiny$HTML("
  Shiny.addCustomMessageHandler('focus-element', function(id) {
    setTimeout(function() {
      var el = document.getElementById(id);
      if (!el) return;
      if (!el.matches('input, select, textarea, button, a')) el.setAttribute('tabindex', '-1');
      el.focus();
    }, 60);
  });
"))

# The "native-date" input binding for app/view/date_field.R: a plain <input type="date">
# whose value is "YYYY-MM-DD" (or "" when empty).
native_date_script <- shiny$tags$script(shiny$HTML("
  (function() {
    var binding = new Shiny.InputBinding();
    $.extend(binding, {
      find: function(scope) { return $(scope).find('input.native-date'); },
      getValue: function(el) { return el.value; },
      setValue: function(el, value) { el.value = value; },
      subscribe: function(el, callback) {
        $(el).on('change.nativeDate', function() { callback(false); });
      },
      unsubscribe: function(el) { $(el).off('.nativeDate'); },
      receiveMessage: function(el, data) {
        if (data.hasOwnProperty('value')) { el.value = data.value; $(el).trigger('change'); }
      }
    });
    Shiny.inputBindings.register(binding, 'lab.nativeDate');
  })();
"))

# Shows error messages under their fields (see app/view/field_errors.R): fills the
# message, marks the field (red border; aria-invalid and aria-describedby for screen
# readers) and moves focus to the first field with an error.
field_errors_script <- shiny$tags$script(shiny$HTML("
  Shiny.addCustomMessageHandler('field-errors', function(msg) {
    var first = null;
    msg.ids.forEach(function(id, i) {
      var text = msg.messages[i];
      var field = document.getElementById(id);
      var box = document.getElementById(id + '-error');
      if (box) box.textContent = text;
      if (!field) return;
      field.classList.toggle('field-invalid', text !== '');
      if (text) {
        field.setAttribute('aria-invalid', 'true');
        field.setAttribute('aria-describedby', id + '-error');
        if (!first) first = field;
      } else {
        field.removeAttribute('aria-invalid');
        field.removeAttribute('aria-describedby');
      }
    });
    if (first && msg.focus) {
      var details = first.closest('details');
      if (details) details.open = true;
      var target = first.matches('input, select, textarea') ? first : first.querySelector('input');
      if (target) target.focus();
    }
  });
"))

# The header of the second pages: the page title, a button back to the landing page
# (to change the name, date or experiment) and the chips with name and date.
page_header <- function(heading_id, title, info_id, back_id) {
  shiny$div(
    class = "page-header",
    shiny$div(
      class = "page-header-row",
      shiny$h2(id = heading_id, title),
      shiny$actionButton(back_id, "Back to home",
        class = "btn-outline-secondary", icon = shiny$icon("house")
      )
    ),
    shiny$uiOutput(info_id)
  )
}

#' @export
ui <- function(id) {
  ns <- shiny$NS(id)

  shiny$fluidPage(
    title = "Lab Documentation Prototype",
    # Tells screen readers which language to read in. (fluidPage(lang = ...) has no
    # effect here, because Rhino wraps this UI in its own page.)
    shiny$tags$head(shiny$tags$script("document.documentElement.lang = 'en';")),

    # Colours and font for the whole app (card styles live in app/styles/main.scss)
    theme = bslib$bs_theme(
      version = 5,
      primary = "#004195", # buttons and accents (Sebia blue)
      bg = "#f4f6f8", # page background
      fg = "#1f2933", # text colour
      base_font = bslib$font_collection(
        "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
      )
    ),

    focus_script,
    field_errors_script,
    native_date_script,

    # The pages of the app. `type = "hidden"` hides the tab buttons:
    # the server decides which page is shown (see "Routing" below).
    shiny$tabsetPanel(
      id = ns("pages"),
      type = "hidden",

      # --- Page 1: landing -------------------------------------------------
      shiny$tabPanel(
        "landing",
        landing$ui(ns("landing"))
      ),

      # --- Page 2a: first run of the day -----------------------------------
      shiny$tabPanel(
        "daily_check",
        shiny$div(
          class = "questionnaire-page narrow",
          page_header(
            heading_id = ns("daily_heading"),
            title = "First run of the day",
            info_id = ns("daily_info"), # chips: user, date
            back_id = ns("back_home_daily")
          ),
          daily_check$ui(ns("daily_check"))
        )
      ),

      # --- Page 2b: experiments --------------------------------------------
      shiny$tabPanel(
        "questionnaire",
        shiny$div(
          class = "questionnaire-page narrow",
          page_header(
            heading_id = ns("experiment_heading"),
            title = shiny$textOutput(ns("experiment_title"), inline = TRUE), # e.g. "Linearity"
            info_id = ns("experiment_info"), # chips: user, date, type of run
            back_id = ns("back_home_experiment")
          ),
          # One page per experiment. The tab names must match data/assays.csv exactly.
          shiny$tabsetPanel(
            id = ns("experiment_pages"),
            type = "hidden",
            shiny$tabPanel(
              "Detection Capability",
              detection_capability$ui(ns("detection_capability"))
            ),
            shiny$tabPanel(
              "Linearity",
              linearity$ui(ns("linearity"))
            )
          ),
          answers_list$ui(ns("responses"), title = "Your answers for this date")
        )
      )
    )
  )
}

# ============================================================================
# Server
# ============================================================================

#' @export
server <- function(id) {
  shiny$moduleServer(id, function(input, output, session) {
    # ------------------------------------------------------------------------
    # 1. Settings and options (file paths come from config.yml)
    # ------------------------------------------------------------------------
    responses_file <- config$get("responses_file")
    daily_checks_file <- config$get("daily_checks_file")
    changes_file <- config$get("changes_file")

    people <- read_options(config$get("users_file"), "user")
    experiment_types <- read_options(config$get("assays_file"), "assay")
    # The base lists, as they are in the CSV files.
    base_instruments <- read_instruments(config$get("instruments_file")) # with versions
    base_controls <- read_controls(config$get("controls_file")) # columns: control, lot
    base_products <- read_products(config$get("products_file")) # columns: product, lot
    base_fluids <- read_fluids(config$get("fluids_file")) # columns: fluid, lot, expiry_date

    # ------------------------------------------------------------------------
    # 1b. Change log: corrected versions and added lots
    # ------------------------------------------------------------------------
    # Like the daily checks below: re-read when the file changes on disk (changes
    # made by colleagues) and at once when this session logs a change.
    changes_on_disk <- shiny$reactiveFileReader(5000, session, changes_file, read_changes)
    change_saved <- shiny$reactiveVal(0)
    changes <- shiny$reactive({
      change_saved()
      changes_on_disk()
      read_changes(changes_file)
    })

    # The lists the app shows: base lists with the logged changes applied.
    instruments <- shiny$reactive(apply_version_changes(base_instruments, changes()))
    controls <- shiny$reactive(add_logged_lots(base_controls, changes(), "control", "control"))
    products <- shiny$reactive(add_logged_lots(base_products, changes(), "product", "product"))
    fluids <- shiny$reactive(fluids_with_changes(base_fluids, changes()))

    # Logs one change with the user's name (from the landing page) and the time.
    # Rows logged by one action share a change_id, so they can be undone together.
    # `note`: the user's optional note about the change.
    record_change <- function(what, item, field, old_value, new_value,
                              change_id = new_change_id(), note = "") {
      log_change(changes_file, experiment()$name, what, item, field, old_value, new_value,
        change_id = change_id, note = note
      )
      change_saved(change_saved() + 1)
    }

    # New software/firmware versions for an instrument: one log line per changed field.
    change_versions <- function(instrument, software, firmware, note = "") {
      current <- shiny$isolate(instruments())
      current <- current[current$instrument == instrument, ]
      new_values <- c(software_version = software, firmware_version = firmware)
      change_id <- new_change_id()
      for (field in names(new_values)) {
        if (!identical(current[[field]][1], new_values[[field]])) {
          record_change("instrument", instrument, field, current[[field]][1], new_values[[field]],
            change_id = change_id, note = note
          )
        }
      }
    }

    # Undoing a version change logs the opposite change (the log itself is never edited).
    undo_versions <- function(change) {
      change_id <- new_change_id()
      for (i in seq_len(nrow(change))) {
        record_change("instrument", change$item[i], change$field[i],
          change$new_value[i], change$old_value[i],
          change_id = change_id
        )
      }
    }

    # (`expiry` is only used for fluids; see lot_changes.R.)
    add_control_lot <- function(control, lot, expiry = NULL, note = "") {
      record_change("control", control, "lot", "", lot, note = note)
    }
    add_product_lot <- function(product, lot, expiry = NULL, note = "") {
      record_change("product", product, "lot", "", lot, note = note)
    }
    remove_control_lot <- function(control, lot) {
      record_change("control", control, "lot_removed", lot, "")
    }
    remove_product_lot <- function(product, lot) {
      record_change("product", product, "lot_removed", lot, "")
    }
    # A new fluid lot is logged with its expiry date (two rows, one change_id).
    add_fluid_lot <- function(fluid, lot, expiry, note = "") {
      change_id <- new_change_id()
      record_change("fluid", fluid, "lot", "", lot, change_id = change_id, note = note)
      record_change("fluid", fluid_lot_item(fluid, lot), "expiry_date", "",
        format(as.Date(expiry), "%Y-%m-%d"),
        change_id = change_id, note = note
      )
    }
    remove_fluid_lot <- function(fluid, lot) record_change("fluid", fluid, "lot_removed", lot, "")
    # The expiry date on the bottle differs from the list: the list is corrected.
    correct_expiry <- function(fluid, lot, old, new, note = "") {
      record_change("fluid", fluid_lot_item(fluid, lot), "expiry_date", old, new, note = note)
    }
    undo_expiry <- function(correction) {
      record_change("fluid", correction$item, "expiry_date",
        correction$new_value, correction$old_value
      )
    }

    # ------------------------------------------------------------------------
    # 2. Landing page and routing
    # ------------------------------------------------------------------------
    # The saved daily checks as they are on disk, re-read when the file changes
    # (checked every 5 seconds), so checks saved by colleagues show up too.
    daily_checks_on_disk <- shiny$reactiveFileReader(
      5000, session, daily_checks_file, load_daily_checks
    )
    # Changes when this session saves a daily check, so the landing page updates
    # at once instead of after the next 5-second re-read.
    daily_check_saved <- shiny$reactiveVal(0)
    checked_on <- function(date) {
      # Recalculate when this session saves a check, or when the file changes on disk;
      # then read the file itself, so the answer is never an older cached copy.
      daily_check_saved()
      daily_checks_on_disk()
      checks <- load_daily_checks(daily_checks_file)
      unique(checks$instrument[checks$date == format(as.Date(date), "%Y-%m-%d")])
    }

    # `experiment()` holds what the user chose on the landing page:
    # name, experiment_date, first_run (TRUE/FALSE) and experiment_type.
    # Sets "Is this the first run of the day?" when going back to the landing page:
    # cleared after "Back to home", "No" after "Continue to an experiment".
    first_run_answer <- shiny$reactiveVal(list(answer = NULL, n = 0))
    set_first_run <- function(answer) {
      first_run_answer(list(answer = answer, n = first_run_answer()$n + 1))
    }

    experiment <- landing$server("landing",
      people = people,
      experiment_type = experiment_types,
      checked_on = checked_on,
      first_run_answer = first_run_answer
    )

    # When the user clicks Start, show the right second page.
    shiny$observeEvent(experiment(), {
      info <- experiment()
      if (info$first_run) {
        shiny$updateTabsetPanel(session, "pages", selected = "daily_check")
        session$sendCustomMessage("focus-element", session$ns("daily_heading"))
      } else {
        shiny$updateTabsetPanel(session, "experiment_pages", selected = info$experiment_type)
        shiny$updateTabsetPanel(session, "pages", selected = "questionnaire")
        session$sendCustomMessage("focus-element", session$ns("experiment_heading"))
      }
    })

    # "Back to home" (on both second pages) goes back to the landing page, where
    # the user can change their name, the date or the experiment. The first-run
    # question is asked again, so an old answer is not reused by mistake.
    shiny$observeEvent(list(input$back_home_daily, input$back_home_experiment),
      {
        set_first_run(NULL)
        shiny$updateTabsetPanel(session, "pages", selected = "landing")
        session$sendCustomMessage("focus-element", session$ns("landing-title"))
      },
      ignoreInit = TRUE
    )

    # "Continue to an experiment" (after a daily check): back to the landing page with
    # "No" already chosen, and focus on the experiment list.
    continue_to_experiment <- function() {
      set_first_run("no")
      shiny$updateTabsetPanel(session, "pages", selected = "landing")
    }

    # ------------------------------------------------------------------------
    # 3. Page headers: the "chips" under the title
    # ------------------------------------------------------------------------
    info_chips <- function(...) {
      info <- experiment()
      shiny$div(
        class = "info-chips",
        shiny$span(class = "chip", shiny$icon("user"), info$name),
        shiny$span(
          class = "chip", shiny$icon("calendar"),
          format(as.Date(info$experiment_date), "%d %b %Y")
        ),
        ...
      )
    }
    output$daily_info <- shiny$renderUI(info_chips())
    # The experiment page also shows the type of run (regular, retest or pre-test).
    output$experiment_info <- shiny$renderUI(
      info_chips(shiny$span(class = "chip", shiny$icon("repeat"), experiment()$run_type))
    )
    output$experiment_title <- shiny$renderText(experiment()$experiment_type)

    # ------------------------------------------------------------------------
    # 4. First run of the day (daily check)
    # ------------------------------------------------------------------------
    # Was this instrument already checked on the date chosen on the landing page?
    # Reads the saved checks from the file every time, so a check saved by a
    # colleague a moment ago counts too (not only what this session saw at start).
    is_already_checked <- function(instrument) {
      saved <- load_daily_checks(daily_checks_file)
      already_checked(saved, experiment()$experiment_date, instrument)
    }

    # Saves one finished daily check; returns TRUE if it was saved.
    # Checks again right before saving: someone may have saved this instrument
    # while this user was filling in the controls.
    save_daily_check <- function(check) {
      info <- experiment() # from the landing page
      if (is_already_checked(check$instrument)) {
        shiny$showNotification(
          paste("A daily check for", check$instrument, "was already saved on this date."),
          type = "error"
        )
        return(FALSE)
      }
      row <- new_daily_checks(
        date = info$experiment_date,
        user = info$name,
        instrument = check$instrument,
        software_version = check$software_version,
        firmware_version = check$firmware_version,
        positive = check$positive,
        negative = check$negative
      )
      append_row(row, daily_checks_file)
      daily_check_saved(daily_check_saved() + 1)
      TRUE
    }

    # The step-by-step form. It uses the two functions above to check and save.
    # Instruments with a daily check on the chosen date: the only ones that can be
    # used for an experiment (and the ones that can't get a second check).
    checked_instruments <- shiny$reactive({
      shiny$req(experiment())
      checked_on(experiment()$experiment_date)
    })

    daily_check$server("daily_check",
      instruments = instruments,
      controls = controls,
      is_already_checked = is_already_checked,
      checked_today = checked_instruments,
      save_check = save_daily_check,
      start = experiment,
      changes = changes,
      change_versions = change_versions,
      undo_versions = undo_versions,
      add_control_lot = add_control_lot,
      remove_control_lot = remove_control_lot,
      continue_to_experiment = continue_to_experiment
    )

    # ------------------------------------------------------------------------
    # 5. Experiments
    # ------------------------------------------------------------------------
    # All saved experiment answers; starts with what is already in the CSV.
    responses <- shiny$reactiveVal(load_responses(responses_file))

    # Saves one answer, whichever experiment page it came from.
    save_answer <- function(answer) {
      info <- experiment() # from the landing page
      response <- new_response(
        experiment_date = info$experiment_date,
        user = info$name,
        experiment = info$experiment_type,
        run_type = info$run_type,
        answer = answer
      )
      append_row(response, responses_file)
      responses(load_responses(responses_file))
      # The confirmation is shown in the form itself (see experiment_form.R).
    }

    # One module per experiment; each returns its answer after "Save answer".
    # What every experiment page gets (see app/view/experiment_form.R).
    experiment_date <- shiny$reactive(experiment()$experiment_date)
    form_inputs <- list(
      product_id = products,
      checked_instruments = checked_instruments,
      changes = changes,
      add_product_lot = add_product_lot,
      remove_product_lot = remove_product_lot,
      fluids = fluids,
      experiment_date = experiment_date,
      add_fluid_lot = add_fluid_lot,
      remove_fluid_lot = remove_fluid_lot,
      correct_expiry = correct_expiry,
      undo_expiry = undo_expiry,
      start = experiment
    )
    detection_submission <- do.call(
      detection_capability$server, c(list("detection_capability"), form_inputs)
    )
    linearity_submission <- do.call(linearity$server, c(list("linearity"), form_inputs))

    shiny$observeEvent(detection_submission(), save_answer(detection_submission()))
    shiny$observeEvent(linearity_submission(), save_answer(linearity_submission()))

    # The table shows only this user's answers for the chosen date (without the
    # date and user columns, which the chips above already show).
    # (Fluids and sample preparation are summed up in one column each.)
    my_answers <- shiny$reactive({
      info <- experiment()
      all <- responses()
      mine <- all$date == format(as.Date(info$experiment_date), "%Y-%m-%d") & all$user == info$name
      answers_for_table(all[mine, , drop = FALSE])
    })
    answers_list$server("responses",
      answers = my_answers,
      empty_text = "No answers saved for this date yet. They will appear here after you save one."
    )
  })
}
