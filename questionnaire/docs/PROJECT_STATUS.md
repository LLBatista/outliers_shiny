# Lab Documentation Prototype: project status

Last updated: 2026-09-26 · Branch: `claude/shiny-questionnaire-app-q0cycc`

**In one sentence:** a Shiny + Rhino app for documenting lab work on a tablet at
the bench. It covers the daily instrument check (first run of the day) and
experiment answers (product and lot used). It will run in Docker on a local server.

---

## 1. What the app does today

### Page 1: Landing
- The user picks **their name**, the **date** (no future dates) and whether this is the
  **first run of the day**.
- If it is not the first run, they also pick the **type of experiment**; then the **type of
  run** appears, with "Regular" already chosen (or retest / pre-test). It is saved with
  every answer.
- A status box lists **the instruments already checked** on that date.

### Page 2a: First run of the day (daily check)
This is a step-by-step form with a progress indicator.

1. **Instrument.** The user picks the instrument, and the app shows its software and
   firmware versions.
   - If the versions are wrong, **"Versions not right? Correct them here"** lets the
     user type the right ones. They are saved and logged when the user presses Confirm.
   - "Confirm" is blocked if this instrument was already checked on this date.
2. **Positive control.** The user picks the lot, says whether the control was valid,
   and says whether the rerun was valid (asked only if the control was not valid).
- At the end: "Continue to an experiment" (goes to the landing page with "No" chosen)
  or "Check another instrument".
3. **Negative control.** Same questions as the positive control.
- **"Lot not listed? Add it"** is available for both controls, and the new lot is logged.
- A summary appears at the end, with "Check another instrument".
- Only **one check per instrument per day**. The app reads the file fresh right before
  saving, so this still holds when two people use the app at the same time.
- Saved to `data/daily_checks.csv`.

### Page 2b: Experiments (Detection Capability, Linearity)
Every experiment starts with the same steps (`experiment_form.R`), one at a time like the
daily check, with a progress bar and Back / Next: **Instrument & product → Fluids →
Samples (+ optional notes) → Save answer**, then a summary and "Add another product".
- **Instrument:** only instruments **with a daily check on the chosen date** are listed.
  - If there are none, a notice explains what to do.
  - Saving is also refused on the server.
- **Product and lot:** the lot list shows only the chosen product's lots.
  **"Lot not listed? Add it"** is available here too.
- **Instrument fluids:** system fluid and system buffer, each with its lot.
  - The expiry date from `data/fluids.csv` is shown, and the user answers "Does the
    expiry date on the bottle match?" Yes / No.
  - On "No", the user enters the date printed on the bottle. When the answer is saved,
    that date is used and the fluids list is corrected for everyone (logged, shown as
    "Corrected by …", and it can be undone).
  - **An expired lot can't be used**: saving is blocked until another lot is chosen or
    a later bottle date is entered.
  - A missing lot can be added with its expiry date (confirmed and logged).
  - "Another lot used in this run? (rare)" adds a second system fluid or system buffer.
- **Sample preparation:** vortexed? thawed? and if thawed, for how many minutes.
- After saving, a confirmation appears and the product and lot are cleared. The
  instrument, fluids and sample preparation are kept for the next answer (a note next
  to "Save answer" says so).
- "Your answers for this date" lists each answer in short, newest first, including
  fluids and sample preparation.
- **"Your answers for this date"** shows the user's own answers.
- Saved to `data/responses.csv`.

### Change log (who changed what, when)
- The base lists (`instruments.csv`, `controls.csv`, `products.csv`) are **never edited
  by the app**.
- Corrected versions and added lots are appended to `data/change_log.csv` with these
  columns: `changed_at, user, what, item, field, old_value, new_value`.
- The app shows the base lists with the logged changes applied. For versions, the
  latest change wins.
- Every change asks for **confirmation** first ("Add lot B7 to Positive Control for
  everyone?").
- The app shows **who made a change and when**, under the versions ("Last changed by …")
  and under a lot that was added in the app.
- Changes can be **undone**: "Undo this change" for versions, "Remove this lot" for added
  lots. The undo is logged as a new change; nothing is deleted from the log.
- There is no approval by a second person.

### Languages (English / German)
- **EN | DE** at the top right of the start page. The language is part of the page
  address (`?lang=de`, handy for a tablet bookmark) and the browser remembers the choice.
- Without a choice, `default_language` in `config.yml` decides (now `"en"`).
- All texts are in `app/i18n/translations.csv` (`key, en, de`): wording can be changed
  there without touching code. Placeholders like `{lot}` must stay.
- Saved data is the same in both languages (e.g. run type `Retest`, answers `yes`/`no`);
  only what is shown changes. Names from the lists (instruments, products, experiment
  types) are shown as they are in the CSV files.

### Look and usability
- The colour is Sebia blue (`#004195`).
- It is built for a **tablet at the bench**: touch targets are at least 48 px, and it
  works in portrait and landscape.
- Accessibility work:
  - contrast checked (text ≥ 4.5:1, borders ≥ 3:1)
  - page language set
  - keyboard focus moves to each new page
  - "saved" messages are announced to screen readers
- It uses native dropdowns (easier on touch screens).
- There are no internet assets: icons (Font Awesome, open licence) and CSS are served
  locally.

---

## 2. How the code is organised

```
questionnaire/
├── app.R, rhino.yml         # Rhino entry point and settings (don't edit app.R)
├── config.yml               # all file paths
├── data/                    # base lists (tracked in git) + saved records (not tracked)
├── app/
│   ├── main.R               # puts pages together; routing; saving; change log wiring
│   ├── logic/               # plain R, no Shiny (easy to test)
│   │   ├── records.R        # read/append CSV tables; upgrades old files with new columns
│   │   ├── changes.R        # change log: read, log, apply to lists
│   │   ├── daily_checks.R   # instruments, controls, daily check rows, once-per-day rule
│   │   ├── products.R       # products and their lots
│   │   ├── responses.R      # experiment answer rows
│   │   └── options.R        # simple one-column lists (users, assays)
│   ├── view/                # Shiny modules
│   │   ├── landing.R        # page 1
│   │   ├── daily_check.R    # page 2a, the step-by-step check
│   │   ├── control_check.R  # one control step (used twice)
│   │   ├── add_lot.R        # "Lot not listed? Add it"
│   │   ├── insert_product.R # instrument + product + lot form
│   │   ├── detection_capability.R, linearity.R  # experiment pages
│   │   └── responses_table.R
│   ├── styles/main.scss     # styles -> run rhino::build_sass() after changes
│   └── static/              # compiled CSS, favicon
└── tests/testthat/          # unit tests
```

### Data files

| File | What it holds | In git? |
|---|---|---|
| `users.csv`, `assays.csv` | names, experiment types | yes |
| `instruments.csv` | instrument, software_version, firmware_version | yes |
| `controls.csv`, `products.csv` | name + lot | yes |
| `fluids.csv` | fluid, lot, expiry_date (example values; replace with the real list) | yes |
| `daily_checks.csv` | one row per instrument per day, with who and when | no |
| `responses.csv` | experiment answers: type of run, instrument, product, fluid lots with expiry dates, sample preparation, and when | no |
| `change_log.csv` | corrections and added lots, with who and when | no |
| `daily_checks_old_format.csv` | old file from before the step-by-step check | yes |

Files from older versions keep working: missing columns are added (empty) the next
time a row is saved.

---

## 3. History (main milestones)

1. A simple questionnaire: pick a product from a CSV, and a date.
2. Product → lot selection. A landing page (user, date, experiment type).
3. A separate page per experiment (Detection Capability, Linearity).
4. "First run of the day" daily check. Control lots. Once per day, **per instrument**.
5. `main.R` organised into commented sections.
6. We tried shiny.fluent, then **reverted** to Shiny + bslib. Icons made fully offline.
7. The daily check became a step-by-step flow, showing instrument versions.
8. Polish for tablet use. Sebia blue.
9. UX review (two rounds): date rules, contrast, dropdowns, announcements, daily
   status, own answers, focus.
10. Fix: no duplicate daily checks when two people save at once.
11. Experiments only on checked instruments. Logged version corrections and lot
    additions.
12. Fixes from a UX critique:
    - corrected versions are saved by Confirm, so they can't be lost;
    - changes are confirmed, show who made them, and can be undone;
    - "Continue to an experiment" after the daily check; "Change details" instead of
      "Start over";
    - error messages under their fields;
    - page titles name the page.
13. Experiment steps like the daily check; "Back to home"; version corrections on an
    instrument already checked (with an alert); version history one line per field;
    length limits (versions 5, lots 12 characters); optional notes on controls, answers
    and every change to the lists.
14. English and German version, with a language switch on the start page.

---

## 4. Still open

### Next up
- [ ] **Void + re-enter corrections** (already decided). A wrong daily check or answer is
  marked *voided* with who, when and a reason, and the user re-enters it. Voided rows
  should not count for the once-per-day rule, the daily status or the tables.
- [x] **Tests updated.** All 65 tests pass (`rhino::test_r()`): logic (change log,
  fluids, responses, daily checks, products, options, CSV records) and two form sections
  (sample preparation, instrument/product/lot).
- [ ] **Update `README.md`.** It still describes the first simple version of the app.

### Growth and structure
- [ ] **Experiment registry.** Adding an experiment currently means editing `main.R` in
  several places and matching names in `assays.csv`. Replace this with one list
  (name → module).
- [ ] **Database.** Move from CSV files to a database; SQLite and Postgres are both
  still options. `records.R` and `changes.R` are the places to swap. Postgres is better
  if several people write at once or other systems need to read the data.

### Running it in the lab (Docker on a local server)
- [ ] `Dockerfile` (R + packages pinned with `renv.lock`).
- [ ] Set the **time zone** (`TZ`), so times in the logs are local and not UTC.
- [ ] Put `data/` on a **volume**, so records survive container updates.
- [ ] `docker-compose.yml` and the start/update steps for the server.
- [ ] Backups of the data folder or database.

### Questions to decide
- [ ] Should changes (new lots, new versions) need **approval** by a second person, or
  is logging enough? This depends on the lab's quality rules.
- [ ] **Who is the user?** Today it is picked from a list. A login may be needed for
  audit trails (e.g. GxP / 21 CFR Part 11 style requirements).
- [ ] Should the base lists (users, instruments, products) be managed inside the app
  by an admin?

---

## 5. Everyday commands

```r
shiny::runApp()        # run the app (from the questionnaire folder)
rhino::test_r()        # unit tests
rhino::lint_r()        # code style (100-char lines, sorted box::use)
rhino::build_sass()    # after editing app/styles/main.scss
```
