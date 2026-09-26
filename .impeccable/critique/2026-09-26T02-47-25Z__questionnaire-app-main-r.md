---
target: questionnaire app UI/UX
total_score: 26
max_score: 40
na_heuristics: 
p0_count: 0
p1_count: 3
target_identity: "file:/home/user/outliers_shiny/questionnaire/app/main.R"
target_fingerprint: "sha256:a62a5f5203b257ec6b467da3507be0c0419d178d40c15b0216882bfef5ae86e6"
target_path: /home/user/outliers_shiny/questionnaire/app/main.R
timestamp: 2026-09-26T02-47-25Z
slug: questionnaire-app-main-r
---
Method: single context, degraded (no sub-agents; the user did not ask for them)

| # | Heuristic | Score | Key issue |
|---|---|---|---|
| 1 | Visibility of system status | 3 | Edited but unsaved versions show no "unsaved" state |
| 2 | Match with the real world | 3 | "Which product did you use?" sits above Instrument; "experiment" wording on the daily check |
| 3 | User control and freedom | 2 | Added lots and version changes can't be undone; saved checks can't be corrected yet |
| 4 | Consistency and standards | 3 | Dates shown in 2 formats; generic page title on every page |
| 5 | Error prevention | 2 | Unsaved version edits are silently dropped; a mistyped lot is permanent for everyone |
| 6 | Recognition rather than recall | 3 | Change history is not visible anywhere in the app |
| 7 | Flexibility and efficiency | 2 | No direct path from a finished daily check to an experiment |
| 8 | Aesthetic and minimalist design | 3 | Clean; the step heading repeats the stepper |
| 9 | Error recovery | 3 | Messages are clear but collect at the bottom; fields are not marked |
| 10 | Help and documentation | 2 | Only the no-instrument notice explains anything |
| Total | | 26/40 | Acceptable |

Priority issues
- [P1] Version corrections typed but not saved are dropped silently at Confirm (daily_check.R, instrument step).
- [P1] Added lots and version changes are instant, global and cannot be undone; no confirmation; history not visible.
- [P1] Dead end after the daily check: you reach an experiment only through "Start over", and the landing page keeps "Yes" selected.
- [P2] Validation messages are listed at the bottom, away from their fields; fields are not highlighted or marked aria-invalid.
- [P2] Copy doesn't match the content: the card title vs the Instrument field, "Tell us about your experiment", "Lab Documentation Prototype" as every page's heading.

Detector: 0 findings (R sources are not scannable; the rendered landing and daily HTML came back clean).
