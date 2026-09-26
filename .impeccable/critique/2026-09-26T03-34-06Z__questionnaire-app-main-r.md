---
target: questionnaire app UI/UX
total_score: 27
max_score: 40
na_heuristics: 
p0_count: 0
p1_count: 3
target_identity: "file:/home/user/outliers_shiny/questionnaire/app/main.R"
target_fingerprint: "sha256:01dcd5e1fde4752f64856dc42a60f4d7a90888e0b8d04778408ef6e7fd6db027"
target_path: /home/user/outliers_shiny/questionnaire/app/main.R
timestamp: 2026-09-26T03-34-06Z
slug: questionnaire-app-main-r
---
Method: single context, degraded (no sub-agents; the user did not ask for them)

| # | Heuristic | Score | Key issue |
|---|---|---|---|
| 1 | Visibility of system status | 3 | "Answer saved" at the bottom; kept values (fluids, samples) not signalled |
| 2 | Match with the real world | 3 | System buffer listed before system fluid; "matches the bottle" tick |
| 3 | User control and freedom | 3 | Undo for lots/versions; saved answers can't be corrected yet |
| 4 | Consistency and standards | 3 | Tick box for the bottle vs Yes/No everywhere else |
| 5 | Error prevention | 2 | Expired fluid saved with no friction; habit-tick; silent carry-over |
| 6 | Recognition rather than recall | 3 | Saved table hides fluids and sample preparation |
| 7 | Flexibility and efficiency | 3 | Instrument/fluids kept; Continue path exists |
| 8 | Aesthetic and minimalist design | 3 | Fluids card has 4 link actions competing |
| 9 | Error recovery | 2 | No path when the bottle date doesn't match |
| 10 | Help and documentation | 2 | Little guidance beyond the notices |
| Total | | 27/40 | Acceptable |

Priority issues
- [P1] No path when the bottle's expiry date differs from the list; the tick box only allows "yes".
- [P1] An expired fluid lot saves like any other: red box, then blue tick, then saved; nothing recorded or confirmed.
- [P1] Fluids and sample preparation carry over silently to the next answer.
- [P2] Fluids card: system buffer before system fluid; 4 link actions ("Lot not listed" x2, "Add a second" x2) crowd the card.
- [P2] Saved-answer table shows only 5 of 18 saved fields; no way to check fluids/samples after saving.
- [P3] Headings skip a level (h2 -> h4 on the daily check); card titles only 1.2x body size.

Detector: 3 findings (skipped-heading x2, flat-type-hierarchy x1); all valid.
