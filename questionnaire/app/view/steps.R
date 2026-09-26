# The progress bar of a step-by-step flow (daily check, experiment form):
# done steps get a tick, the current one is highlighted.
box::use(
  shiny,
)

#' `steps`: named vector, step id -> label shown. `current`: the current step id
#' (or "done" when all steps are done).
#' @export
progress <- function(steps, current) {
  position <- match(current, c(names(steps), "done"))
  items <- lapply(seq_along(steps), function(i) {
    state <- if (i < position) "done" else if (i == position) "current" else "todo"
    marker <- if (state == "done") shiny$icon("check") else i
    shiny$div(
      class = paste("step", state),
      `aria-current` = if (state == "current") "step",
      shiny$span(class = "step-marker", marker),
      shiny$span(steps[[i]])
    )
  })
  shiny$div(class = "step-progress", items)
}
