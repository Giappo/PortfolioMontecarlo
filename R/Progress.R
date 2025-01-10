#' @export
WithProgress <- function(
  message = "Running simulations...",
  expr
) {
  if (shiny::isRunning()) {
    shiny::withProgress(message = message, value = 0, expr = expr)
  } else {
    eval(expr)
  }
}

#' @export
IncProgress <- function(index, maxIndex) {
  percent_complete <- (index / maxIndex) * 100

  if (shiny::isRunning()) {
    shiny::incProgress(1 / maxIndex, detail = sprintf("Progress: %.0f%%", percent_complete))
  } else {
    cat(sprintf("Progresso: %.0f%% - Iterazione %d di %d\n", percent_complete, index, maxIndex))
  }
}

#' @export
SetProgress <- function(index, maxIndex) {
  percent_complete <- (index / maxIndex) * 100

  if (shiny::isRunning()) {
    shiny::setProgress(index / maxIndex, detail = sprintf("Progress: %.0f%%", percent_complete))
  } else {
    cat(sprintf("Progresso: %.0f%% - Iterazione %d di %d\n", percent_complete, index, maxIndex))
  }
}
