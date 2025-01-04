#' @export
WithProgress <- function(expr) {
  if (shiny::isRunning()) {
    shiny::withProgress(message = 'Running simulations...', value = 0, expr = expr)
  } else {
    eval(expr)
  }
}

#' @export
IncProgress <- function(i, NUM_PORTFOLIOS) {
  percent_complete <- (i / NUM_PORTFOLIOS) * 100

  if (shiny::isRunning()) {
    shiny::incProgress(1 / NUM_PORTFOLIOS, detail = sprintf("Progress: %.0f%%", percent_complete))
  } else {
    cat(sprintf("Progresso: %.0f%% - Iterazione %d di %d\n", percent_complete, i, NUM_PORTFOLIOS))
  }
}

#' @export
SetProgress <- function(i, NUM_PORTFOLIOS) {
  percent_complete <- (i / NUM_PORTFOLIOS) * 100

  if (shiny::isRunning()) {
    shiny::setProgress(i / NUM_PORTFOLIOS, detail = sprintf("Progress: %.0f%%", percent_complete))
  } else {
    cat(sprintf("Progresso: %.0f%% - Iterazione %d di %d\n", percent_complete, i, NUM_PORTFOLIOS))
  }
}
