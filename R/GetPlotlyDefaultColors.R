#' @export
GetPlotlyDefaultColors <- function(n) {
  `%||%` <- function(a, b) if (!is.null(a)) a else b

  dummyPlot <- plotly::plot_ly()

  for (i in seq_len(n)) {
    dummyPlot <- plotly::add_trace(
      p = dummyPlot,
      x = i,
      y = i,
      name = paste0("Trace", i),
      type = "scatter",
      mode = "lines+markers"
    )
  }

  # Force plotly to build the plot and populate $x$data
  builtPlot <- plotly::plotly_build(dummyPlot)

  colorsUsed <- builtPlot$x$data |>
    purrr::map_chr(~ .x$line$color %||% .x$marker$color %||% NA_character_)

  return(colorsUsed)
}

