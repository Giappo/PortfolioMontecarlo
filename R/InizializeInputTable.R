#' @export
InitializeInputTable <- function(rows = 10) {
  data.frame(
    Symbol = character(rows),
    Weight = numeric(rows),
    MinWeight = numeric(rows),
    # MaxWeight = numeric(rows),
    MaxWeight = rep(100, rows),
    stringsAsFactors = FALSE
  )
}
