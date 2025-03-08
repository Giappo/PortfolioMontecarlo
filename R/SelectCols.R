#' @export
SelectCols <- function(data, symbols) {
  symbols2 <- symbols |> gsub(pattern = "-", replacement = ".")
  symbols3 <- c(symbols, symbols2) |> unique() |> sort()

  data[, colnames(data) %in% symbols3]
}
