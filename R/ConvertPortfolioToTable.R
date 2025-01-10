#' @export
ConvertPortfolioToTable <- function(pf) {
  x <- pf |>
    dplyr::select(-Simulation, -Return, -Volatility, -SharpeRatio) |>
    t() |>
    data.frame()
  x2 <- x |>
    dplyr::mutate(Symbol = rownames(x)) |>
    dplyr::relocate(Symbol)

  rownames(x2) <- NULL
  colnames(x2) <- c("Symbol", "Weight")
  x3 <- x2 |>
    dplyr::mutate(Weight = paste0((Weight |> signif(2)) * 100, "%"))

  x3
}
