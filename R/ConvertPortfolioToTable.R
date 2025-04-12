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

  nDigits <- 3
  prec <- 10 ^ nDigits

  x3 <- x2 |>
    dplyr::mutate(
      Weight = round(Weight, nDigits),
      Weight = formatC(Weight, format = "f", digits = nDigits)
    )

  x3
}
