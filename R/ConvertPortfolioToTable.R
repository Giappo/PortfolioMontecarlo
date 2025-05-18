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

#' @export
ConvertPortfoliosToTable <- function(portfolios) {
  outs <- vector("list", length = length(portfolios))
  names(outs) <- names(portfolios)
  for (i in seq_along(portfolios)) {
    outs[[i]] <- PortfolioMontecarlo::ConvertPortfolioToTable(pf = portfolios[[i]])
  }

  combined <- Reduce(function(x, y) merge(x, y, by = "Symbol", all = TRUE), lapply(names(outs), function(name) {
    df <- outs[[name]]
    colnames(df)[2] <- name
    df
  }))

  combined[is.na(combined)] <- 0

  combined
}
