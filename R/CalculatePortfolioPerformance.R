#' @export
CalculatePortfolioPerformanceOverTime <- function(
  portfolio,
  data,
  cumulative = TRUE
) {
  symbols <- colnames(portfolio) |> setdiff(c("Simulation", "Return", "Volatility", "SharpeRatio"))

  portfolio <- portfolio[symbols]
  pctChanges <- data |>
    PortfolioMontecarlo::SelectCols(symbols) |>
    PortfolioMontecarlo::PctChange()

  portfolioReturns <- as.matrix(portfolio) %*% t(pctChanges)
  portfolioCum <- cumprod(1 + portfolioReturns)

  df <- data.frame(
    Date = zoo::index(portfolioCum),
    Portfolio = as.numeric(portfolioCum),
    DailyGain = as.numeric(portfolioReturns)
  )

  rownames(df) <- zoo::index(pctChanges)

  df$MaxPortfolio <- cummax(df$Portfolio)
  df$Drawdown <- (df$Portfolio - df$MaxPortfolio) / df$MaxPortfolio

  df
}
