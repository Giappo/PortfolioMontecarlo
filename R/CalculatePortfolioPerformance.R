#' @export
CalculatePortfolioPerformance <- function(
  portfolio,
  data
) {
  symbols <- colnames(portfolio) |> setdiff(c("Simulation", "Return", "Volatility", "SharpeRatio"))

  portfolio <- portfolio[symbols]
  pctChanges <- PortfolioMontecarlo::PctChange(
    data |> PortfolioMontecarlo::SelectCols(symbols)
  )

  portfolioReturns <- as.matrix(portfolio) %*% t(pctChanges)
  portfolioCum <- cumprod(1 + portfolioReturns)

  df <- data.frame(
    Date = zoo::index(portfolioCum),
    Portfolio = as.numeric(portfolioCum)
  )

  rownames(df) <- zoo::index(pctChanges)

  df$MaxPortfolio <- cummax(df$Portfolio)
  df$Drawdown <- (df$Portfolio - df$MaxPortfolio) / df$MaxPortfolio

  df
}
