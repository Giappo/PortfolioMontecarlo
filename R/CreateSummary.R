CreateSummary <- function(portfolio, data) {

  performance <- PortfolioMontecarlo::CalculatePortfolioPerformance(
    portfolio = portfolio,
    data = data
  )

  maxDrawdown <- max(abs(performance$Drawdown))

  statistics <- c(
    portfolio$Return,
    portfolio$Volatility,
    portfolio$SharpeRatio,
    maxDrawdown
  )
  statisticsNames <- c(
    "Annual Return",
    "Annual Volatility",
    "Sharpe Ratio",
    "Max Drawdown"
  )

  cbind(statisticsNames, statistics |> signif(3))
}
