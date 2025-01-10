#' @export
CreateSummary <- function(
  portfolio,
  portfolioName = "",
  data
) {

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
    paste0(portfolioName, " ", "Annual Return"),
    paste0(portfolioName, " ", "Annual Volatility"),
    paste0(portfolioName, " ", "Sharpe Ratio"),
    paste0(portfolioName, " ", "Max Drawdown")
  )

  out <- cbind(statisticsNames, statistics |> signif(3))
  colnames(out) <- c("Quantity", "Value")
  out
}
