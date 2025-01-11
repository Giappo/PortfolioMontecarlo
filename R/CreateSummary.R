#' @export
CreateSummary <- function(
  portfolios,
  data
) {
  outs <- vector("list", length(portfolios))
  for (i in seq_along(portfolios)) {
    portfolio <- portfolios[[i]]
    portfolioName <- names(portfolios)[i]

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

    outs[[i]] <- cbind(statisticsNames, statistics |> signif(3)) |>
      data.frame()
    colnames(outs[[i]]) <- c("Quantity", "Value")
  }

  out <- dplyr::bind_rows(outs)

  out
}
