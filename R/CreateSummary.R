#' @export
CreateSummary <- function(
  portfolios,
  data
) {
  outs <- vector("list", length(portfolios))
  names(outs) <- names(portfolios)
  i <- 1
  for (i in seq_along(portfolios)) {
    portfolio <- portfolios[[i]]
    portfolioName <- names(portfolios)[i]

    performance <- PortfolioMontecarlo::CalculatePortfolioPerformanceOverTime(
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
    # statisticsNames <- c(
    #   paste0(portfolioName, " ", "Annual Return"),
    #   paste0(portfolioName, " ", "Annual Volatility"),
    #   paste0(portfolioName, " ", "Sharpe Ratio"),
    #   paste0(portfolioName, " ", "Max Drawdown")
    # )
    statisticsNames <- c(
      "Annual Return",
      "Annual Volatility",
      "Sharpe Ratio",
      "Max Drawdown"
    )

    outs[[i]] <- cbind(statisticsNames, statistics |> signif(3)) |>
      data.frame()

    colnames(outs[[i]]) <- c("Quantity", "Value")
  }

  # out <- dplyr::bind_rows(outs)
  #
  # out

  combined <- Reduce(function(x, y) merge(x, y, by = "Quantity", all = TRUE), lapply(names(outs), function(name) {
    df <- outs[[name]]
    colnames(df)[2] <- name
    df
  }))

  combined[is.na(combined)] <- 0

  combined
}
