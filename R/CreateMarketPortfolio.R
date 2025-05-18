#' @export
CreateMarketPortfolio <- function(
  market_data
) {
  marketName <- market_data |> colnames()
  marketMetrics <- market_data |> PortfolioMontecarlo::CalculateAnnualizedDataMetrics()
  out <- data.frame(
    "Simulation" = 0,
    "Return" = marketMetrics$Return |> unname(),
    "Volatility" = marketMetrics$Volatility |> unname(),
    "SharpeRatio" = marketMetrics$SharpeRatio |> unname(),
    "Market" = 1
  )
  out <- out |> dplyr::rename(!!rlang::sym(marketName) := Market)

  out
}
