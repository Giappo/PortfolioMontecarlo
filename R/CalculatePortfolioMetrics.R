#' @export
CalculatePortfolioMetrics <- function(
  weights,
  annualizedReturns,
  cov_matrix,
  riskFreeRate = 0,
  simulationNumber = NA_integer_
) {
  return <- sum(weights * annualizedReturns)
  volatility <- sqrt(t(weights) %*% cov_matrix %*% weights) * sqrt(252)

  data.frame(
    Simulation = simulationNumber,
    Return = return,
    Volatility = volatility,
    SharpeRatio = (return - riskFreeRate) / volatility
  )
}
