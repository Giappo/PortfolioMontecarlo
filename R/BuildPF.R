#' @export
BuildPF <- function(
  weights,
  annualizedReturns,
  cov_matrix,
  riskFreeRate,
  simulationNumber
) {
  pfMetrics <- PortfolioMontecarlo::CalculatePortfolioMetrics(
    weights = weights,
    annualizedReturns = annualizedReturns,
    cov_matrix = cov_matrix,
    riskFreeRate = riskFreeRate,
    simulationNumber = simulationNumber
  )
  cbind(pfMetrics, as.data.frame(t(weights)))
}
