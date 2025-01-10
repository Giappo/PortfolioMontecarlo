#' @export
RunMontecarlo <- function(
  ASSETS,
  MIN_WEIGHTS = rep(0, length(ASSETS)),
  MAX_WEIGHTS = rep(1, length(ASSETS)),
  RISK_FREE_RATE = 0,
  NUM_PORTFOLIOS = 100000,
  start_date,
  end_date,
  MARKET_REPRESENTATION = "SPY",
  # consensusFraction = sqrt(NUM_PORTFOLIOS) ^ -1 # Average across top simulations
  NUM_CONSENSUS = 100
) {
  start_date <- start_date |> PortfolioMontecarlo::FormatDate()
  end_date <- end_date |> PortfolioMontecarlo::FormatDate()

  # Download data ====
  data <- PortfolioMontecarlo::DownloadData(assets = ASSETS, start_date = start_date, end_date = end_date)
  market_data <- PortfolioMontecarlo::DownloadData(assets = MARKET_REPRESENTATION, start_date = start_date, end_date = end_date)
  daily_returns <- data |> PortfolioMontecarlo::PctChange()
  cov_matrix <- cov(daily_returns)
  dataMetrics <- data |> PortfolioMontecarlo::CalculateAnnualizedMetrics(RISK_FREE_RATE = RISK_FREE_RATE)
  annualizedReturns <- dataMetrics$Return

  marketMetrics <- market_data |> PortfolioMontecarlo::CalculateAnnualizedMetrics(RISK_FREE_RATE = RISK_FREE_RATE)
  market_return <- marketMetrics$Return
  market_volatility <- marketMetrics$Volatility
  market_sharpe_ratio <- marketMetrics$SharpeRatio

  # Execute Montecarlo ====
  colSummary <- c("Simulation", "Return", "Volatility", "SharpeRatio")
  colOutputNames <- c(colSummary, ASSETS)
  output <- matrix(NA, ncol = length(colOutputNames), nrow = NUM_PORTFOLIOS)
  colnames(output) <- colOutputNames

  PortfolioMontecarlo::WithProgress(expr = {
    for (i in 1:NUM_PORTFOLIOS) {
      # set.seed(i)

      PortfolioMontecarlo::SetProgress(index = i, maxIndex = NUM_PORTFOLIOS)

      weights <- PortfolioMontecarlo::GenerateRandomWeights(minWeights = MIN_WEIGHTS, maxWeights = MAX_WEIGHTS)
      # port_return <- sum(weights * colMeans(daily_returns)) * 252 # Rendimento annualizzato del portafoglio
      port_return <- sum(weights * annualizedReturns)
      port_volatility <- sqrt(t(weights) %*% cov_matrix %*% weights) * sqrt(252) # Volatilità annualizzata
      port_sharpe_ratio <- (port_return - RISK_FREE_RATE) / port_volatility

      output[i, "Simulation"] <- i
      output[i, "Return"] <- port_return
      output[i, "Volatility"] <- port_volatility
      output[i, "SharpeRatio"] <- port_sharpe_ratio
      output[i, ASSETS] <- weights
    }
  })

  output <- output |>
    as.data.frame() |>
    PortfolioMontecarlo::AddConsensusSimulation(
      ASSETS = ASSETS,
      data = data,
      NUM_CONSENSUS = NUM_CONSENSUS
      # fraction = consensusFraction
    )

  pf_max_sharpe <- output |> dplyr::filter(SharpeRatio == max(SharpeRatio))
  pf_consensus <- output |> dplyr::filter(Simulation == max(Simulation))
  pf_market <- PortfolioMontecarlo::CreateMarketPortfolio(market_data)
  efficient_frontier <- PortfolioMontecarlo::ExtractEfficientFrontier(output)

  list(
    data = data |> cbind(market_data),
    market_data = market_data,
    pf_market = pf_market,
    pf_max_sharpe = pf_max_sharpe,
    pf_consensus = pf_consensus,
    efficient_frontier = efficient_frontier
  )
}
