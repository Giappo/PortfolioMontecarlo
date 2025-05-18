#' @export
RunMontecarlo <- function(
  # ASSETS,
  # MIN_WEIGHTS = rep(0, length(ASSETS)),
  # MAX_WEIGHTS = rep(1, length(ASSETS)),
  pf,
  RISK_FREE_RATE = 0,
  NUM_PORTFOLIOS = 100000,
  start_date,
  end_date,
  MARKET_REPRESENTATION = "SPY",
  # consensusFraction = sqrt(NUM_PORTFOLIOS) ^ -1 # Average across top simulations
  NUM_CONSENSUS = 100
) {
  ASSETS <- pf |> dplyr::pull(Symbol) |> PortfolioMontecarlo::ConvertTickersForQuantmod()
  MIN_WEIGHTS <- pf |> dplyr::pull(MinWeight)
  MAX_WEIGHTS <- pf |> dplyr::pull(MaxWeight)
  WEIGHTS <- pf |> dplyr::pull(Weight)
  WEIGHTS <- setNames(object = WEIGHTS, nm = ASSETS) / sum(WEIGHTS, na.rm = TRUE)

  start_date <- start_date |> PortfolioMontecarlo::FormatDate()
  end_date <- end_date |> PortfolioMontecarlo::FormatDate()

  # Download data ====
  data <- PortfolioMontecarlo::DownloadData(assets = ASSETS, start_date = start_date, end_date = end_date)
  data <- data |>
    zoo::na.locf(na.rm = FALSE) |>
    zoo::na.locf(fromLast = TRUE, na.rm = FALSE)

  market_data <- PortfolioMontecarlo::DownloadData(assets = MARKET_REPRESENTATION, start_date = start_date, end_date = end_date)
  daily_returns <- data |> PortfolioMontecarlo::PctChange()
  cov_matrix <- cov(daily_returns)
  dataMetrics <- data |>
    PortfolioMontecarlo::CalculateAnnualizedDataMetrics(RISK_FREE_RATE = RISK_FREE_RATE)
  annualizedReturns <- dataMetrics$Return

  marketMetrics <- market_data |>
    PortfolioMontecarlo::CalculateAnnualizedDataMetrics(RISK_FREE_RATE = RISK_FREE_RATE)
  market_return <- marketMetrics$Return
  market_volatility <- marketMetrics$Volatility
  market_sharpe_ratio <- marketMetrics$SharpeRatio

  # Execute Montecarlo ====
  colSummary <- c("Simulation", "Return", "Volatility", "SharpeRatio")
  colOutputNames <- c(colSummary, ASSETS)
  output <- matrix(NA, ncol = length(colOutputNames), nrow = NUM_PORTFOLIOS)
  colnames(output) <- colOutputNames

  # t0 <- Sys.time()
  PortfolioMontecarlo::WithProgress(expr = {
    for (i in 1:NUM_PORTFOLIOS) {
      # set.seed(i)

      PortfolioMontecarlo::SetProgress(index = i, maxIndex = NUM_PORTFOLIOS)

      weights <- PortfolioMontecarlo::GenerateRandomWeights(minWeights = MIN_WEIGHTS, maxWeights = MAX_WEIGHTS)
      names(weights) <- ASSETS

      pfSim <- PortfolioMontecarlo::BuildPF(
        weights = weights,
        annualizedReturns = annualizedReturns,
        cov_matrix = cov_matrix,
        riskFreeRate = RISK_FREE_RATE,
        simulationNumber = i
      )

      output[i, "Simulation"] <- i
      output[i, "Return"] <- pfSim$Return
      output[i, "Volatility"] <- pfSim$Volatility
      output[i, "SharpeRatio"] <- pfSim$SharpeRatio
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
  # t1 <- Sys.time()
  # print(t1 - t0)

  efficient_frontier <- PortfolioMontecarlo::ExtractEfficientFrontier(output)

  # Portfolios
  pf_max_sharpe <- output |> dplyr::filter(SharpeRatio == max(SharpeRatio))
  pf_consensus <- output |> dplyr::filter(Simulation == max(Simulation))
  pf_market <- PortfolioMontecarlo::CreateMarketPortfolio(market_data)
  pf_user <- PortfolioMontecarlo::BuildPF(
    weights = WEIGHTS,
    annualizedReturns = annualizedReturns,
    cov_matrix = cov_matrix,
    riskFreeRate = RISK_FREE_RATE,
    simulationNumber = -1L
  )

  portfolios <- list(
    "Benchmark" = pf_market,
    "MaxSR" = pf_max_sharpe,
    "Consensus" = pf_consensus,
    "User" = pf_user
  )

  list(
    data = data |>
      cbind(market_data) |>
      zoo::na.locf(na.rm = FALSE) |>
      zoo::na.locf(fromLast = TRUE, na.rm = FALSE),
    market_data = market_data,
    efficient_frontier = efficient_frontier,
    portfolios = portfolios
  )
}
