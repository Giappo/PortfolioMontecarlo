# Setup ====
filename <- "PortfoglioPython.csv"
pf <- FindLocalDownloadFolder() |> file.path(filename) |> read.csv()
ASSETS <- pf |> dplyr::pull(Symbol)
MIN_WEIGHTS <- pf |> dplyr::pull(Min.Weight)
MAX_WEIGHTS <- pf |> dplyr::pull(Max.Weight)
MARKET_REPRESENTATION <- "SPY"
assets <- ASSETS
start_date <- 20170101
end_date <- 20241229
RISK_FREE_RATE <- 0
NUM_PORTFOLIOS <- 5e5

out <- RunMontecarlo(
  ASSETS = ASSETS,
  MIN_WEIGHTS = MIN_WEIGHTS,
  MAX_WEIGHTS = MAX_WEIGHTS,
  MARKET_REPRESENTATION = MARKET_REPRESENTATION,
  RISK_FREE_RATE = RISK_FREE_RATE,
  NUM_PORTFOLIOS = NUM_PORTFOLIOS,
  start_date = start_date,
  end_date = end_date
)

out$efficient_frontier |> print(n = 1000)
out$pf_max_sharpe
out$pf_consensus

PlotOutput(
  efficient_frontier = efficient_frontier,
  # portfolios = list(out$pf_max_sharpe, out$pf_consensus),
  portfolios = list(out$pf_consensus),
  market_data = out$market_data,
  RISK_FREE_RATE = RISK_FREE_RATE
)

PlotPie(out$pf_consensus)
PlotPortfolioPerformance(data = data, portfolio = out$pf_consensus, market_data = out$market_data)
