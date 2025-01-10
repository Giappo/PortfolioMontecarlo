# Setup ====
filename <- "PortfoglioApp2.csv"
pf <- PortfolioMontecarlo::FindLocalDownloadFolder() |> file.path(filename) |> read.csv()
ASSETS <- pf |> dplyr::pull(Symbol)
MIN_WEIGHTS <- pf |> dplyr::pull(MinWeight)
MAX_WEIGHTS <- pf |> dplyr::pull(MaxWeight)
MARKET_REPRESENTATION <- "SPY"
start_date <- 20170101
end_date <- 20241229
RISK_FREE_RATE <- 0
NUM_PORTFOLIOS <- 1e4
NUM_CONSENSUS <- 1e2

out <- PortfolioMontecarlo::RunMontecarlo(
  ASSETS = ASSETS,
  MIN_WEIGHTS = MIN_WEIGHTS,
  MAX_WEIGHTS = MAX_WEIGHTS,
  MARKET_REPRESENTATION = MARKET_REPRESENTATION,
  RISK_FREE_RATE = RISK_FREE_RATE,
  NUM_CONSENSUS = NUM_CONSENSUS,
  NUM_PORTFOLIOS = NUM_PORTFOLIOS,
  start_date = start_date,
  end_date = end_date
)
print(out)

consensusSummary <- PortfolioMontecarlo::CreateSummary(
  portfolio = out$pf_consensus,
  portfolioName = "Consensus",
  data = out$data
)
marketSummary <- PortfolioMontecarlo::CreateSummary(
  portfolio = out$pf_market,
  portfolioName = "Market",
  data = out$data
)
maxSRSummary <- PortfolioMontecarlo::CreateSummary(
  portfolio = out$pf_max_sharpe,
  portfolioName = "MaxSR",
  data = out$data
)
rbind(
  marketSummary,
  maxSRSummary,
  consensusSummary
)

PortfolioMontecarlo::PlotOutput(
  efficient_frontier = out$efficient_frontier,
  portfolios = list(out$pf_consensus),
  market_data = out$market_data,
  RISK_FREE_RATE = RISK_FREE_RATE
)

PortfolioMontecarlo::PlotPie(out$pf_consensus)
PortfolioMontecarlo::PlotPortfolioPerformance(data = out$data, portfolio = out$pf_consensus, market_data = out$market_data)
PortfolioMontecarlo::PlotPortfolioDrawdown(data = out$data, portfolio = out$pf_consensus)

# portfolio <- out$pf_consensus
