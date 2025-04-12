# Setup ====
# filename <- "PortfolioApp8.csv"
filename <- "Sollazzo - 2025-04-12.csv"
pf <- PortfolioMontecarlo::FindLocalDownloadFolder() |> file.path(filename) |> read.csv()
ASSETS <- pf |> dplyr::pull(Symbol)
MIN_WEIGHTS <- pf |> dplyr::pull(MinWeight)
MAX_WEIGHTS <- pf |> dplyr::pull(MaxWeight)
MARKET_REPRESENTATION <- "SPY"
start_date <- 20170101
end_date <- 20241229
RISK_FREE_RATE <- 0
NUM_PORTFOLIOS <- 1e5
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

PortfolioMontecarlo::CreateSummary(
  portfolios = out$portfolios,
  data = out$data
)

out$portfolios$Benchmark |> PortfolioMontecarlo::ConvertPortfolioToTable()
out$portfolios$MaxSR |> PortfolioMontecarlo::ConvertPortfolioToTable()
out$portfolios$Consensus |> PortfolioMontecarlo::ConvertPortfolioToTable()

PortfolioMontecarlo::PlotPortfolioPerformance(
  data = out$data,
  portfolios = out$portfolios,
  logY = TRUE
)

PortfolioMontecarlo::PlotEfficientFrontier(
  efficient_frontier = out$efficient_frontier,
  portfolios = out$portfolios,
  RISK_FREE_RATE = RISK_FREE_RATE
)

PortfolioMontecarlo::PlotPie(out$portfolios$Consensus)
PortfolioMontecarlo::PlotPortfolioDrawdown(data = out$data, portfolio = out$portfolios$Benchmark)
PortfolioMontecarlo::PlotPortfolioDrawdown(data = out$data, portfolio = out$portfolios$MaxSR)
PortfolioMontecarlo::PlotPortfolioDrawdown(data = out$data, portfolio = out$portfolios$Consensus)
