url <- "https://www.dataroma.com/m/grid.php?s=p"
page <- rvest::read_html(url)

# Extract all tables first
tables <- page |>
  rvest::html_nodes("table")

# Extract the first table and convert it to a data frame
tableData <- tables[[1]] |>
  rvest::html_table(fill = TRUE)

# Define the function to extract information
extract_info <- function(cell) {
  ticker <- stringr::str_extract(cell, "^[A-Za-z]+")
  companyName <- stringr::str_extract(cell, "(?<=\\r\\n)[A-Za-z ]+(?=\\()")
  sector <- stringr::str_extract(cell, "(?<=\\()[A-Za-z ]+(?=\\))")
  percentage <- stringr::str_extract(cell, "\\d+\\.\\d+%")  # Modified regex for percentage
  price <- stringr::str_extract(cell, "(?<=Price: \\$)\\d+\\.\\d+")

  # Return as a named tibble
  tibble::tibble(ticker, companyName, sector, percentage, price)
}

df <- apply(tableData, 1, function(row) {
  dplyr::bind_rows(lapply(row, extract_info))
}) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    Weight = stringr::str_extract(percentage, "\\d+\\.\\d+") |>
      as.numeric() / 100
  ) |>
  dplyr::rename(Symbol = ticker) |>
  dplyr::mutate(Symbol = dplyr::if_else(Symbol == "BRK", "BRK-B", Symbol)) |>
  dplyr::select(Symbol, Weight) |>
  dplyr::arrange(dplyr::desc(Weight)) |>
  dplyr::mutate(rank = dplyr::row_number(dplyr::desc(Weight))) |>
  dplyr::filter(rank <= 101) |>
  dplyr::select(-rank) |>
  dplyr::mutate(Weight = Weight / sum(Weight))

start_date <- 20170101
end_date <- 20241229
data <- PortfolioMontecarlo::DownloadData(assets = df$Symbol, start_date = start_date, end_date = end_date)
MARKET_REPRESENTATION <- "SPY"
market_data <- PortfolioMontecarlo::DownloadData(assets = MARKET_REPRESENTATION, start_date = start_date, end_date = end_date)
data = data |>
  cbind(market_data) |>
  zoo::na.locf(na.rm = FALSE) |>
  zoo::na.locf(fromLast = TRUE, na.rm = FALSE)

# Identify columns with at least one NA value
columns_with_na <- data |>
  as.data.frame() |>
  dplyr::select(where(~ any(is.na(.)))) |>
  colnames()

# Create a new dataframe with ticker symbols as column names
portfolio_df <- df |>
  dplyr::filter(!Symbol %in% columns_with_na) |>
  dplyr::mutate(Weight = Weight / sum(Weight)) |>
  dplyr::mutate(
    Simulation = -1,
    Return = 1,
    Volatility = 1,
    SharpeRatio = 1,
  ) |>
  tidyr::pivot_wider(names_from = Symbol, values_from = Weight) |>
  as.data.frame()

PortfolioMontecarlo::PlotPortfolioPerformance(
  data = data,
  portfolios = list(
    "Benchmark" = out$portfolios$Benchmark,
    "SuperInvestor" = portfolio_df,
    "Consensus" = out$portfolios$Consensus
  ),
  logY = TRUE
)
