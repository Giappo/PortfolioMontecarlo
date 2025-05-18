#' @export
GetCurrentPE <- function(symbol) {
  if (symbol == "BRK-B") symbol <- "BRK.B"
  url <- paste0("https://www.stockanalysis.com/stocks/", tolower(symbol), "/")

  page <- rvest::read_html(url)
  pe_ratio <- page |>
    rvest::html_nodes("body") |>
    rvest::html_text() |>
    stringr::str_extract("PE Ratio\\s+[0-9\\.]+") |>
    stringr::str_extract("[0-9\\.]+") |>
    as.numeric()

  return(pe_ratio)
}

#' @export
GetForwardPE <- function(symbol) {
  if (symbol == "BRK-B") symbol <- "BRK.B"
  url <- paste0("https://www.stockanalysis.com/stocks/", tolower(symbol), "/")

  page <- rvest::read_html(url)
  pe_ratio <- page |>
    rvest::html_nodes("body") |>
    rvest::html_text() |>
    stringr::str_extract("Forward PE\\s+[0-9\\.]+") |>
    stringr::str_extract("[0-9\\.]+") |>
    as.numeric()

  return(pe_ratio)
}

#' @export
GetCurrentPEdf <- function(df) {
  df |>
    dplyr::mutate(
      PE = purrr::map_dbl(Symbol, function(sym) {
        pe <- PortfolioMontecarlo::GetCurrentPE(sym)
        if (length(pe) == 0 || is.na(pe)) {
          return(NA_real_)
        }
        as.numeric(pe)
      })
    )
}

#' @export
GetForwardPEdf <- function(df) {
  df |>
    dplyr::mutate(
      ForwardPE = purrr::map_dbl(Symbol, function(sym) {
        pe <- PortfolioMontecarlo::GetForwardPE(sym)
        if (length(pe) == 0 || is.na(pe)) {
          return(NA_real_)
        }
        as.numeric(pe)
      })
    )
}
