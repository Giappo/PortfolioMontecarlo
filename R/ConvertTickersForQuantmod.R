#' @export
ConvertTickersForQuantmod <- function(
  tickers
) {
  # Known stock market suffixes
  knownSuffixes <- c(
    "PA", "SW", "MI", "L", "TO", "V", "AX", "NZ", "SI", "HK", "NS", "BO",
    "BK", "SS", "SZ", "TWO", "KS", "KQ", "SA", "MX", "IS", "VI", "MC", "DE",
    "IR", "TA", "JK", "PL", "PR", "CO", "HE", "ST", "OL", "AS", "BR", "BI",
    "LS", "VI", "F", "HM", "BE", "PK", "OB"
  )

  sapply(tickers, function(ticker) {
    if (grepl("\\.", ticker)) {
      parts <- unlist(strsplit(ticker, "\\."))
      suffix <- tail(parts, 1)
      if (suffix %in% knownSuffixes) {
        # Leave it as is if it's a known exchange
        ticker
      } else {
        # Replace dot with dash for class shares
        gsub("\\.", "-", ticker)
      }
    } else {
      ticker
    }
  }, USE.NAMES = FALSE)
}
