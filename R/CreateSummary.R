CreateSummary <- function(portfolio) {

  data.frame(
    "Return" = portfolio$Return,
    "Volatility" = portfolio$Volatility
  )
}
