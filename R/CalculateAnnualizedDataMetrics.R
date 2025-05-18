#' @export
CalculateAnnualizedDataMetrics <- function(data, RISK_FREE_RATE = 0) {
  cols <- colnames(data)
  N <- length(cols)
  outS <- outV <- outR <- rep(NA, N)
  names(outS) <- names(outV) <- names(outR) <- cols
  for (i in seq_along(cols)) {
    col <- cols[i]
    x <- data[, col]
    pcts <- PctChange(x)
    returnDaily <- tail(cumprod(1 + pcts), 1) ^ (1 / nrow(pcts)) - 1
    returnDaily <- returnDaily |> as.numeric()
    sdDaily <- pcts |> sd(na.rm = TRUE)

    returnAnnual <- returnDaily * 252
    sdAnnual <- sdDaily * sqrt(252)
    sharpeRatio <- (returnAnnual - RISK_FREE_RATE) / sdAnnual

    outR[i] <- returnAnnual
    outV[i] <- sdAnnual
    outS[i] <- sharpeRatio
  }
  list(
    Return = outR,
    Volatility = outV,
    SharpeRatio = outS
  )
}
