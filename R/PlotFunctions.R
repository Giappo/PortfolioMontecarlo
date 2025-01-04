#' @export
PlotOutput <- function(
  efficient_frontier,
  portfolios,
  market_data = NULL,
  RISK_FREE_RATE = 0
) {
  p <- efficient_frontier |>
    ggplot2::ggplot(ggplot2::aes(x = Volatility, y = Return, color = SharpeRatio)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_gradient(low = "blue", high = "green", name = "Sharpe Ratio") +
    ggplot2::labs(
      title = "Efficient Frontier",
      x = "Volatility",
      y = "Return"
    ) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom"
    )

  if (!is.null(market_data)) {
    marketDailyReturns <- market_data |> PctChange()
    marketReturn <- mean(marketDailyReturns) * 252  # Rendimento annualizzato
    marketVolatility <- sd(marketDailyReturns) * sqrt(252)  # Volatilità annualizzata
    marketSharpeRatio <- (marketReturn - RISK_FREE_RATE) / marketVolatility  # Sharpe ratio

    p <- p +
      ggplot2::geom_point(ggplot2::aes(x = marketVolatility, y = marketReturn), color = "red", size = 4, shape = 16)
  }

  for (portfolio in portfolios) {
    p <- p +
      ggplot2::geom_point(data = portfolio, ggplot2::aes(x = Volatility, y = Return), color = "red", size = 4, shape = 8)
  }

  print(p)
}

#' @export
PlotPie <- function(portfolio) {
  # Rimuovi le colonne non relative ai pesi
  portfolio_weights <- portfolio[, !names(portfolio) %in% c("Simulation", "Return", "Volatility", "SharpeRatio")]

  # Trasforma in un formato lungo per plotly
  portfolio_long <- data.frame(
    Asset = names(portfolio_weights),
    Weight = as.numeric(portfolio_weights[1, ])
  )

  # Ordina gli asset per peso (opzionale, per un grafico più leggibile)
  portfolio_long <- portfolio_long[order(-portfolio_long$Weight), ]

  # Genera una palette di colori meno accesi
  num_assets <- nrow(portfolio_long)
  color_palette <- grDevices::hcl.colors(num_assets, palette = "Pastel1")

  # Crea il grafico a torta interattivo con linee di separazione nere
  plotly::plot_ly(
    portfolio_long,
    labels = ~Asset,
    values = ~Weight,
    type = "pie",
    marker = list(
      colors = color_palette,
      line = list(color = "black", width = 1)  # Linee nere tra gli spicchi
    ),
    textinfo = "label+percent",
    insidetextorientation = "radial"
  ) |>
    plotly::layout(
      showlegend = TRUE
    )
}

#' @export
PlotPortfolioPerformance <- function(
    data,
    portfolio,
    market_data = NULL,
    logY = FALSE
) {
  portfolio <- portfolio[colnames(data)]
  portfolioReturns <- as.matrix(portfolio) %*% t(PctChange(data))
  portfolioCum <- cumprod(1 + portfolioReturns)

  plotData <- data.frame(
    Date = zoo::index(portfolioCum),
    Portfolio = as.numeric(portfolioCum)
  )

  if (!is.null(market_data)) {
    marketCum <- cumprod(1 + PctChange(market_data))
    plotData$Market <- as.numeric(marketCum)
  }

  p <- plotly::plot_ly(
    plotData,
    x = ~Date,
    y = ~Portfolio,
    type = "scatter",
    mode = "lines",
    name = "Portfolio"
  )

  if (!is.null(market_data)) {
    p <- p |>
      plotly::add_trace(
        y = ~Market,
        name = "Market",
        mode = "lines"
      )
  }

  nTicks <- 8
  tickvals <- 1:nTicks * floor(nrow(data) / nTicks)

  xaxis <- list(
    title = "Date",
    tickangle = 90,
    ticktext = zoo::index(data)[tickvals],
    tickvals = tickvals
  )
  yaxis <- list(title = "Cumulative Performance")

  if (logY) {
    yaxis$type = "log"
  }

  p <- p |>
    plotly::layout(
      title = "Portfolio Performance Over Time",
      xaxis = xaxis,
      yaxis = yaxis
    )

  return(p)
}
