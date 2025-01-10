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
  portfolios,
  logY = FALSE
) {
  plotData <- list()
  for (i in seq_along(portfolios)) {
    plotData[[i]] <- PortfolioMontecarlo::CalculatePortfolioPerformance(
      portfolio = portfolios[[i]],
      data = data
    )
  }

  p <- plotly::plot_ly(
    plotData[[1]],
    x = ~Date,
    y = ~Portfolio,
    type = "scatter",
    mode = "lines",
    name = names(portfolios)[1]
  )

  for (i in seq_along(plotData[-1])) {
    p <- plotly::add_trace(
      p = p,
      data = plotData[[i + 1]],
      x = ~Date,
      y = ~Portfolio,
      type = "scatter",
      mode = "lines",
      name = names(portfolios)[i + 1]
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

#' @export
PlotPortfolioDrawdown <- function(
  data,
  portfolio
) {
  performance <- PortfolioMontecarlo::CalculatePortfolioPerformance(
    portfolio = portfolio,
    data = data
  )

  p <- plotly::plot_ly(
    performance,
    x = ~Date,
    y = ~Drawdown,
    type = "scatter",
    mode = "lines",
    name = "Portfolio"
  )

  nTicks <- 8
  tickvals <- 1:nTicks * floor(nrow(data) / nTicks)

  xaxis <- list(
    title = "Date",
    tickangle = 90,
    ticktext = zoo::index(data)[tickvals],
    tickvals = tickvals
  )
  yaxis <- list(title = "Drawdown")

  p <- p |>
    plotly::layout(
      title = "Portfolio Drawdown",
      xaxis = xaxis,
      yaxis = yaxis
    )

  p
}
