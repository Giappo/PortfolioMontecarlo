#' @export
PlotEfficientFrontier <- function(
  efficient_frontier,
  portfolios,
  RISK_FREE_RATE = 0
) {
  efficient_frontier$Name <- efficient_frontier$Simulation

  p <- efficient_frontier |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = Volatility,
        y = Return,
        color = SharpeRatio,
        text = paste(
          "Name:", Name,
          "<br>Volatility:", scales::percent(Volatility),
          "<br>Return:", scales::percent(Return),
          "<br>Sharpe Ratio:", round(SharpeRatio, 2)
        )
      )
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_gradient(low = "purple", high = "lightseagreen", name = "Sharpe Ratio") +
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

  defaultColors <- PortfolioMontecarlo::GetPlotlyDefaultColors(length(portfolios))

  i <- 1
  for (i in seq_along(portfolios)) {
    portfolio <- portfolios[[i]]
    portfolio$Name <- names(portfolios)[i]
    p <- p +
      ggplot2::geom_point(
        data = portfolio,
        mapping = ggplot2::aes(
          x = Volatility,
          y = Return,
          text = paste(
            "Name:", Name,
            "<br>Volatility:", scales::percent(Volatility),
            "<br>Return:", scales::percent(Return),
            "<br>Sharpe Ratio:", round(SharpeRatio, 2)
          )
        ),
        size = 4,
        shape = 23,
        color = "black",
        fill = defaultColors[i]
      )
  }

  p |> plotly::ggplotly(tooltip = "text")
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
  i <- 1
  for (i in seq_along(portfolios)) {
    plotData[[i]] <- PortfolioMontecarlo::CalculatePortfolioPerformanceOverTime(
      portfolio = portfolios[[i]],
      data = data
    )
  }

  p <- plotly::plot_ly()
  for (i in seq_along(portfolios)) {
    p <- plotly::add_trace(
      p = p,
      data = plotData[[i]],
      x = ~Date,
      y = ~Portfolio,
      type = "scatter",
      mode = "lines",
      name = names(portfolios)[i]
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

  p
}

#' @export
PlotPortfolioDrawdown <- function(
  data,
  portfolios
) {
  plotData <- list()
  for (i in seq_along(portfolios)) {
    plotData[[i]] <- PortfolioMontecarlo::CalculatePortfolioPerformanceOverTime(
      portfolio = portfolios[[i]],
      data = data
    )
  }

  p <- plotly::plot_ly()
  for (i in seq_along(portfolios)) {
    p <- plotly::add_trace(
      p = p,
      data = plotData[[i]],
      x = ~Date,
      y = ~Drawdown,
      type = "scatter",
      mode = "lines",
      name = names(portfolios)[i]
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
  yaxis <- list(title = "Drawdown")

  p <- p |>
    plotly::layout(
      title = "Portfolio Drawdown",
      xaxis = xaxis,
      yaxis = yaxis
    )

  p
}

#' @export
PlotPortfolioGains <- function(
  data,
  portfolios
) {
  plotData <- list()
  for (i in seq_along(portfolios)) {
    plotData[[i]] <- PortfolioMontecarlo::CalculatePortfolioPerformanceOverTime(
      portfolio = portfolios[[i]],
      data = data
    )
  }

  # Estimate densities
  densityList <- lapply(plotData, function(df) {
    stats::density(df$DailyGain)
  })

  p <- plotly::plot_ly()
  for (i in seq_along(portfolios)) {
    p <- plotly::add_trace(
      p = p,
      x = densityList[[i]]$x,
      y = densityList[[i]]$y,
      type = "scatter",
      mode = "lines",
      fill = "tozeroy",
      alpha = 0.1,
      name = names(portfolios)[i]
    )
  }

  p <- p |>
    plotly::layout(
      title = "Smoothed Daily Gain Distributions",
      xaxis = list(title = "Daily Gain"),
      yaxis = list(title = "Density")
    )

  p
}
