panelOutput <- shiny::tabPanel(
  "Output",
  shiny::fluidRow(
    shinydashboard::box(
      title = "Allocation Consensus",
      width = 4,
      DT::DTOutput("tablePFConsensus")
    ),
    shinydashboard::box(
      title = "Allocation MaxSR",
      width = 4,
      DT::DTOutput("tablePFMaxSR")
    ),
    shinydashboard::box(
      title = "Allocation Benchmark",
      width = 4,
      DT::DTOutput("tablePFBenchmark")
    )
  ),
  shiny::fluidRow(
    shinydashboard::box(
      title = "Statistics",
      width = 6,
      height = 700,
      DT::DTOutput("tableStats")
    ),
    shinydashboard::box(
      title = "Allocation",
      width = 6,
      height = 700,
      plotly::plotlyOutput("plotPie")
    )
  ),
  shinydashboard::box(
    title = "Performance",
    width = 12,
    plotly::plotlyOutput("plotPerformance"),
  ),
  shinydashboard::box(
    title = "Gains",
    width = 12,
    plotly::plotlyOutput("plotGains"),
  ),
  shinydashboard::box(
    title = "Drawdown",
    width = 12,
    plotly::plotlyOutput("plotDrawDown"),
  ),
  shinydashboard::box(
    title = "Efficient Frontier",
    width = 12,
    plotly::plotlyOutput("plotFrontier")
  )
)
