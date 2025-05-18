panelOutput <- shiny::tabPanel(
  "Output",
  shinydashboard::box(
    title = "Allocation",
    DT::DTOutput("portfolioTables")
  ),
  shinydashboard::box(
    title = "Statistics",
    DT::DTOutput("tableStats")
  ),
  shinydashboard::box(
    title = "Performance",
    plotly::plotlyOutput("plotPerformance"),
  ),
  shinydashboard::box(
    title = "Gains",
    plotly::plotlyOutput("plotGains"),
  ),
  shinydashboard::box(
    title = "Drawdown",
    plotly::plotlyOutput("plotDrawDown"),
  ),
  shinydashboard::box(
    title = "Efficient Frontier",
    plotly::plotlyOutput("plotFrontier")
  )
)
