panelOutput <- shiny::tabPanel(
  "Output",
  shiny::fluidRow(
    shinydashboard::box(
      title = "Allocation",
      width = 6,
      height = 500,
      plotly::plotlyOutput("plotPie")
    ),
    shinydashboard::box(
      title = "Allocation",
      width = 6,
      height = 500,
      plotly::plotlyOutput("plotPie")
    )
  ),
  shinydashboard::box(
    title = "Performance",
    width = 12,
    plotly::plotlyOutput("plotPerformance"),
  ),
  shinydashboard::box(
    title = "Efficient Frontier",
    width = 12,
    plotly::plotlyOutput("plotFrontier")
  )
)
