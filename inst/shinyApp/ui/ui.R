PortfolioMontecarlo::LoadPanels()

header <- shinydashboard::dashboardHeader(title = "Portfolio Optimizer")

sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    shiny::numericInput(
      inputId = "RISK_FREE_RATE",
      label = "Risk free rate:",
      value = 0,
      min = 0,
      max = Inf,
      step = 0.01
    ),
    shiny::numericInput(
      inputId = "NUM_PORTFOLIOS",
      label = "Number of simulations:",
      value = 10000,
      min = 1,
      step = 1
    ),
    shiny::numericInput(
      inputId = "NUM_CONSENSUS",
      label = "N. portfolios (consensus):",
      value = 100,
      min = 1,
      step = 1
    ),
    shiny::numericInput(
      inputId = "start_date",
      label = "Start Date:",
      value = 20170101,
      min = 20010101,
      max = 99999999,
      step = 1
    ),
    shiny::numericInput(
      inputId = "end_date",
      label = "End Date:",
      value = as.numeric(format(Sys.Date(), "%Y%m%d")),
      min = 20010101,
      max = 99999999,
      step = 1
    ),
    shiny::textInput(
      inputId = "MARKET_REPRESENTATION",
      label = "Benchmark:",
      value = "SPY"
    ),
    shiny::numericInput(
      inputId = "numRows",
      label = "Number of rows:",
      value = 10,
      min = 1,
      step = 1
    ),
    shiny::actionButton(
      inputId = "updateRows",
      label = "Update number of rows"
    )
  )
)

# Corpo della dashboard con i pannelli definiti sopra
body <- shinydashboard::dashboardBody(
  shiny::tabsetPanel(
    panelInput,
    panelOutput
  )
)

# UI utilizzando la struttura di shinydashboard
ui <- shinydashboard::dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)
