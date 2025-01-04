panelInput <- shiny::tabPanel(
  "Input",
  shinydashboard::box(
    width = 12,
    shiny::fileInput(
      inputId = "csvFile",
      label = "Import CSV file",
      accept = c(".csv")
    )
  ),
  shinydashboard::box(
    title = "Input assets",
    width = 12,
    rhandsontable::rHandsontableOutput("EditableTable")
  ),
  shiny::actionButton("runMontecarlo", "Run Montecarlo")
)
