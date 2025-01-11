panelInput <- shiny::tabPanel(
  "Input",
  shiny::fluidRow(
    shinydashboard::box(
      width = 6,
      height = 90,
      shiny::fileInput(
        inputId = "csvFile",
        label = "Import CSV file",
        accept = c(".csv")
      )
    ),
    shinydashboard::box(
      width = 6,
      height = 90,
      shiny::actionButton(
        inputId = "runMontecarlo",
        label = "Run Montecarlo",
        class = "btn-block btn-primary",
        style = "width: 100%; height: 100%; font-size: 20px;",
        disabled = TRUE
      ),
      style = "height: 100%; display: flex; align-items: center; justify-content: center;",
    )
  ),

  shinydashboard::box(
    title = "Input assets",
    width = 12,
    rhandsontable::rHandsontableOutput("EditableTable")
  )

)
