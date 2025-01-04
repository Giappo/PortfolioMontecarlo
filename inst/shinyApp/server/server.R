server <- function(input, output, session) {
  # Input table
  tableData <- shiny::reactiveVal(InitializeInputTable())

  shiny::observeEvent(input$updateRows, {
    n <- input$numRows
    currentTable <- tableData()
    newTable <- InitializeInputTable(n)

    # Copia i dati esistenti nella nuova tabella, se possibile
    commonRows <- min(n, nrow(currentTable))
    newTable[1:commonRows, ] <- currentTable[1:commonRows, ]
    tableData(newTable)
  })

  shiny::observeEvent(tableData(), {
    print(tableData())
  })

  output$EditableTable <- rhandsontable::renderRHandsontable({
    rhandsontable::rhandsontable(
      tableData(),
      rowHeaders = NULL
    ) |>
      rhandsontable::hot_col("Symbol", type = "text") |>
      rhandsontable::hot_col("Weight", type = "numeric", format = "0.00", min = 0, max = 1) |>
      rhandsontable::hot_col("MinWeight", type = "numeric", format = "0.00", min = 0, max = 1) |>
      rhandsontable::hot_col("MaxWeight", type = "numeric", format = "0.00", min = 0, max = 1)
  })

  shiny::observeEvent(input$EditableTable, {
    newData <- rhandsontable::hot_to_r(input$EditableTable)

    if ("Symbol" %in% colnames(newData)) {
      newData$Symbol <- toupper(newData$Symbol)
    }

    tableData(newData)
  })

  shiny::observeEvent(input$csvFile, {
    shiny::req(input$csvFile) # Assicurati che un file sia stato caricato

    # Leggi il file CSV
    uploadedData <- tryCatch(
      read.csv(input$csvFile$datapath, stringsAsFactors = FALSE),
      error = function(e) {
        shiny::showNotification("Errore nella lettura del file CSV", type = "error")
        return(NULL)
      }
    )

    if (!is.null(uploadedData)) {
      # Verifica e pulizia dei dati
      requiredColumns <- c("Symbol", "Weight", "MinWeight", "MaxWeight")
      if (all(requiredColumns %in% colnames(uploadedData))) {
        # Converti Symbol in maiuscolo
        uploadedData$Symbol <- toupper(uploadedData$Symbol)

        # Aggiorna i dati della tabella
        tableData(uploadedData)
      } else {
        shiny::showNotification(
          "Il file CSV deve contenere le colonne: Symbol, Weight, MinWeight, MaxWeight",
          type = "error"
        )
      }
    }
  })

  output$TableData <- shiny::renderPrint({
    tableData()
  })

  # Calcola
  ASSETS <- shiny::reactive({
    assets <- tableData() |> dplyr::pull(Symbol)
    cat("ASSETS updated:", assets, "\n")
    return(assets)
  })

  MIN_WEIGHTS <- shiny::reactive({
    min_weights <- tableData() |> dplyr::pull(MinWeight)
    cat("MIN_WEIGHTS updated:", min_weights, "\n")
    return(min_weights)
  })

  MAX_WEIGHTS <- shiny::reactive({
    max_weights <- tableData() |> dplyr::pull(MaxWeight)
    cat("MAX_WEIGHTS updated:", max_weights, "\n")
    return(max_weights)
  })

  shiny::observe({
    cat("tableData changed\n")
    ASSETS()
    MIN_WEIGHTS()
    MAX_WEIGHTS()
  })


  Out <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$runMontecarlo, {
    print("RUNNING MONTECARLO")

    # Calcola i risultati della simulazione Monte Carlo e aggiorna la reattiva Out
    result <- RunMontecarlo(
      ASSETS = ASSETS(),
      MIN_WEIGHTS = MIN_WEIGHTS(),
      MAX_WEIGHTS = MAX_WEIGHTS(),
      MARKET_REPRESENTATION = input$MARKET_REPRESENTATION,
      RISK_FREE_RATE = input$RISK_FREE_RATE,
      NUM_PORTFOLIOS = input$NUM_PORTFOLIOS,
      start_date = input$start_date,
      end_date = input$end_date
    )

    # Usa Out per memorizzare il risultato della simulazione
    Out(result)
  })

  # Output pie
  output$plotPie <- plotly::renderPlotly({
    shiny::req(Out())
    PlotPie(Out()$pf_consensus)
  })

  # Output performance
  output$plotPerformance <- plotly::renderPlotly({
    shiny::req(Out())
    PlotPortfolioPerformance(
      data = Out()$data,
      portfolio = Out()$pf_consensus,
      market_data = Out()$market_data
    )
  })

  # Output efficient frontier
  output$plotFrontier <- plotly::renderPlotly({
    shiny::req(Out())
    PlotOutput(
      efficient_frontier = Out()$efficient_frontier,
      portfolios = list(Out()$pf_consensus),
      market_data = Out()$market_data,
      RISK_FREE_RATE = input$RISK_FREE_RATE
    )
  })
}
