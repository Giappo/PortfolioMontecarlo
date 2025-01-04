uiFile <- system.file("shinyApp", "ui", "ui.R", package = "PortfolioMontecarlo")
serverFile <- system.file("shinyApp", "server", "server.R", package = "PortfolioMontecarlo")

if (uiFile == "" || serverFile == "") {
  stop("Errore: Non è stato possibile trovare `ui.R` o `server.R` nel pacchetto.")
}

source(uiFile)
source(serverFile)

shiny::shinyApp(ui = ui, server = server)
