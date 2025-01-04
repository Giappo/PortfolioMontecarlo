#' @export
LoadPanels <- function() {
  uiFolder <- system.file("shinyApp", "ui", package = "PortfolioMontecarlo")
  panelFiles <- list.files(uiFolder, full.names = TRUE, pattern = "panel.*\\.R$")

  for (file in panelFiles) {
    source(file)
    message("Caricato file: ", file)
  }
  return(NULL)
}
