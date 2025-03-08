#' @export
DeployApp <- function() {
  detach("package:PortfolioMontecarlo", unload = TRUE)
  devtools::install_github("Giappo/PortfolioMontecarlo", force = TRUE)
  library(PortfolioMontecarlo)
  rsconnect::deployApp("inst/shinyApp", appName = "PortfolioMontecarlo")
}

#' @export
RunApp <- function() {
  shiny::runApp("inst/shinyApp")
}
