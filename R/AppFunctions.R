#' @export
DeployApp <- function() {
  devtools::install_github("Giappo/PortfolioMontecarlo", force = TRUE)
  rsconnect::deployApp("inst/shinyApp", appName = "PortfolioMontecarlo")
}

#' @export
RunApp <- function() {
  shiny::runApp("inst/shinyApp")
}
