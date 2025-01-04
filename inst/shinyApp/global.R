if (!requireNamespace("PortfolioMontecarlo", quietly = TRUE)) {
  remotes::install_github("Giappo/PortfolioMontecarlo", ref = "main")
}

library(dplyr)
library(DT)
library(grDevices)
library(ggplot2)
library(logger)
library(plotly)
library(quantmod)
library(rvest)
library(rhandsontable)
library(scales)
library(shiny)
library(shinydashboard)
library(PortfolioMontecarlo)
