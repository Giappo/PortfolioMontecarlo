#' @export
FindLocalDownloadFolder <- function() {
  file.path(Sys.getenv("USERPROFILE"), "Downloads") |>
    strsplit(split = "\\\\|/|//") |>
    unlist() |>
    as.list() |>
    do.call(what = file.path)
}

#' @export
DownloadData <- function(
  assets,
  start_date,
  end_date
) {
  start_date <- start_date |> FormatDate()
  end_date <- end_date |> FormatDate()

  PortfolioMontecarlo::WithProgress(
    message = "Downloading assets...",
    expr = {
      total_assets <- length(assets)

      data_list <- lapply(seq_along(assets), function(i) {
        asset <- assets[i]

        cat("Downloading:", asset, "\n")
        PortfolioMontecarlo::SetProgress(index = i, maxIndex = total_assets)

        quantmod::getSymbols(
          asset,
          src = "yahoo",
          from = start_date,
          to = end_date,
          auto.assign = TRUE
        )

        # Estrai i prezzi di chiusura aggiustati (Adjusted Close)
        data <- quantmod::Ad(get(asset))  # 'Ad()' restituisce i dati 'Adjusted Close'
        return(data)
      })
    })


  # Combina i dati in una matrice, dove ogni colonna è un asset
  combined_data <- do.call(cbind, data_list)

  # Rinomina le colonne con i simboli degli asset
  colnames(combined_data) <- assets

  return(combined_data)
}

#' @export
GenerateRandomWeights <- function(minWeights, maxWeights) {
  minWeights <- as.numeric(minWeights)  # Converti in numerico
  maxWeights <- as.numeric(maxWeights)  # Converti in numerico

  while (TRUE) {
    # Genera pesi casuali tra minWeights e maxWeights
    random_weights <- runif(length(minWeights)) * (maxWeights - minWeights) + minWeights

    # Normalizza i pesi per sommare a 1
    random_weights <- random_weights / sum(random_weights)

    # Controlla se i pesi sono validi, ossia che siano compresi tra minWeights e maxWeights
    if (all(random_weights >= minWeights) && all(random_weights <= maxWeights)) {
      break
    }
  }

  return(random_weights)
}

#' @export
PctChange <- function(x) {
  x2 <- x |>
    log() |>
    diff() |>
    exp()

  x3 <- x2 - 1
  return(x3[-1, ])
}

#' @export
AddConsensusSimulation <- function(
  output,
  ASSETS,
  data,
  # fraction = 0.01,
  NUM_CONSENSUS,
  RISK_FREE_RATE = 0
) {

  logger::log_info("AddConsensusSimulation. Start.")
  on.exit(logger::log_info("AddConsensusSimulation. Done."))

  # Calcola i rendimenti giornalieri
  dailyReturns <- data |> PctChange()
  covMatrix <- stats::cov(dailyReturns)
  dataMetrics <- data |> PortfolioMontecarlo::CalculateAnnualizedMetrics(RISK_FREE_RATE = RISK_FREE_RATE)

  # Seleziona il top-performing fraction%
  # nWinners <- max(floor(nrow(output) * fraction), 1)
  nWinners <- max(floor(NUM_CONSENSUS), 1)
  winnersOutput <- output |>
    dplyr::arrange(dplyr::desc(SharpeRatio)) |>
    dplyr::slice_head(n = nWinners)

  # Definisci i pesi basati sul quadrato del Sharpe Ratio
  winnersOutput <- winnersOutput |>
    dplyr::mutate(
      SRSquared = SharpeRatio ^ 2,
      Weight = SRSquared / sum(SRSquared)
    ) |>
    dplyr::select(-SRSquared) # Rimuove la colonna temporanea

  # Moltiplica i valori della colonna per il peso
  for (asset in ASSETS) {
    winnersOutput[[asset]] <- winnersOutput[[asset]] * winnersOutput$Weight
  }

  # Calcola i pesi di consenso sommando per asset
  consensusWeights <- colSums(winnersOutput[, ASSETS, drop = FALSE])

  # Ottieni l'ID della prossima simulazione
  nextSimulationID <- max(output$Simulation, na.rm = TRUE) + 1

  annualizedReturns <- dataMetrics$Return
  consensusReturn <- sum(consensusWeights * annualizedReturns)

  # Calcola il rendimento del portafoglio
  # consensusReturn <- sum(consensusWeights * colMeans(dailyReturns)) * 252  # Rendimento annualizzato

  # Calcola la volatilità del portafoglio
  consensusStddev <- sqrt(t(consensusWeights) %*% covMatrix %*% consensusWeights) * sqrt(252)  # Volatilità annualizzata

  # Calcola il rapporto di Sharpe
  consensusSharpeRatio <- (consensusReturn - RISK_FREE_RATE) / consensusStddev

  # Crea una nuova riga con i valori calcolati
  newRow <- tibble::tibble(
    Simulation = nextSimulationID,
    Return = consensusReturn,
    Volatility = consensusStddev,
    SharpeRatio = consensusSharpeRatio
  )

  # Aggiungi i pesi di ciascun asset alla nuova riga
  for (asset in names(consensusWeights)) {
    newRow[[asset]] <- consensusWeights[asset]
  }

  # Unisci la nuova riga al DataFrame di output
  output2 <- dplyr::bind_rows(output, newRow)

  return(output2)
}

#' @export
ExtractEfficientFrontier <- function(output, nDigits = 3) {
  threshold <- 10 ^ (-nDigits)

  output$Volatility <- output$Volatility |> signif(digits = nDigits)

  output$VolatilityBin <- cut(
    output$Volatility,
    breaks = seq(0, max(output$Volatility) + threshold, by = threshold), # Precisione dello 0.1%
    include.lowest = TRUE
  )

  maxReturnPortfolios <- output |>
    dplyr::group_by(VolatilityBin) |>
    dplyr::filter(Return == max(Return)) |>
    dplyr::ungroup() |>
    dplyr::arrange(Volatility) |>
    dplyr::select(-VolatilityBin)

  return(maxReturnPortfolios)
}
