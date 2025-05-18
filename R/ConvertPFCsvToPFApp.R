#' @export
GetSymbolAndTypeFromISIN <- function(isin) {
  # Fallback manual per casi noti
  fallbackSymbols <- list(
    "IE0031442068" = list(Symbol = "IUSA", Type = "etf")
  )
  if (!is.na(isin) && isin %in% names(fallbackSymbols)) {
    return(fallbackSymbols[[isin]])
  }

  if (is.na(isin) || isin == "") {
    return(list(Symbol = NA_character_, Type = NA_character_))
  }

  # Yahoo Finance lookup
  urlYahoo <- paste0("https://finance.yahoo.com/lookup?s=", isin)
  pageYahoo <- tryCatch(
    rvest::read_html(urlYahoo),
    error = function(e) NULL
  )

  if (!is.null(pageYahoo)) {
    tblYahoo <- tryCatch(
      rvest::html_element(pageYahoo, "table") |>
        rvest::html_table(),
      error = function(e) NULL
    )
    if (!is.null(tblYahoo) && nrow(tblYahoo) > 0) {
      symbolY_raw <- tblYahoo$Symbol[1]
      # Rimuovi suffissi di borsa (es. .MI, .PA) e prefissi numerici
      symbolY <- symbolY_raw |>
        sub("\\..*$", "", x = _) |>         # rimuove tutto dopo il punto (es. .MI)
        sub("^\\d+", "", x = _)   |>        # rimuove prefissi numerici
        toupper()
      typeY   <- tolower(tblYahoo$Type[1])
      # normalizza valori tipo
      typeClean <- dplyr::case_when(
        grepl("etf", typeY)   ~ "etf",
        grepl("equity|stock", typeY) ~ "stock",
        TRUE                     ~ "stock"
      )
      return(list(Symbol = symbolY, Type = typeClean))
    }
  }

  # Se non trovato
  list(Symbol = NA_character_, Type = NA_character_)
}

#' @export
BuildCleanPortfolioDf <- function(df) {
  df |>
    # escludi cash / senza ISIN
    dplyr::filter(!is.na(Symbol.ISIN) & Symbol.ISIN != "") |>
    # mappa ISIN → Symbol/Type
    dplyr::mutate(
      tmp    = purrr::map(Symbol.ISIN, GetSymbolAndTypeFromISIN),
      Symbol = purrr::map_chr(tmp, "Symbol"),
      Type   = purrr::map_chr(tmp, "Type")
    ) |>
    dplyr::select(-tmp) |>
    # calcola pesi normalizzati
    dplyr::mutate(
      Weight = Value.in.EUR / sum(Value.in.EUR, na.rm = TRUE)
    ) |>
    # rimuovi NA
    dplyr::filter(!is.na(Symbol) & Symbol != "") |>
    # ordina e seleziona colonne
    dplyr::arrange(dplyr::desc(Weight)) |>
    dplyr::select(Symbol, Type, Weight)
}

#' @export
ProcessPortfolioFile <- function(filepath) {
  # Leggi CSV
  df <- read.csv(filepath, stringsAsFactors = FALSE)

  cleanDf <- BuildCleanPortfolioDf(df)

  stocksDf <- cleanDf |>
    dplyr::filter(Type == "stock")

  stocksDf <- stocksDf |>
    dplyr::mutate(
      Weight = Weight / sum(Weight, na.rm = TRUE)
    )

  stocksDf <- stocksDf |>
    dplyr::mutate(
      MinWeight = 0,
      MaxWeight = 0.2
    ) |>
    dplyr::select(
      Symbol,
      Weight,
      MinWeight,
      MaxWeight
    )

  outputPath <- file.path(
    dirname(filepath),
    "PortfolioApp.csv"
  )
  write.csv(stocksDf, outputPath, row.names = FALSE)

  cat("Saved portfolio in ", outputPath, "\n")
  return(stocksDf)
}

#' @export
GetMostRecentPortfolioFile <- function(
  folderPath,
  filename = "Portfolio.csv"
) {
  baseName <- tools::file_path_sans_ext(filename)
  extension <- tools::file_ext(filename)

  files <- list.files(
    path = folderPath,
    pattern = paste0("^", baseName, "( \\(\\d+\\))?\\.", extension, "$"),
    full.names = TRUE
  )

  if (length(files) == 0) {
    stop("Nessun file Portfolio.csv trovato nella cartella.")
  }

  # Seleziona il più recente in base alla data di modifica
  latestFile <- files[which.max(file.info(files)$mtime)]
  return(latestFile)
}

#' @export
ConvertPFCsvToPFApp <- function(filename = "Portfolio.csv") {
  out <- PortfolioMontecarlo::FindLocalDownloadFolder() |>
    PortfolioMontecarlo::GetMostRecentPortfolioFile(filename) |>
    PortfolioMontecarlo::ProcessPortfolioFile()

  out
}
