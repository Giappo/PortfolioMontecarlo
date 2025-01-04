#' @export
FormatDate <- function(inputDate) {
  # Controlla se l'input è già nel formato "YYYY-MM-DD"
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", inputDate)) {
    return(inputDate)  # Restituisce la stessa data se già nel formato giusto
  }

  # Converte la data dal formato "YYYYMMDD" al formato "YYYY-MM-DD"
  formattedDate <- as.Date(as.character(inputDate), format = "%Y%m%d")
  return(format(formattedDate, "%Y-%m-%d"))
}
