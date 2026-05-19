#' @title Request Template para SICONFI
#' @description Abstração para chamadas à API com tratamento de erros.
#' @param url String. Endpoint da API.
#' @return Objeto httr.
.fetch_siconfi_data <- function(url) {
  response <- httr::GET(url)

  # Tratamento robusto de erro HTTP
  if (httr::http_error(response)) {
    stop(sprintf("Erro na API SICONFI: %s", httr::http_status(response)$message))
  }

  return(httr::content(response, as = "text", encoding = "UTF-8") |>
           jsonlite::fromJSON())
}
