#' Motor Interno de Requisições SICONFI
#' @keywords internal
#' @noRd
.fetch_siconfi_api <- function(ano, periodo, anexo, ibge, relatorio, simplificado = FALSE, poder = NULL) {

  base_url <- if (relatorio == "RGF") {
    "https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rgf"
  } else {
    "https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rreo"
  }

  esfera <- dplyr::case_when(
    nchar(as.character(ibge)) == 1 ~ "U",
    nchar(as.character(ibge)) == 2 ~ "E",
    nchar(as.character(ibge)) == 7 ~ "M",
    TRUE ~ NA_character_
  )

  if (isTRUE(simplificado)) {
    tipo_relatorio <- paste(relatorio, "Simplificado")
    periodicidade <- "S"
  } else {
    tipo_relatorio <- relatorio
    periodicidade <- if (relatorio == "RGF") "Q" else "B"
  }

  num_anexo <- sprintf("%s-Anexo %02d", relatorio, as.numeric(anexo))

  query_params <- list(
    an_exercicio = ano,
    nr_periodo = periodo,
    co_tipo_demonstrativo = tipo_relatorio,
    no_anexo = num_anexo,
    co_esfera = esfera,
    id_ente = ibge
  )

  if (relatorio == "RGF") {
    query_params$in_periodicidade <- periodicidade
    if (!is.null(poder)) query_params$co_poder <- poder
  }

  # Requisição com tratamento de falhas de rede
  resp <- tryCatch({
    httr::GET(base_url, query = query_params, config = httr::config(connecttimeout = 60))
  }, error = function(e) {
    cli::cli_alert_warning("Falha de conex\u00e3o ao buscar {relatorio} ({ano}): {e$message}")
    return(NULL)
  })

  if (is.null(resp)) return(data.frame())

  # Alerta sobre status HTTP adversos (401, 500, etc) sem quebrar o loop
  if (httr::http_error(resp)) {
    cli::cli_alert_warning(
      "Erro HTTP {httr::status_code(resp)} na API do SICONFI para o ente {ibge}."
    )
    return(data.frame())
  }

  txt <- httr::content(resp, as = "text", encoding = "UTF-8")

  # Proteção contra Payload corrompido ou HTML mascarado de JSON
  dados_json <- tryCatch({
    jsonlite::fromJSON(txt, flatten = FALSE)
  }, error = function(e) {
    cli::cli_alert_warning("Falha ao interpretar o JSON retornado para o ente {ibge}.")
    return(NULL)
  })

  if (is.null(dados_json) || length(dados_json$items) == 0) return(data.frame())

  df <- as.data.frame(dados_json$items)

  # [O restante do bloco de tratamento "Matemática Universal das Colunas <MR-X>" permanece idêntico]
  # ... (Linhas 66 a 99 do seu arquivo original) ...

  return(df)
}
