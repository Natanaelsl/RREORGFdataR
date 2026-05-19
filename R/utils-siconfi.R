#' Motor Interno de Requisições SICONFI
#' @keywords internal
#' @noRd
.fetch_siconfi_api <- function(ano, periodo, anexo, ibge, relatorio, simplificado = FALSE, poder = NULL) {

  # 1. Definição do Endpoint e Esfera
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

  # 2. Definição de Relatório e Periodicidade
  if (isTRUE(simplificado)) {
    tipo_relatorio <- paste(relatorio, "Simplificado")
    periodicidade <- "S"
  } else {
    tipo_relatorio <- relatorio
    periodicidade <- if (relatorio == "RGF") "Q" else "B"
  }

  # 3. Formatação do Anexo
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

  # 4. Requisição com tratamento de falhas de rede (Timeout/Offline)
  resp <- tryCatch({
    httr::GET(base_url, query = query_params, config = httr::config(connecttimeout = 60))
  }, error = function(e) {
    cli::cli_alert_warning("Falha de conex\u00e3o ao buscar {relatorio} ({ano}): {e$message}")
    return(NULL)
  })

  if (is.null(resp)) return(data.frame())

  # 5. Tratamento de Erro HTTP
  if (httr::http_error(resp)) {
    cli::cli_alert_warning(
      "Erro HTTP {httr::status_code(resp)} na API do SICONFI para o ente {ibge}."
    )
    return(data.frame())
  }

  # 6. Parse protegido do JSON
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")

  dados_json <- tryCatch({
    jsonlite::fromJSON(txt, flatten = FALSE)
  }, error = function(e) {
    cli::cli_alert_warning("Falha ao interpretar o JSON retornado para o ente {ibge}.")
    return(NULL)
  })

  if (is.null(dados_json) || length(dados_json$items) == 0) return(data.frame())

  df <- as.data.frame(dados_json$items)

  # 7. MATEMÁTICA UNIVERSAL DAS COLUNAS <MR-X> (Seu código original mantido)
  mes_base <- dplyr::case_when(
    relatorio == "RREO" ~ as.numeric(periodo) * 2,
    relatorio == "RGF" & isTRUE(simplificado) ~ as.numeric(periodo) * 6,
    relatorio == "RGF" & !isTRUE(simplificado) ~ as.numeric(periodo) * 4,
    TRUE ~ 12
  )

  meses_pt <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")

  df <- df |>
    dplyr::mutate(
      is_mr = grepl("<MR", coluna),
      x_val = ifelse(is_mr, as.numeric(stringr::str_extract(coluna, "\\d+")), NA_real_),
      x_val = ifelse(is_mr & is.na(x_val), 0, x_val),
      mes_calc = ifelse(is_mr, mes_base - x_val, NA_real_),
      mes = ifelse(is_mr, ((mes_calc - 1) %% 12) + 1, NA_real_),
      ano_ref = ifelse(is_mr, ano - floor((12 - mes_calc) / 12), NA_real_),
      coluna = ifelse(
        is_mr,
        sprintf("%s/%04d", meses_pt[mes], ano_ref),
        coluna
      )
    ) |>
    dplyr::select(-is_mr, -x_val, -mes_calc, -mes, -ano_ref)

  return(df)
}
