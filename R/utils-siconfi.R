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
    periodicidade <- if (relatorio == "RGF") "Q" else "B" # RREO costuma ser Bimestral por padrão
  }

  # 3. Formatação do Anexo (Garante o 0 antes de números < 10)
  num_anexo <- sprintf("%s-Anexo %02d", relatorio, as.numeric(anexo))

  # 4. Construção Segura dos Parâmetros (URL Encoding automático pelo httr)
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

  # 5. Requisição à API
  resp <- httr::GET(base_url, query = query_params, config = httr::config(connecttimeout = 60))

  if (httr::status_code(resp) != 200) return(data.frame())

  # 6. Parse do JSON
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  dados_json <- jsonlite::fromJSON(txt, flatten = FALSE)

  if (length(dados_json$items) == 0) return(data.frame())

  df <- as.data.frame(dados_json$items)

  # 7. Tradução Matemática Universal das Colunas <MR-X> (RGF Anexo 1 e RREO Anexo 3)
  if ("coluna" %in% names(df) && any(grepl("<MR", df$coluna))) {

    mes_base <- dplyr::case_when(
      relatorio == "RREO" ~ as.numeric(periodo) * 2,
      relatorio == "RGF" & isTRUE(simplificado) ~ as.numeric(periodo) * 6,
      relatorio == "RGF" & !isTRUE(simplificado) ~ as.numeric(periodo) * 4,
      TRUE ~ 12
    )

    meses_pt <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")

    df <- df |>
      dplyr::mutate(
        # 7.1 Cria uma flag indicando se a linha atual é de fato uma coluna de "Mês de Referência"
        is_mr = grepl("<MR", coluna),

        # 7.2 Extrai o X apenas se for MR. Caso contrário, ignora.
        x_val = ifelse(is_mr, as.numeric(stringr::str_extract(coluna, "\\d+")), NA_real_),
        x_val = ifelse(is_mr & is.na(x_val), 0, x_val),

        # 7.3 Calcula a distância do mês atual
        mes_calc = ifelse(is_mr, mes_base - x_val, NA_real_),

        # 7.4 Matemática modular: garante que o resultado sempre fique entre 1 e 12 perfeitamente.
        mes = ifelse(is_mr, ((mes_calc - 1) %% 12) + 1, NA_real_),

        # 7.5 Retrocesso de Ano usando Divisão Inteira (floor)
        ano_ref = ifelse(is_mr, as.numeric(ano) + floor((mes_calc - 1) / 12), NA_real_),

        # 7.6 Substitui o texto da coluna APENAS onde for <MR. O resto fica intacto (ex: PREVISÃO 2022).
        coluna = ifelse(is_mr, paste0(meses_pt[mes], "/", ano_ref), coluna),

        valor = as.numeric(valor)
      ) |>
      dplyr::select(-is_mr, -x_val, -mes_calc, -mes, -ano_ref)
  }

  return(df)
}
