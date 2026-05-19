#' Extração Inteligente e Vetorizada de Dados do RGF (SICONFI)
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Realiza a extração automatizada e em lote dos dados do Relatório de Gestão Fiscal (RGF)
#' diretamente da API do SICONFI (Tesouro Nacional). A função resolve nativamente grades
#' paramétricas complexas (produto cartesiano de anos, entes, poderes e períodos), otimiza o
#' consumo de memória RAM e processa de forma determinística as colunas temporais móveis
#' (como a apuração de meses de referência no Anexo 01).
#'
#' @param cod.ibge Código IBGE do Ente. Vetor numérico ou caractere contendo os códigos dos entes.
#'   Aceita códigos de 1 dígito (União), 2 dígitos (Estados) ou 7 dígitos (Municípios).
#'   Para obter todas as UF's de forma combinada utilize o atalho `"all_states"`. Para obter todos
#'   os municípios utilize `"all_muni"`. Se `simplified = TRUE`, selecione municípios compatíveis.
#' @param year Exercício do relatório. Vetor numérico contendo os anos desejados para a análise (ex: `2022:2024`).
#' @param power Código do poder. Vetor de caracteres indicando os poderes pretendidos.
#'   Valores disponíveis: `E` = Executivo, `L` = Legislativo, `J` = Judiciário, `M` = Ministério Público, `D` = Defensoria Pública.
#' @param period Quadrimestre ou semestre de referência do relatório dentro de um exercício.
#'   A periodicidade semestral é automaticamente selecionada se `simplified = TRUE` (valores aceitos: 1 ou 2).
#'   A periodicidade padrão do relatório é quadrimestral (valores aceitos: 1, 2 ou 3).
#' @param annex Anexos dos demonstrativos do RGF. Valores disponíveis: `1`, `2`, `3`, `4`, `5` ou `6`.
#' @param simplified Tipo do Demonstrativo. RGF Simplificado aplica-se apenas aos municípios com menos
#'   de 50 mil habitantes que optaram pela publicação semestral dos relatórios. Se `TRUE`, a periodicidade
#'   semestral será automaticamente assumida e o período 3 passará a ser inválido. Padrão é `FALSE`.
#' @param save_path Caminho de arquivo opcional em disco (caractere) para persistência imediata dos dados.
#'   Se a extensão informada for `.parquet`, o pacote exige a instalação do pacote `arrow`. Também aceita
#'   extensões `.rds` e `.csv`. Se omitido (`NULL`), os dados são retornados diretamente para a sessão do R.
#'
#' @return Um objeto `data.frame` (ou um objeto invisível caso `save_path` seja acionado) contendo
#'   as colunas padronizadas da API do SICONFI empilhadas verticalmente de forma limpa, com as colunas
#'   do tipo `<MR-X>` devidamente traduzidas para o formato de data real `mes/ano`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. Extração básica: Anexo 01 de Goiás (IBGE 52) para o 3º quadrimestre de 2023, todos os poderes
#' dados_rgf_go <- RGFdata(
#'   cod.ibge = 52,
#'   year = 2023,
#'   power = c("E", "L", "J", "D", "M"),
#'   period = 3,
#'   annex = 1,
#'   simplified = FALSE
#' )
#'
#' # 2. Extração combinada multivariada em lote
#' # Baixa dados do Executivo e Legislativo para os anos 2022 e 2023 em múltiplos quadrimestres
#' dados_rgf_multi <- RGFdata(
#'   cod.ibge = c(5208707, 5201405),
#'   year = 2022:2023,
#'   power = c("E", "L"),
#'   period = c(1, 2),
#'   annex = 1
#' )
#'
#' # 3. Ingestão para Data Lake local salvando diretamente em formato Parquet
#' RGFdata(
#'   cod.ibge = "all_states",
#'   year = 2024,
#'   power = "E",
#'   period = 1:3,
#'   annex = 2,
#'   save_path = "data/storage/rgf_executivo_estados_2024.parquet"
#' )
#' }
RGFdata <- function(cod.ibge = NULL, year = NULL, power = NULL, period = NULL, annex = NULL, simplified = FALSE, save_path = NULL) {

  # 1. Tratamento de Atalhamento em Lote (Macros)
  if (length(cod.ibge) == 1 && cod.ibge == "all_states") {
    cod_ibge <- siconfi_list() |>
      janitor::clean_names() |>
      dplyr::filter(stringr::str_length(as.character(codigo_ibge)) == 2) |>
      dplyr::pull(codigo_ibge) |>
      as.numeric()
  } else if (length(cod.ibge) == 1 && cod.ibge == "all_muni") {
    cod_ibge <- siconfi_list() |>
      janitor::clean_names() |>
      dplyr::filter(stringr::str_length(as.character(codigo_ibge)) == 7) |>
      dplyr::pull(codigo_ibge) |>
      as.numeric()
  } else {
    cod_ibge <- as.character(cod.ibge)
  }

  chars_ibge <- stringr::str_length(cod_ibge)

  # 2. Cláusulas de Proteção de Escopo (Fail-Fast)
  if (any(chars_ibge %in% c(1, 2)) && isTRUE(simplified)) {
    cli::cli_abort(c(
      "x" = "The simplified publication only applies to municipalities with less than 50 thousand inhabitants.",
      "i" = "Not compatible with the `cod.ibge` provided (Union or State level entity detected).",
      "*" = "Run {.run RREORGFdataR::siconfi_list()} to see the list of valid municipalities."
    ))
  }
  if (any(chars_ibge == 7) && isTRUE(simplified) && (3 %in% period)) {
    cli::cli_abort(c(
      "x" = "Simplified RGF applies only to municipalities that have opted for biannual publication.",
      "i" = "Period 3 is invalid for simplified reports.",
      "*" = "Set `period` to 1 or 2."
    ))
  }

  # 3. Construção da Malha Paramétrica Combinada (Grid)
  param_grid <- tidyr::expand_grid(
    ano = year,
    periodo = period,
    poder = power,
    anexo = annex,
    ibge = cod_ibge
  )
  total_req <- nrow(param_grid)

  # 4. Interface Visual do Usuário (UX/CLI)
  cli::cli_h1("SICONFI Extraction: RGF")
  cli::cli_alert_info("Starting data extraction for {total_req} configuration{?s}...")

  pb <- cli::cli_progress_bar(
    format = "{cli::pb_spin} [{cli::pb_current}/{cli::pb_total}] Target Year: {.val {ano}} | IBGE: {.val {ibge}} | Power: {.val {poder}} | {cli::pb_percent} | ETA: {cli::pb_eta}",
    total = total_req,
    clear = FALSE
  )

  # 5. Pipeline Iterativo Alocado (Prevenção O(n^2))
  resultados <- vector("list", total_req)

  for (i in seq_len(total_req)) {
    p <- param_grid[i, ]
    ano <- p$ano; ibge <- p$ibge; poder <- p$poder # Escopo léxico lido pelo format do cli_progress_bar

    resultados[[i]] <- .fetch_siconfi_api(
      ano = ano, periodo = p$periodo, anexo = p$anexo,
      ibge = ibge, relatorio = "RGF", simplificado = simplified, poder = poder
    )

    cli::cli_progress_update(id = pb)
    Sys.sleep(0.3) # Intervalo de segurança contra rate-limits restritivos da API
  }

  cli::cli_progress_done(id = pb)

  # 6. Consolidação Estrutural
  final_df <- dplyr::bind_rows(resultados)

  if (nrow(final_df) == 0) {
    cli::cli_alert_warning("No data was returned. Please verify parameters or API availability status.")
    return(final_df)
  } else {
    cli::cli_alert_success("Extraction finished! {.val {nrow(final_df)}} database records obtained.")
  }

  # 7. Engine Opcional de Persistência em Disco (Parquet / RDS / CSV)
  if (!is.null(save_path)) {
    ext <- tolower(tools::file_ext(save_path))
    cli::cli_progress_step("Writing file to storage using format: {.val {ext}}...")

    if (ext == "parquet") {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        cli::cli_abort(c(
          "x" = "The {.pkg arrow} package is strictly required to read and write Parquet files.",
          "i" = "Please execute {.run install.packages('arrow')} and restart your R session."
        ))
      }
      arrow::write_parquet(final_df, save_path)

    } else if (ext == "rds") {
      saveRDS(final_df, save_path)

    } else if (ext == "csv") {
      utils::write.csv(final_df, save_path, row.names = FALSE, fileEncoding = "UTF-8")

    } else {
      cli::cli_alert_warning("File extension format {.val {ext}} is not supported. Data returned strictly in-memory.")
      return(final_df)
    }

    cli::cli_alert_success("File successfully persisted at path: {.path {save_path}}")
    return(invisible(final_df))
  }

  return(final_df)
}
