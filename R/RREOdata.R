#' Extração Inteligente e Vetorizada de Dados do RREO (SICONFI)
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Realiza a extração automatizada e em lote dos dados do Relatório Resumido de
#' Execução Orçamentária (RREO) Diretamente da API do SICONFI (Tesouro Nacional).
#' A função resolve nativamente grades paramétricas complexas (produto cartesiano),
#' otimiza o consumo de memória RAM e processa de forma determinística as colunas
#' temporais móveis (como os últimos 12 meses do Anexo 03).
#'
#' @param cod.ibge Vetor numérico ou caractere contendo os códigos IBGE dos entes federativos.
#'   Aceita códigos de 1 dígito (União), 2 dígitos (Estados) ou 7 dígitos (Municípios).
#'   Pode receber os atalhos estendidos `"all_states"` para extrair todas as Unidades da
#'   Federação de forma combinada ou `"all_muni"` para todos os municípios brasileiros.
#' @param year Vetor numérico contendo os anos/exercícios desejados para a análise (ex: `2022:2024`).
#' @param period Vetor numérico contendo os bimestres de referência do relatório dentro de um exercício.
#'   Valores típicos disponíveis: `1` a `6`. Caso `simplified = TRUE`, a periodicidade passa a
#'   ser semestral, aceitando os valores `1` ou `2`.
#' @param annex Vetor numérico mapeando os anexos dos demonstrativos do RREO a serem baixados
#'   (valores aceitos de `1` a `14` conforme disponibilidade na API do SICONFI).
#' @param simplified Lógico. Define se a consulta deve buscar a versão do RREO Simplificado.
#'   Esta opção aplica-se estritamente a municípios com menos de 50 mil habitantes que optaram
#'   formalmente pela publicação semestral dos seus relatórios fiscais. Padrão é `FALSE`.
#' @param save_path Caminho de arquivo opcional em disco (caractere) para persistência imediata
#'   dos dados consolidados. Se a extensão informada for `.parquet`, o pacote exige a instalação
#'   do pacote `arrow`. Também aceita extensões `.rds` e `.csv`. Se omitido (`NULL`), os dados
#'   são retornados diretamente para a sessão do R.
#'
#' @return Um objeto `data.frame` (ou um objeto invisível caso `save_path` seja acionado) contendo
#'   as colunas padronizadas da API do SICONFI empilhadas verticalmente de forma limpa, com as
#'   colunas do tipo `<MR-X>` devidamente traduzidas para o formato de data real `mes/ano`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. Extração básica: Anexo 01 do Estado de Goiás (IBGE 52) para o 6º Bimestre de 2023
#' dados_goias <- RREOdata(
#'   cod.ibge = 52,
#'   year = 2023,
#'   period = 6,
#'   annex = 1
#' )
#'
#' # 2. Extração combinada multivariada (Gera produto cartesiano interno)
#' # Baixa dados de Goiânia (5208707) e Aparecida (5201405), anos 2022 e 2023, bimestres 1 e 6
#' dados_multi <- RREOdata(
#'   cod.ibge = c(5208707, 5201405),
#'   year = 2022:2023,
#'   period = c(1, 6),
#'   annex = 3
#' )
#'
#' # 3. Pipeline de Data Lake: Baixando dados em lote e salvando direto em Parquet
#' # Nota: Requer que o pacote 'arrow' esteja instalado no ambiente.
#' RREOdata(
#'   cod.ibge = "all_states",
#'   year = 2024,
#'   period = 1:6,
#'   annex = 1,
#'   save_path = "data/storage/rreo_estados_2024.parquet"
#' )
#'
#' # 4. Fluxo de leitura local após salvar (Lazy Loading)
#' # gc() # Chama o garbage collector para limpar a RAM
#' # tab.parquet <- arrow::open_dataset("data/storage/rreo_estados_2024.parquet")
#' # tab.parquet |>
#' #  filter(cod_ibge == "52") |>
#' #  collect()
#' }
RREOdata <- function(cod.ibge = NULL, year = NULL, period = NULL, annex = NULL, simplified = FALSE, save_path = NULL) {

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
      "i" = "Not compatible with the `cod.ibge` provided (Union or State level entity detected)."
    ))
  }
  if (!all(chars_ibge %in% c(1, 2, 7))) {
    cli::cli_abort("Invalid `cod.ibge`. Select '1' for Union, 2 digits for States, or 7 digits for Municipalities.")
  }

  # 3. Construção da Malha Paramétrica Combinada (Grid)
  param_grid <- tidyr::expand_grid(
    ano = year,
    periodo = period,
    anexo = annex,
    ibge = cod_ibge
  )
  total_req <- nrow(param_grid)

  # 4. Interface Visual do Usuário (UX/CLI)
  cli::cli_h1("SICONFI Extraction: RREO")
  cli::cli_alert_info("Starting data extraction for {total_req} configuration{?s}...")

  pb <- cli::cli_progress_bar(
    format = "{cli::pb_spin} [{cli::pb_current}/{cli::pb_total}] Target Year: {.val {ano}} | IBGE: {.val {ibge}} | {cli::pb_percent} | ETA: {cli::pb_eta}",
    total = total_req,
    clear = FALSE
  )

  # 5. Pipeline Iterativo Alocado (Prevenção O(n^2))
  resultados <- vector("list", total_req)

  for (i in seq_len(total_req)) {
    p <- param_grid[i, ]
    ano <- p$ano; ibge <- p$ibge # Mutação léxica interna capturada pela string de formatação do cli

    resultados[[i]] <- .fetch_siconfi_api(
      ano = ano, periodo = p$periodo, anexo = p$anexo,
      ibge = ibge, relatorio = "RREO", simplificado = simplified, poder = NULL
    )

    cli::cli_progress_update(id = pb)
    Sys.sleep(0.3) # Intervalo de segurança para conformidade de taxa da API
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

    # 7.1 Garante que a árvore de pastas exista antes de salvar
    dir_path <- dirname(save_path)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      cli::cli_alert_info("Created missing directory structure: {.path {dir_path}}")
    }

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
