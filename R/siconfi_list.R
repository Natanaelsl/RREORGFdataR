#' Extração da Lista de Instituições e Códigos SICONFI
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Retorna o dicionário de dados contendo os códigos do IBGE e do SICONFI das
#' instituições federativas compatíveis com a API do Tesouro Nacional. Permite
#' visualizar a base empacotada em formato `data.frame` ou realizar o download
#' do arquivo oficial em PDF diretamente do site do Tesouro.
#'
#' @param action Caractere definindo a ação da função. Aceita `"view"` (padrão) para
#'   carregar e retornar o `data.frame` contendo a base de dados interna do pacote,
#'   ou `"download"` para baixar o arquivo PDF oficial do Tesouro Nacional.
#' @param dest_dir Caminho de diretório (caractere) indicando onde o arquivo PDF
#'   deve ser salvo caso `action = "download"`. O padrão é o diretório de trabalho
#'   atual (`getwd()`). Ignorado se `action = "view"`.
#'
#' @return Se `action = "view"`, retorna um objeto `data.frame` com os códigos.
#'   Se `action = "download"`, salva o arquivo `.pdf` no diretório especificado e
#'   retorna o caminho completo do arquivo invisivelmente (`invisible()`).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # 1. Visualizar o dicionário de códigos no console (retorna o data.frame)
#' df_codigos <- siconfi_list(action = "view")
#' head(df_codigos)
#'
#' # 2. Baixar o arquivo PDF oficial para a pasta atual (Working Directory)
#' siconfi_list(action = "download")
#'
#' # 3. Baixar o PDF para uma pasta específica do projeto (Cross-platform)
#' siconfi_list(
#'   action = "download",
#'   dest_dir = "C:/meus_dados/referencias"
#' )
#' }
siconfi_list <- function(action = c("view", "download"), dest_dir = getwd()) {

  # Garante que o usuário digitou uma opção válida (se não digitar, assume "view")
  action <- match.arg(action)

  if (action == "view") {

    # Localiza o arquivo embutido na instalação do pacote
    data_path <- system.file("extdata", "Cod_instituicoes_siconfi.rds", package = "RREORGFdataR")

    # Fail-fast se o arquivo não for encontrado na compilação
    if (data_path == "") {
      cli::cli_abort(c(
        "x" = "O arquivo RDS da base de c\u00f3digos n\u00e3o foi encontrado na instala\u00e7\u00e3o do pacote.",
        "i" = "Verifique se a pasta {.path inst/extdata/} cont\u00e9m o arquivo {.file Cod_instituicoes_siconfi.rds}."
      ))
    }

    return(readRDS(data_path))

  } else if (action == "download") {

    url <- "https://siconfi.tesouro.gov.br/siconfi/pages/public/arquivo/conteudo/Cod_instituicoes_siconfi.pdf"

    # Normaliza o caminho para garantir compatibilidade estrutural (Windows, macOS, Linux)
    caminho_arquivo <- normalizePath(file.path(dest_dir, "Cod_instituicoes_siconfi.pdf"), mustWork = FALSE)

    # Inicia o step e salva o ID na memória
    step_id <- cli::cli_progress_step("Baixando arquivo PDF do Tesouro Nacional...")

    tryCatch({
      utils::download.file(url, destfile = caminho_arquivo, mode = "wb", quiet = TRUE)

      # 1. Encerra o spinner de progresso explicitamente com sucesso
      cli::cli_progress_done(step_id)

      # 2. Só então, emite o alerta final com o caminho
      cli::cli_alert_success("Arquivo salvo com sucesso em: {.path {caminho_arquivo}}")

    }, error = function(e) {

      # Em caso de erro, atualiza o status do spinner para falha
      cli::cli_progress_done(step_id, result = "failed")
      cli::cli_abort(c(
        "x" = "Falha ao baixar o arquivo do Tesouro Nacional.",
        "i" = "Detalhe do erro: {e$message}"
      ))

    })

    return(invisible(caminho_arquivo))
  }
}
