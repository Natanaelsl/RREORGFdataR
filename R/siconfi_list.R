#' siconfi_list: Lista de instituições e respectivos códigos
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#'
#' Retorna informações sobre os códigos do IBGE e do Siconfi e as respectivas instituições compatíveis com a API do SICONFI.
#'
#' @param options A opção padrão `NULL` retorna um `data.frame` contendo os códigos do IBGE e do Siconfi disponíveis para a API, juntamente com as instituições correspondentes. A escolha da opção `options = down` permite o download do arquivo (.pdf) disponibilizado pelo Tesouro Nacional.
#'
#' @export
#' @examples
#' # install.packages("devtools")
#' # devtools::install_github("Natanaelsl/RREORGFdataR")
#'
#' # Carregando o pacote
#' # library(RREORGFdataR)
#'
#' # Reportando `data.frame` com os códigos disponibilizados pelo Tesouro Nacional
#' # siconfi_list()
#'
#' # Baixando arquivo (.pdf) disponibilizado pelo Tesouro Nacional
#' # apresentando lista com os códigos
#' # siconfi_list(options == "down")
siconfi_list <- function(options = NULL){

  # Siconfi_table <- "Cod_instituicoes_siconfi.pdf"
  # Siconfi_data <- tabulizer::extract_tables(Siconfi_table)
  #
  # Siconfi_data <- do.call(rbind, lapply(Siconfi_data, as.data.frame)) %>%
  #   stats::setNames(.[1,]) %>%
  #   dplyr::slice(-1)
  #
  # saveRDS(Siconfi_data, file = "Cod_instituicoes_siconfi.rds")

  # https://raw.githubusercontent.com/Natanaelsl/RREORGFdataR/6e9381012ccb1afad53c80447d56f228f751319e/Cod_instituicoes_siconfi.pdf

  # if(!is.null(options) | options != "down") {
  #
  #   cli::cli_alert_warning("opção inválida!")
  #
  # }

  if(is.null(options)) {

    Siconfi_data <<- readRDS("Cod_instituicoes_siconfi.rds")

  }

  if(options == "down") {

    url <- "https://siconfi.tesouro.gov.br/siconfi/pages/public/arquivo/conteudo/Cod_instituicoes_siconfi.pdf"

    # Nome do arquivo e extensão
    filename <- "Cod_instituicoes_siconfi.pdf"

    # Caminho completo para o arquivo a ser baixado
    caminho_arquivo <- file.path(utils::choose.dir(), filename)

    caminho_arquivo <- gsub("/", "\\\\", caminho_arquivo)

    # Verificar se o arquivo foi baixado com sucesso
    utils::download.file(url, destfile = caminho_arquivo, mode = "wb")
    cli::cli_alert_info("Arquivo PDF baixado e salvo com sucesso em: \n")
    cat(caminho_arquivo)

  }

}
