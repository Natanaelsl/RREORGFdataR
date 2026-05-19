#' @keywords internal
#'
#' @title RREORGFdataR: Extração intuitiva e fácil dos dados do RREO e RGF utilizando API do SICONFI.
#'
#' @description
#' \if{html}{\figure{logo.png}{options: style='float: right' alt='Logo_1' width='120'}}
#' O 'RREORGFdataR' é um pacote R que permite aos usuários acessar facilmente o conjunto de dados
#' do Relatório Resumido da Execução Orçamentária (RREO) e do Relatório de Gestão Fiscal (RGF)
#' utilizando a API do SICONFI. O pacote foca em alta performance e automação de extração
#' para analistas da área fiscal.
#'
#' @details
#' O pacote abstrai a complexidade da API do Tesouro Nacional, implementando funções para extração
#' em lote, tratamento de erros com retry automático e estruturação otimizada para análise em R.
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://natanaelsl.com.br/project/rreorgf_pkg/}
#'   \item \url{https://natanaelsl.github.io/RREORGFdataR/}
#'   \item Report bugs at \url{https://github.com/Natanaelsl/RREORGFdataR/issues}
#' }
#'
#' @author
#' Natanael Soares Leite \email{natanael.leite@goias.gov.br}
#' Raphael Maciel de Lima
#'
#' @importFrom lifecycle deprecated
#' @importFrom httr GET content status_code timeout
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
"_PACKAGE"

# Declaração global para evitar NOTEs no R CMD check
utils::globalVariables(c("codigo_ibge"))
