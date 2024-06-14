#' RGFdata: Extração intuitiva e fácil dos dados do RREO
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#'
#' Realiza a extração dos dados do Relatório Resumido de Execução Orçamentária (RREO) de maneira mais intuitiva e fácil utilizando a API do [SICONFI](https://apidatalake.tesouro.gov.br/docs/siconfi/).
#'
#' @param cod.ibge Código IBGE do Ente. se `simplified = TRUE`, então selecione municípios.
#' @param year Exercício do relatório
#' @param period Quadrimestre ou semestre de referência do relatório dentro de um exercício. A periodicidade semestral é automaticamente selecionada se `simplified = TRUE`.  A periodicidade padrão do relatório é quadrimestral. Valores disponíveis: 1, 2 ou 3.
#' @param annex Anexos dos demonstrativos do RREO. Valores disponíveis: 1, 2, 3, 4, 5 ou 6.
#' @param simplified Tipo do Demonstrativo. RREO Simplificado aplica-se apenas aos municípios com menos de 50 mil habitantes que optaram pela publicação semestral dos relatórios. Se `TRUE` a periodicidade semestral será automaticamente selecionada.
#'
#'
#' @export
#' @examples
#' # install.packages("devtools")
#' # devtools::install_github("Natanaelsl/RREORGFdataR")
#'
#' # Carregando o pacote
#' library(RREORGFdataR)
#'
#' # Extraindo dados do anexo 1, 6º bimestre
#' # do RREO de 2020 até 2023 do Estado de Goiás.
#' df.RREO <- RREOdata(cod.ibge = 52,
#'                     year = c(2020:2023),
#'                     period = 6,
#'                     annex = 1,
#'                     simplified = FALSE)
#'
RREOdata <- function(cod.ibge = NULL, year = NULL, period = NULL, annex = NULL, simplified = FALSE) {
  options(error = NULL)
  #    Functions
  check_cod <- function(cod.ibge , year , period , annex, simplified ) {

    if (all(nchar(cod.ibge) == 1 ) & simplified == FALSE) {
      cod.ibge  <- '01'
      esfera <- "U"
      #print('União')

    } else if (all(nchar(cod.ibge) == 1 ) & simplified == TRUE) {
      cod.ibge  <- '01'
      esfera <- "U"
      #print('União')
      cli::cli_alert_danger("The simplified publication only applies to municipalities
                             with less than 50 thousand inhabitants. Not compatible with
                             the cod.ibge provided.")

    } else if (all(nchar(cod.ibge) == 2 ) & simplified == TRUE) {
      cod.ibge  <- as.character(cod.ibge)
      esfera <- "E"
      #print('estado')
      cli::cli_alert_danger("The simplified publication only applies to municipalities
                             with less than 50 thousand inhabitants. Not compatible with
                             the cod.ibge provided.")

    }else if (all(nchar(cod.ibge) == 2) & simplified == FALSE) {
      cod.ibge  <- as.character(cod.ibge)
      esfera <- "E"
      #print('estado')


    } else if (all(nchar(cod.ibge) == 7)) {
      esfera <- "M"
      #print('municipio')

    } else {
      cli::cli_alert_danger("You are selecting several Entities.
                          Select '1' for Union
                          or a two-digit list for States
                          or a seven-digit list for municipalities.")
    }
    return(esfera)
  }
  esfera <- check_cod(cod.ibge , year , period , annex, simplified )

  base_url_rreo <- "https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rreo?"

  rreo_df <- data.frame(stringsAsFactors = FALSE)


  for (year in seq(from = min(year), to = max(year))) {

    cli::cli_progress_step(paste("EXTRACTING", year), spinner = TRUE)
    cli::cli_progress_step("cod. IBGE | 01", spinner = TRUE, msg_done = "Finished!")

    Sys.sleep(1)
    exercicio <- year
    tempo <- as.character(period)
    tipo_relatorio <- "RREO"

    num_anexo <- ifelse(annex < 10,
                        glue::glue("RREO-Anexo%200",
                                   annex),
                        glue::glue("RREO-Anexo%20",
                                   annex))


    ente <- as.character(cod.ibge)

    chamada_api_rreo <- paste(base_url_rreo,
                              "an_exercicio=", exercicio, "&",
                              "nr_periodo=", tempo, "&",
                              "co_tipo_demonstrativo=", tipo_relatorio, "&",
                              "no_anexo=", num_anexo, "&",
                              "co_esfera=", esfera , "&",
                              "id_ente=", ente, sep = "")
    #return(chamada_api_rreo)
    rreo <- httr::GET(chamada_api_rreo)
    httr::status_code(rreo)
    rreo_txt <- httr::content(rreo,
                              as = "text",
                              encoding = "UTF-8")

    rreo_json <- jsonlite::fromJSON(rreo_txt,
                                    flatten = FALSE)

    rreo_df1 <- as.data.frame(rreo_json[["items"]])
    rreo_df <- rbind(rreo_df, rreo_df1)
  }

  return(rreo_df)
}
