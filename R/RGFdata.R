#' RGFdata: Extração intuitiva e fácil dos dados do RGF
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#'
#' Realiza a extração dos dados do Relatório de Gestão Fiscal (RGF) de maneira mais intuitiva e fácil utilizando a API do [SICONFI](https://apidatalake.tesouro.gov.br/docs/siconfi/).
#'
#' @param cod.ibge Código IBGE do Ente. se `simplificado = TRUE`, então selecione municípios.
#' @param ano Exercício do relatório
#' @param poder Código do poder. Valores disponíveis: E = Executivo, L = Legislativo, J = Judiciário, M = Ministério Público, D = Defensoria Pública
#' @param periodo Quadrimestre ou semestre de referência do relatório dentro de um exercício. A periodicidade semestral é automaticamente selecionada se `simplificado = TRUE`.  A periodicidade padrão do relatório é quadrimestral. Valores disponíveis: 1, 2 ou 3.
#' @param anexo Anexos dos demonstrativos do RGF. Valores disponíveis: 1, 2, 3, 4, 5 ou 6.
#' @param simplificado Tipo do Demonstrativo. RGF Simplificado aplica-se apenas aos municípios com menos de 50 mil habitantes que optaram pela publicação semestral dos relatórios. Se `TRUE` a periodicidade semestral será automaticamente selecionada.
#'
#'
#' @export
#' @examples
#' # install.packages("devtools")
#' devtools::install_github("Natanaelsl/RREORGFdataR")
#'
#' # Carregando o pacote
#' library(RREORGFdataR)
#'
#' # Extraindo dados do anexo 1 para o 3º quadrimestre
#' # do RGF de 2020 até 2023 do Estado de Goiás para todos os poderes.
#' RGFdata(cod.ibge = 52,
#'         ano = c(2020:2023),
#'         poder = c('E','L','J','D','M'),
#'         periodo = 3,
#'         anexo = 1,
#'         simplificado = FALSE)
RGFdata <- function(cod.ibge = NULL, ano = NULL, poder = NULL, periodo = NULL, anexo = NULL, simplificado = FALSE){


  base_url_rgf <- "https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rgf?"


  rgf_df <- data.frame(stringsAsFactors = FALSE)

  # MENSAGENS DE ERRO
  if(all(nchar(cod.ibge) == 1) & {simplificado} == TRUE ) {
    cli::cli_alert_danger("The simplified publication only applies to municipalities with less than 50 thousand inhabitants. Not compatible with the `cod.ibge` provided.")
  }

  if(all(nchar(cod.ibge) == 2) & isTRUE(simplificado) ) {
    cli::cli_alert_danger("The simplified publication only applies to municipalities with less than 50 thousand inhabitants. Not compatible with the `cod.ibge` provided.")
  }

  if(all(nchar(cod.ibge) == 7) & isTRUE(simplificado) & (3 %in% periodo)) {
    cli::cli_alert_danger("Simplified RGF applies only to municipalities with less than 50 thousand inhabitants that have opted for biannual publication of reports. Set `periodo` to 1 or 2.")
  }

  # DEFININDO ESTRUTURA PARA UNIÃO
  if(cod.ibge == 1 & (simplificado == FALSE) | is.null(simplificado) ) {

    for(z in 1:length(ano)) {
      cli::cli_progress_step("EXTRACTING {ano[[z]]}", spinner = TRUE)


      step0 <- " "
      cli::cli_progress_step("cod. IBGE | {step0}", spinner = TRUE, msg_done = "Finished!")
      for (j in 1:length(cod.ibge)) {
        Sys.sleep(.1)
        step0 <- glue::glue("{cod.ibge[[j]]}")
        cli::cli_progress_update(set = j)


        for (i in 1:length(poder)) {


          for (w in 1:length(periodo)) {

            exercicio <- ano[[z]]
            periodicidade = "Q"
            tempo <- periodo[[w]]
            tipo_relatorio <- "RGF"
            num_anexo <- glue::glue("RGF-Anexo%200",{{anexo}})
            esfera <- "U"
            cod_poder <- poder[[i]]
            ente <- cod.ibge[[j]]

            # montar a chamada à API
            chamada_api_rgf <- paste(base_url_rgf,
                                     "an_exercicio=", exercicio, "&",
                                     "in_periodicidade=", periodicidade,"&",
                                     "nr_periodo=", tempo, "&",
                                     "co_tipo_demonstrativo=", tipo_relatorio, "&",
                                     "no_anexo=", num_anexo, "&",
                                     "co_esfera=", esfera , "&",
                                     "co_poder=", cod_poder, "&",
                                     "id_ente=", ente, sep = ""
            )


            rgf <- httr::GET(chamada_api_rgf, config = httr::config(connecttimeout = 60))

            httr::status_code(rgf)

            rgf_txt <- httr::content(rgf, as="text", encoding="UTF-8")

            rgf_json <- jsonlite::fromJSON(rgf_txt, flatten = FALSE)

            rgf_df1 <- as.data.frame(rgf_json[["items"]])

            rgf_df <-  rbind(rgf_df, rgf_df1)

          }
        }
      }
    }

    if({{num_anexo}} == "RGF-Anexo%2001"){
      rgf_df0 <- rgf_df %>%
        dplyr::mutate(mes_nome = dplyr::case_when(periodo == 3 & coluna == '<MR-11>'~ glue::glue('jan/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-11>'~ glue::glue('set/{exercicio - 1}'),
                                                  periodo == 1 & coluna == '<MR-11>'~ glue::glue('mai/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-10>'~ glue::glue('fev/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-10>'~ glue::glue('out/{exercicio - 1}'),
                                                  periodo == 1 & coluna == '<MR-10>'~ glue::glue('jun/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-9>'~ glue::glue('mar/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-9>'~ glue::glue('nov/{exercicio - 1}'),
                                                  periodo == 1 & coluna == '<MR-9>'~ glue::glue('jul/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-8>'~ glue::glue('abr/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-8>'~ glue::glue('dez/{exercicio - 1}'),
                                                  periodo == 1 & coluna == '<MR-8>'~ glue::glue('ago/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-7>'~ glue::glue('mai/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-7>'~ glue::glue('jan/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-7>'~ glue::glue('set/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-6>'~ glue::glue('jun/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-6>'~ glue::glue('fev/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-6>'~ glue::glue('out/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-5>'~ glue::glue('jul/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-5>'~ glue::glue('mar/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-5>'~ glue::glue('nov/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-4>'~ glue::glue('ago/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-4>'~ glue::glue('abr/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-4>'~ glue::glue('dez/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-3>'~ glue::glue('set/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-3>'~ glue::glue('mai/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-3>'~ glue::glue('jan/{exercicio}'),
                                                  periodo == 3 & coluna == '<MR-2>'~ glue::glue('out/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-2>'~ glue::glue('jun/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-2>'~ glue::glue('fev/{exercicio}'),
                                                  periodo == 3 & coluna == '<MR-1>'~ glue::glue('nov/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-1>'~ glue::glue('jul/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-1>'~ glue::glue('mar/{exercicio}'),
                                                  periodo == 3 & coluna == '<MR>'~ glue::glue('dez/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR>'~ glue::glue('ago/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR>'~ glue::glue('abr/{exercicio}'),
                                                  TRUE ~ coluna),
                      mes = dplyr::case_when(periodo == 3 & coluna == '<MR-11>'~1,
                                             periodo == 2 & coluna == '<MR-11>'~9,
                                             periodo == 1 & coluna == '<MR-11>'~5,
                                             periodo == 3 & coluna == '<MR-10>'~2,
                                             periodo == 2 & coluna == '<MR-10>'~10,
                                             periodo == 1 & coluna == '<MR-10>'~6,
                                             periodo == 3 & coluna == '<MR-9>'~3,
                                             periodo == 2 & coluna == '<MR-9>'~11,
                                             periodo == 1 & coluna == '<MR-9>'~7,
                                             periodo == 3 & coluna == '<MR-8>'~4,
                                             periodo == 2 & coluna == '<MR-8>'~12,
                                             periodo == 1 & coluna == '<MR-8>'~8,
                                             periodo == 3 & coluna == '<MR-7>'~5,
                                             periodo == 2 & coluna == '<MR-7>'~1,
                                             periodo == 1 & coluna == '<MR-7>'~9,
                                             periodo == 3 & coluna == '<MR-6>'~6,
                                             periodo == 2 & coluna == '<MR-6>'~2,
                                             periodo == 1 & coluna == '<MR-6>'~10,
                                             periodo == 3 & coluna == '<MR-5>'~7,
                                             periodo == 2 & coluna == '<MR-5>'~3,
                                             periodo == 1 & coluna == '<MR-5>'~11,
                                             periodo == 3 & coluna == '<MR-4>'~8,
                                             periodo == 2 & coluna == '<MR-4>'~4,
                                             periodo == 1 & coluna == '<MR-4>'~12,
                                             periodo == 3 & coluna == '<MR-3>'~9,
                                             periodo == 2 & coluna == '<MR-3>'~5,
                                             periodo == 1 & coluna == '<MR-3>'~1,
                                             periodo == 3 & coluna == '<MR-2>'~10,
                                             periodo == 2 & coluna == '<MR-2>'~6,
                                             periodo == 1 & coluna == '<MR-2>'~2,
                                             periodo == 3 & coluna == '<MR-1>'~11,
                                             periodo == 2 & coluna == '<MR-1>'~7,
                                             periodo == 1 & coluna == '<MR-1>'~3,
                                             periodo == 3 & coluna == '<MR>'~12,
                                             periodo == 2 & coluna == '<MR>'~8,
                                             periodo == 1 & coluna == '<MR>'~4),
                      valor = as.numeric(rgf_df$valor)
        )

      # assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      # rm(rgf_df0)
      return(rgf_df0)

    } else {

      rgf_df0 <- rgf_df
      # assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      # rm(rgf_df0)
      return(rgf_df0)

    }

    cli::cli_progress_done()

  }

  # DEFININDO ESTRUTURA PARA ESTADOS
  if(all(nchar(cod.ibge) == 2) & (simplificado == FALSE) | is.null(simplificado) ) {

    for(z in 1:length(ano)) {
      cli::cli_progress_step("EXTRACTING {ano[[z]]}", spinner = TRUE)


      step1 <- " "
      cli::cli_progress_step("cod. IBGE | {step1}", spinner = TRUE, msg_done = "Finished!")
      for (j in 1:length(cod.ibge)) {
        Sys.sleep(.1)
        step1 <- glue::glue("{cod.ibge[[j]]}")
        cli::cli_progress_update(set = j)


        for (i in 1:length(poder)) {


          for (w in 1:length(periodo)) {

            exercicio <- ano[[z]]
            periodicidade = "Q"
            tempo <- periodo[[w]]
            tipo_relatorio <- "RGF"
            num_anexo <- glue::glue("RGF-Anexo%200",{{anexo}})
            esfera <- "E"
            cod_poder <- poder[[i]]
            ente <- cod.ibge[[j]]

            # montar a chamada à API
            chamada_api_rgf <- paste(base_url_rgf,
                                     "an_exercicio=", exercicio, "&",
                                     "in_periodicidade=", periodicidade,"&",
                                     "nr_periodo=", tempo, "&",
                                     "co_tipo_demonstrativo=", tipo_relatorio, "&",
                                     "no_anexo=", num_anexo, "&",
                                     "co_esfera=", esfera , "&",
                                     "co_poder=", cod_poder, "&",
                                     "id_ente=", ente, sep = ""
            )



            rgf <- httr::GET(chamada_api_rgf, config = httr::config(connecttimeout = 60))

            httr::status_code(rgf)

            rgf_txt <- httr::content(rgf, as="text", encoding="UTF-8")

            rgf_json <- jsonlite::fromJSON(rgf_txt, flatten = FALSE)

            rgf_df1 <- as.data.frame(rgf_json[["items"]])

            rgf_df <-  rbind(rgf_df, rgf_df1)

          }
        }
      }
    }

    if({{num_anexo}} == "RGF-Anexo%2001"){
      rgf_df0 <- rgf_df %>%
        dplyr::mutate(mes_nome = dplyr::case_when(periodo == 3 & coluna == '<MR-11>'~ glue::glue('jan/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-11>'~ glue::glue('set/{exercicio - 1}'),
                                                  periodo == 1 & coluna == '<MR-11>'~ glue::glue('mai/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-10>'~ glue::glue('fev/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-10>'~ glue::glue('out/{exercicio - 1}'),
                                                  periodo == 1 & coluna == '<MR-10>'~ glue::glue('jun/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-9>'~ glue::glue('mar/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-9>'~ glue::glue('nov/{exercicio - 1}'),
                                                  periodo == 1 & coluna == '<MR-9>'~ glue::glue('jul/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-8>'~ glue::glue('abr/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-8>'~ glue::glue('dez/{exercicio - 1}'),
                                                  periodo == 1 & coluna == '<MR-8>'~ glue::glue('ago/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-7>'~ glue::glue('mai/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-7>'~ glue::glue('jan/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-7>'~ glue::glue('set/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-6>'~ glue::glue('jun/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-6>'~ glue::glue('fev/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-6>'~ glue::glue('out/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-5>'~ glue::glue('jul/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-5>'~ glue::glue('mar/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-5>'~ glue::glue('nov/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-4>'~ glue::glue('ago/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-4>'~ glue::glue('abr/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-4>'~ glue::glue('dez/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-3>'~ glue::glue('set/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-3>'~ glue::glue('mai/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-3>'~ glue::glue('jan/{exercicio}'),
                                                  periodo == 3 & coluna == '<MR-2>'~ glue::glue('out/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-2>'~ glue::glue('jun/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-2>'~ glue::glue('fev/{exercicio}'),
                                                  periodo == 3 & coluna == '<MR-1>'~ glue::glue('nov/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-1>'~ glue::glue('jul/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-1>'~ glue::glue('mar/{exercicio}'),
                                                  periodo == 3 & coluna == '<MR>'~ glue::glue('dez/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR>'~ glue::glue('ago/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR>'~ glue::glue('abr/{exercicio}'),
                                                  TRUE ~ coluna),
                      mes = dplyr::case_when(periodo == 3 & coluna == '<MR-11>'~1,
                                             periodo == 2 & coluna == '<MR-11>'~9,
                                             periodo == 1 & coluna == '<MR-11>'~5,
                                             periodo == 3 & coluna == '<MR-10>'~2,
                                             periodo == 2 & coluna == '<MR-10>'~10,
                                             periodo == 1 & coluna == '<MR-10>'~6,
                                             periodo == 3 & coluna == '<MR-9>'~3,
                                             periodo == 2 & coluna == '<MR-9>'~11,
                                             periodo == 1 & coluna == '<MR-9>'~7,
                                             periodo == 3 & coluna == '<MR-8>'~4,
                                             periodo == 2 & coluna == '<MR-8>'~12,
                                             periodo == 1 & coluna == '<MR-8>'~8,
                                             periodo == 3 & coluna == '<MR-7>'~5,
                                             periodo == 2 & coluna == '<MR-7>'~1,
                                             periodo == 1 & coluna == '<MR-7>'~9,
                                             periodo == 3 & coluna == '<MR-6>'~6,
                                             periodo == 2 & coluna == '<MR-6>'~2,
                                             periodo == 1 & coluna == '<MR-6>'~10,
                                             periodo == 3 & coluna == '<MR-5>'~7,
                                             periodo == 2 & coluna == '<MR-5>'~3,
                                             periodo == 1 & coluna == '<MR-5>'~11,
                                             periodo == 3 & coluna == '<MR-4>'~8,
                                             periodo == 2 & coluna == '<MR-4>'~4,
                                             periodo == 1 & coluna == '<MR-4>'~12,
                                             periodo == 3 & coluna == '<MR-3>'~9,
                                             periodo == 2 & coluna == '<MR-3>'~5,
                                             periodo == 1 & coluna == '<MR-3>'~1,
                                             periodo == 3 & coluna == '<MR-2>'~10,
                                             periodo == 2 & coluna == '<MR-2>'~6,
                                             periodo == 1 & coluna == '<MR-2>'~2,
                                             periodo == 3 & coluna == '<MR-1>'~11,
                                             periodo == 2 & coluna == '<MR-1>'~7,
                                             periodo == 1 & coluna == '<MR-1>'~3,
                                             periodo == 3 & coluna == '<MR>'~12,
                                             periodo == 2 & coluna == '<MR>'~8,
                                             periodo == 1 & coluna == '<MR>'~4),
                      valor = as.numeric(rgf_df$valor)
        )

      # assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      # rm(rgf_df0)
      return(rgf_df0)

    } else {

      rgf_df0 <- rgf_df
      # assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      # rm(rgf_df0)
      return(rgf_df0)

    }

    cli::cli_progress_done()

  }

  # DEFININDO ESTRUTURA PARA MUNICÍPIOS
  if((all(nchar(cod.ibge) == 7)) & ((isTRUE(simplificado) & !(3 %in% periodo)) | is.null(simplificado) | simplificado == FALSE) ) {


      for(z in 1:length(ano)) {
        cli::cli_progress_step("EXTRACTING {ano[[z]]}", spinner = TRUE)


        step2 <- " "
        cli::cli_progress_step("cod. IBGE | {step2}", spinner = TRUE, msg_done = "Finished!")
        for (j in 1:length(cod.ibge)) {
          Sys.sleep(.1)
          step2 <- glue::glue("{cod.ibge[[j]]}")
          cli::cli_progress_update(set = j)


          for (i in 1:length(poder)) {


            for (w in 1:length(periodo)) {

              exercicio <- ano[[z]]
              tempo <- periodo[[w]]

              if({simplificado} == TRUE) {
                tipo_relatorio <- "RGF+Simplificado"
                periodicidade = "S"
              } else {
                tipo_relatorio <- "RGF"
                periodicidade = "Q"
              }
              num_anexo <- glue::glue("RGF-Anexo%200",{{anexo}})
              esfera <- "M"
              cod_poder <- poder[[i]]
              ente <- cod.ibge[[j]]

              # montar a chamada à API
              chamada_api_rgf <- paste(base_url_rgf,
                                       "an_exercicio=", exercicio, "&",
                                       "in_periodicidade=", periodicidade,"&",
                                       "nr_periodo=", tempo, "&",
                                       "co_tipo_demonstrativo=", tipo_relatorio, "&",
                                       "no_anexo=", num_anexo, "&",
                                       "co_esfera=", esfera , "&",
                                       "co_poder=", cod_poder, "&",
                                       "id_ente=", ente, sep = "")



              rgf <- httr::GET(chamada_api_rgf, config = httr::config(connecttimeout = 60))

              httr::status_code(rgf)

              rgf_txt <- httr::content(rgf, as="text", encoding="UTF-8")

              rgf_json <- jsonlite::fromJSON(rgf_txt, flatten = FALSE)

              rgf_df1 <- as.data.frame(rgf_json[["items"]])

              rgf_df <-  rbind(rgf_df, rgf_df1)

            }
          }
        }
      }



    if({{num_anexo}} == "RGF-Anexo%2001" & simplificado != TRUE) {
      rgf_df0 <- rgf_df %>%
        dplyr::mutate(mes_nome = dplyr::case_when(periodo == 3 & coluna == '<MR-11>'~ glue::glue('jan/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-11>'~ glue::glue('set/{exercicio - 1}'),
                                                  periodo == 1 & coluna == '<MR-11>'~ glue::glue('mai/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-10>'~ glue::glue('fev/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-10>'~ glue::glue('out/{exercicio - 1}'),
                                                  periodo == 1 & coluna == '<MR-10>'~ glue::glue('jun/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-9>'~ glue::glue('mar/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-9>'~ glue::glue('nov/{exercicio - 1}'),
                                                  periodo == 1 & coluna == '<MR-9>'~ glue::glue('jul/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-8>'~ glue::glue('abr/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-8>'~ glue::glue('dez/{exercicio - 1}'),
                                                  periodo == 1 & coluna == '<MR-8>'~ glue::glue('ago/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-7>'~ glue::glue('mai/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-7>'~ glue::glue('jan/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-7>'~ glue::glue('set/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-6>'~ glue::glue('jun/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-6>'~ glue::glue('fev/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-6>'~ glue::glue('out/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-5>'~ glue::glue('jul/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-5>'~ glue::glue('mar/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-5>'~ glue::glue('nov/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-4>'~ glue::glue('ago/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-4>'~ glue::glue('abr/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-4>'~ glue::glue('dez/{exercicio - 1}'),
                                                  periodo == 3 & coluna == '<MR-3>'~ glue::glue('set/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-3>'~ glue::glue('mai/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-3>'~ glue::glue('jan/{exercicio}'),
                                                  periodo == 3 & coluna == '<MR-2>'~ glue::glue('out/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-2>'~ glue::glue('jun/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-2>'~ glue::glue('fev/{exercicio}'),
                                                  periodo == 3 & coluna == '<MR-1>'~ glue::glue('nov/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR-1>'~ glue::glue('jul/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR-1>'~ glue::glue('mar/{exercicio}'),
                                                  periodo == 3 & coluna == '<MR>'~ glue::glue('dez/{exercicio}'),
                                                  periodo == 2 & coluna == '<MR>'~ glue::glue('ago/{exercicio}'),
                                                  periodo == 1 & coluna == '<MR>'~ glue::glue('abr/{exercicio}'),
                                                  TRUE ~ coluna),
                      mes = dplyr::case_when(periodo == 3 & coluna == '<MR-11>'~1,
                                             periodo == 2 & coluna == '<MR-11>'~9,
                                             periodo == 1 & coluna == '<MR-11>'~5,
                                             periodo == 3 & coluna == '<MR-10>'~2,
                                             periodo == 2 & coluna == '<MR-10>'~10,
                                             periodo == 1 & coluna == '<MR-10>'~6,
                                             periodo == 3 & coluna == '<MR-9>'~3,
                                             periodo == 2 & coluna == '<MR-9>'~11,
                                             periodo == 1 & coluna == '<MR-9>'~7,
                                             periodo == 3 & coluna == '<MR-8>'~4,
                                             periodo == 2 & coluna == '<MR-8>'~12,
                                             periodo == 1 & coluna == '<MR-8>'~8,
                                             periodo == 3 & coluna == '<MR-7>'~5,
                                             periodo == 2 & coluna == '<MR-7>'~1,
                                             periodo == 1 & coluna == '<MR-7>'~9,
                                             periodo == 3 & coluna == '<MR-6>'~6,
                                             periodo == 2 & coluna == '<MR-6>'~2,
                                             periodo == 1 & coluna == '<MR-6>'~10,
                                             periodo == 3 & coluna == '<MR-5>'~7,
                                             periodo == 2 & coluna == '<MR-5>'~3,
                                             periodo == 1 & coluna == '<MR-5>'~11,
                                             periodo == 3 & coluna == '<MR-4>'~8,
                                             periodo == 2 & coluna == '<MR-4>'~4,
                                             periodo == 1 & coluna == '<MR-4>'~12,
                                             periodo == 3 & coluna == '<MR-3>'~9,
                                             periodo == 2 & coluna == '<MR-3>'~5,
                                             periodo == 1 & coluna == '<MR-3>'~1,
                                             periodo == 3 & coluna == '<MR-2>'~10,
                                             periodo == 2 & coluna == '<MR-2>'~6,
                                             periodo == 1 & coluna == '<MR-2>'~2,
                                             periodo == 3 & coluna == '<MR-1>'~11,
                                             periodo == 2 & coluna == '<MR-1>'~7,
                                             periodo == 1 & coluna == '<MR-1>'~3,
                                             periodo == 3 & coluna == '<MR>'~12,
                                             periodo == 2 & coluna == '<MR>'~8,
                                             periodo == 1 & coluna == '<MR>'~4),
                      valor = as.numeric(rgf_df$valor)
        )

      # assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      # rm(rgf_df0)
      return(rgf_df0)

    } else {

      rgf_df0 <- rgf_df
      # assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      # rm(rgf_df0)
      return(rgf_df0)

    }

    cli::cli_progress_done()

  }

  # ADICIONAR AJUSTE DE DATA.FRAME AQUI!!!!!!!!

}
