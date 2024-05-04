RGFdata <- function(cod.ibge = NULL, ano = NULL, poder = NULL, periodo = NULL, anexo = NULL, simplificado = FALSE){


  base_url_rgf <- "https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rgf?"


  rgf_df <- data.frame(stringsAsFactors = FALSE)

  # DEFININDO ESTRUTURA PARA UNIÃO
  if(all(nchar(cod.ibge) == 1) & {simplificado} == TRUE ) {
    cli::cli_alert_danger("A publicação simplificada aplica-se apenas aos municípios com menos de 50 mil habitantes. Não compatível com o `cod.ibge` informado.")
  }

  if(cod.ibge == 1 & (simplificado == FALSE) | is.null(simplificado) ) {

    for(z in 1:length(ano)) {
      cli::cli_progress_step("EXTRAINDO {ano[[z]]}", spinner = TRUE)


      step0 <- " "
      cli::cli_progress_step("cod. IBGE | {step0}", spinner = TRUE, msg_done = "Finalizado!")
      for (j in 1:length(cod.ibge)) {
        Sys.sleep(.1)
        step0 <- glue::glue("{cod.ibge[[j]]}")
        cli::cli_progress_update(set = j)


        for (i in 1:length(poder)) {

          exercicio <- ano[[z]]
          periodicidade = "Q"
          tempo <- as.character({periodo})
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


          rgf <- httr::GET(chamada_api_rgf)

          httr::status_code(rgf)

          rgf_txt <- httr::content(rgf, as="text", encoding="UTF-8")

          rgf_json <- jsonlite::fromJSON(rgf_txt, flatten = FALSE)

          rgf_df1 <- as.data.frame(rgf_json[["items"]])

          rgf_df <-  rbind(rgf_df, rgf_df1)

        }
      }
    }

    if({{num_anexo}} == "RGF-Anexo%2001" & {periodo} == 3){
      rgf_df0 <- rgf_df %>%
        dplyr::mutate(mes_nome = dplyr::case_when(coluna == '<MR-11>'~ glue::glue('jan/{exercicio}'),
                                                  coluna == '<MR-10>'~ glue::glue('fev/{exercicio}'),
                                                  coluna == '<MR-9>'~ glue::glue('mar/{exercicio}'),
                                                  coluna == '<MR-8>'~ glue::glue('abr/{exercicio}'),
                                                  coluna == '<MR-7>'~ glue::glue('mai/{exercicio}'),
                                                  coluna == '<MR-6>'~ glue::glue('jun/{exercicio}'),
                                                  coluna == '<MR-5>'~ glue::glue('jul/{exercicio}'),
                                                  coluna == '<MR-4>'~ glue::glue('ago/{exercicio}'),
                                                  coluna == '<MR-3>'~ glue::glue('set/{exercicio}'),
                                                  coluna == '<MR-2>'~ glue::glue('out/{exercicio}'),
                                                  coluna == '<MR-1>'~ glue::glue('nov/{exercicio}'),
                                                  coluna == '<MR>'~ glue::glue('dez/{exercicio}'),
                                                  TRUE ~ coluna),
                      mes = dplyr::case_when(coluna == '<MR-11>'~1,
                                             coluna == '<MR-10>'~2,
                                             coluna == '<MR-9>'~3,
                                             coluna == '<MR-8>'~4,
                                             coluna == '<MR-7>'~5,
                                             coluna == '<MR-6>'~6,
                                             coluna == '<MR-5>'~7,
                                             coluna == '<MR-4>'~8,
                                             coluna == '<MR-3>'~9,
                                             coluna == '<MR-2>'~10,
                                             coluna == '<MR-1>'~11,
                                             coluna == '<MR>'~12),
                      valor = as.numeric(valor)
        )

      assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      rm(rgf_df0)

    } else if({{num_anexo}} == "RGF-Anexo%2001" & {periodo} == 2) {

      rgf_df0 <- rgf_df %>%
        dplyr::mutate(mes_nome = dplyr::case_when(coluna == '<MR-11>'~ glue::glue('set/{exercicio - 1}'),
                                                  coluna == '<MR-10>'~ glue::glue('out/{exercicio - 1}'),
                                                  coluna == '<MR-9>'~ glue::glue('nov/{exercicio - 1}'),
                                                  coluna == '<MR-8>'~ glue::glue('dez/{exercicio - 1}'),
                                                  coluna == '<MR-7>'~ glue::glue('jan/{exercicio}'),
                                                  coluna == '<MR-6>'~ glue::glue('fev/{exercicio}'),
                                                  coluna == '<MR-5>'~ glue::glue('mar/{exercicio}'),
                                                  coluna == '<MR-4>'~ glue::glue('abr/{exercicio}'),
                                                  coluna == '<MR-3>'~ glue::glue('mai/{exercicio}'),
                                                  coluna == '<MR-2>'~ glue::glue('jun/{exercicio}'),
                                                  coluna == '<MR-1>'~ glue::glue('jul/{exercicio}'),
                                                  coluna == '<MR>'~ glue::glue('ago/{exercicio}'),
                                                  TRUE ~ coluna),
                      mes = dplyr::case_when(coluna == '<MR-11>'~9,
                                             coluna == '<MR-10>'~10,
                                             coluna == '<MR-9>'~11,
                                             coluna == '<MR-8>'~12,
                                             coluna == '<MR-7>'~1,
                                             coluna == '<MR-6>'~2,
                                             coluna == '<MR-5>'~3,
                                             coluna == '<MR-4>'~4,
                                             coluna == '<MR-3>'~5,
                                             coluna == '<MR-2>'~6,
                                             coluna == '<MR-1>'~7,
                                             coluna == '<MR>'~8),
                      valor = as.numeric(valor)
        )

      assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      rm(rgf_df0)

    } else if({{num_anexo}} == "RGF-Anexo%2001" & {periodo} == 1) {

      rgf_df0 <- rgf_df %>%
        dplyr::mutate(mes_nome = dplyr::case_when(coluna == '<MR-11>'~ glue::glue('mai/{exercicio - 1}'),
                                                  coluna == '<MR-10>'~ glue::glue('jun/{exercicio - 1}'),
                                                  coluna == '<MR-9>'~ glue::glue('jul/{exercicio - 1}'),
                                                  coluna == '<MR-8>'~ glue::glue('ago/{exercicio - 1}'),
                                                  coluna == '<MR-7>'~ glue::glue('set/{exercicio - 1}'),
                                                  coluna == '<MR-6>'~ glue::glue('out/{exercicio - 1}'),
                                                  coluna == '<MR-5>'~ glue::glue('nov/{exercicio - 1}'),
                                                  coluna == '<MR-4>'~ glue::glue('dez/{exercicio - 1}'),
                                                  coluna == '<MR-3>'~ glue::glue('jan/{exercicio}'),
                                                  coluna == '<MR-2>'~ glue::glue('fev/{exercicio}'),
                                                  coluna == '<MR-1>'~ glue::glue('mar/{exercicio}'),
                                                  coluna == '<MR>'~ glue::glue('abr/{exercicio}'),
                                                  TRUE ~ coluna),
                      mes = dplyr::case_when(coluna == '<MR-11>'~5,
                                             coluna == '<MR-10>'~6,
                                             coluna == '<MR-9>'~7,
                                             coluna == '<MR-8>'~8,
                                             coluna == '<MR-7>'~9,
                                             coluna == '<MR-6>'~10,
                                             coluna == '<MR-5>'~11,
                                             coluna == '<MR-4>'~12,
                                             coluna == '<MR-3>'~1,
                                             coluna == '<MR-2>'~2,
                                             coluna == '<MR-1>'~3,
                                             coluna == '<MR>'~4),
                      valor = as.numeric(valor)
        )

      assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      rm(rgf_df0)

    } else {

      rgf_df0 <- rgf_df
      assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      rm(rgf_df0)

    }

    cli::cli_progress_done()

  }

  # DEFININDO ESTRUTURA PARA ESTADOS
  if(all(nchar(cod.ibge) == 2) & (simplificado == FALSE) | is.null(simplificado) ) {

    for(z in 1:length(ano)) {
      cli::cli_progress_step("EXTRAINDO {ano[[z]]}", spinner = TRUE)


      step1 <- " "
      cli::cli_progress_step("cod. IBGE | {step1}", spinner = TRUE, msg_done = "Finalizado!")
      for (j in 1:length(cod.ibge)) {
        Sys.sleep(.1)
        step1 <- glue::glue("{cod.ibge[[j]]}")
        cli::cli_progress_update(set = j)


        for (i in 1:length(poder)) {

          exercicio <- ano[[z]]
          periodicidade = "Q"
          tempo <- as.character({periodo})
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



          rgf <- httr::GET(chamada_api_rgf)

          httr::status_code(rgf)

          rgf_txt <- httr::content(rgf, as="text", encoding="UTF-8")

          rgf_json <- jsonlite::fromJSON(rgf_txt, flatten = FALSE)

          rgf_df1 <- as.data.frame(rgf_json[["items"]])

          rgf_df <-  rbind(rgf_df, rgf_df1)

        }
      }
    }

    if({{num_anexo}} == "RGF-Anexo%2001" & {periodo} == 3){
      rgf_df0 <- rgf_df %>%
        dplyr::mutate(mes_nome = dplyr::case_when(coluna == '<MR-11>'~ glue::glue('jan/{exercicio}'),
                                                  coluna == '<MR-10>'~ glue::glue('fev/{exercicio}'),
                                                  coluna == '<MR-9>'~ glue::glue('mar/{exercicio}'),
                                                  coluna == '<MR-8>'~ glue::glue('abr/{exercicio}'),
                                                  coluna == '<MR-7>'~ glue::glue('mai/{exercicio}'),
                                                  coluna == '<MR-6>'~ glue::glue('jun/{exercicio}'),
                                                  coluna == '<MR-5>'~ glue::glue('jul/{exercicio}'),
                                                  coluna == '<MR-4>'~ glue::glue('ago/{exercicio}'),
                                                  coluna == '<MR-3>'~ glue::glue('set/{exercicio}'),
                                                  coluna == '<MR-2>'~ glue::glue('out/{exercicio}'),
                                                  coluna == '<MR-1>'~ glue::glue('nov/{exercicio}'),
                                                  coluna == '<MR>'~ glue::glue('dez/{exercicio}'),
                                                  TRUE ~ coluna),
                      mes = dplyr::case_when(coluna == '<MR-11>'~1,
                                             coluna == '<MR-10>'~2,
                                             coluna == '<MR-9>'~3,
                                             coluna == '<MR-8>'~4,
                                             coluna == '<MR-7>'~5,
                                             coluna == '<MR-6>'~6,
                                             coluna == '<MR-5>'~7,
                                             coluna == '<MR-4>'~8,
                                             coluna == '<MR-3>'~9,
                                             coluna == '<MR-2>'~10,
                                             coluna == '<MR-1>'~11,
                                             coluna == '<MR>'~12),
                      valor = as.numeric(valor)
        )

      assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      rm(rgf_df0)

    } else if({{num_anexo}} == "RGF-Anexo%2001" & {periodo} == 2) {

      rgf_df0 <- rgf_df %>%
        dplyr::mutate(mes_nome = dplyr::case_when(coluna == '<MR-11>'~ glue::glue('set/{exercicio - 1}'),
                                                  coluna == '<MR-10>'~ glue::glue('out/{exercicio - 1}'),
                                                  coluna == '<MR-9>'~ glue::glue('nov/{exercicio - 1}'),
                                                  coluna == '<MR-8>'~ glue::glue('dez/{exercicio - 1}'),
                                                  coluna == '<MR-7>'~ glue::glue('jan/{exercicio}'),
                                                  coluna == '<MR-6>'~ glue::glue('fev/{exercicio}'),
                                                  coluna == '<MR-5>'~ glue::glue('mar/{exercicio}'),
                                                  coluna == '<MR-4>'~ glue::glue('abr/{exercicio}'),
                                                  coluna == '<MR-3>'~ glue::glue('mai/{exercicio}'),
                                                  coluna == '<MR-2>'~ glue::glue('jun/{exercicio}'),
                                                  coluna == '<MR-1>'~ glue::glue('jul/{exercicio}'),
                                                  coluna == '<MR>'~ glue::glue('ago/{exercicio}'),
                                                  TRUE ~ coluna),
                      mes = dplyr::case_when(coluna == '<MR-11>'~9,
                                             coluna == '<MR-10>'~10,
                                             coluna == '<MR-9>'~11,
                                             coluna == '<MR-8>'~12,
                                             coluna == '<MR-7>'~1,
                                             coluna == '<MR-6>'~2,
                                             coluna == '<MR-5>'~3,
                                             coluna == '<MR-4>'~4,
                                             coluna == '<MR-3>'~5,
                                             coluna == '<MR-2>'~6,
                                             coluna == '<MR-1>'~7,
                                             coluna == '<MR>'~8),
                      valor = as.numeric(valor)
        )

      assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      rm(rgf_df0)

    } else if({{num_anexo}} == "RGF-Anexo%2001" & {periodo} == 1) {

      rgf_df0 <- rgf_df %>%
        dplyr::mutate(mes_nome = dplyr::case_when(coluna == '<MR-11>'~ glue::glue('mai/{exercicio - 1}'),
                                                  coluna == '<MR-10>'~ glue::glue('jun/{exercicio - 1}'),
                                                  coluna == '<MR-9>'~ glue::glue('jul/{exercicio - 1}'),
                                                  coluna == '<MR-8>'~ glue::glue('ago/{exercicio - 1}'),
                                                  coluna == '<MR-7>'~ glue::glue('set/{exercicio - 1}'),
                                                  coluna == '<MR-6>'~ glue::glue('out/{exercicio - 1}'),
                                                  coluna == '<MR-5>'~ glue::glue('nov/{exercicio - 1}'),
                                                  coluna == '<MR-4>'~ glue::glue('dez/{exercicio - 1}'),
                                                  coluna == '<MR-3>'~ glue::glue('jan/{exercicio}'),
                                                  coluna == '<MR-2>'~ glue::glue('fev/{exercicio}'),
                                                  coluna == '<MR-1>'~ glue::glue('mar/{exercicio}'),
                                                  coluna == '<MR>'~ glue::glue('abr/{exercicio}'),
                                                  TRUE ~ coluna),
                      mes = dplyr::case_when(coluna == '<MR-11>'~5,
                                             coluna == '<MR-10>'~6,
                                             coluna == '<MR-9>'~7,
                                             coluna == '<MR-8>'~8,
                                             coluna == '<MR-7>'~9,
                                             coluna == '<MR-6>'~10,
                                             coluna == '<MR-5>'~11,
                                             coluna == '<MR-4>'~12,
                                             coluna == '<MR-3>'~1,
                                             coluna == '<MR-2>'~2,
                                             coluna == '<MR-1>'~3,
                                             coluna == '<MR>'~4),
                      valor = as.numeric(valor)
        )

      assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      rm(rgf_df0)

    } else {

      rgf_df0 <- rgf_df
      assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      rm(rgf_df0)

    }

    cli::cli_progress_done()

  }

  if(all(nchar(cod.ibge) == 2) & {simplificado} == TRUE ) {
    cli::cli_alert_danger("A publicação simplificada aplica-se apenas aos municípios com menos de 50 mil habitantes. Não compatível com o `cod.ibge` informado.")
  }

  # DEFININDO ESTRUTURA PARA MUNICÍPIOS
  if(all(nchar(cod.ibge) == 7) & {simplificado} == TRUE & {periodo} == 3) {
    cli::cli_alert_danger("RGF Simplificado aplica-se apenas aos municípios com menos de 50 mil habitantes que optaram pela publicação semestral dos relatórios. Defina `periodo` como 1 ou 2.")

  }

  if((all(nchar(cod.ibge) == 7) & {simplificado} == TRUE & {periodo} != 3) | (all(nchar(cod.ibge) == 7) & {simplificado} == FALSE) ) {

    for(z in 1:length(ano)) {
      cli::cli_progress_step("EXTRAINDO {ano[[z]]}", spinner = TRUE)


      step2 <- " "
      cli::cli_progress_step("cod. IBGE | {step2}", spinner = TRUE, msg_done = "Finalizado!")
      for (j in 1:length(cod.ibge)) {
        Sys.sleep(.1)
        step2 <- glue::glue("{cod.ibge[[j]]}")
        cli::cli_progress_update(set = j)


        for (i in 1:length(poder)) {

          exercicio <- ano[[z]]
          if({simplificado} == TRUE & ({periodo} == 1 | {periodo} == 2)) {
            tempo <- as.character({periodo})
          }

          if ({simplificado} == FALSE | is.null(simplificado)) {
            tempo <- as.character({periodo})
          }

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



          rgf <- httr::GET(chamada_api_rgf)

          httr::status_code(rgf)

          rgf_txt <- httr::content(rgf, as="text", encoding="UTF-8")

          rgf_json <- jsonlite::fromJSON(rgf_txt, flatten = FALSE)

          rgf_df1 <- as.data.frame(rgf_json[["items"]])

          rgf_df <-  rbind(rgf_df, rgf_df1)

        }
      }
    }

    if({{num_anexo}} == "RGF-Anexo%2001" & {periodo} == 3) {
      rgf_df0 <- rgf_df %>%
        dplyr::mutate(mes_nome = dplyr::case_when(coluna == '<MR-11>'~ glue::glue('jan/{exercicio}'),
                                                  coluna == '<MR-10>'~ glue::glue('fev/{exercicio}'),
                                                  coluna == '<MR-9>'~ glue::glue('mar/{exercicio}'),
                                                  coluna == '<MR-8>'~ glue::glue('abr/{exercicio}'),
                                                  coluna == '<MR-7>'~ glue::glue('mai/{exercicio}'),
                                                  coluna == '<MR-6>'~ glue::glue('jun/{exercicio}'),
                                                  coluna == '<MR-5>'~ glue::glue('jul/{exercicio}'),
                                                  coluna == '<MR-4>'~ glue::glue('ago/{exercicio}'),
                                                  coluna == '<MR-3>'~ glue::glue('set/{exercicio}'),
                                                  coluna == '<MR-2>'~ glue::glue('out/{exercicio}'),
                                                  coluna == '<MR-1>'~ glue::glue('nov/{exercicio}'),
                                                  coluna == '<MR>'~ glue::glue('dez/{exercicio}'),
                                                  TRUE ~ coluna),
                      mes = dplyr::case_when(coluna == '<MR-11>'~1,
                                             coluna == '<MR-10>'~2,
                                             coluna == '<MR-9>'~3,
                                             coluna == '<MR-8>'~4,
                                             coluna == '<MR-7>'~5,
                                             coluna == '<MR-6>'~6,
                                             coluna == '<MR-5>'~7,
                                             coluna == '<MR-4>'~8,
                                             coluna == '<MR-3>'~9,
                                             coluna == '<MR-2>'~10,
                                             coluna == '<MR-1>'~11,
                                             coluna == '<MR>'~12),
                      valor = as.numeric(valor)
        )

      assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      rm(rgf_df0)

    } else if({{num_anexo}} == "RGF-Anexo%2001" & {periodo} == 2) {

      rgf_df0 <- rgf_df %>%
        dplyr::mutate(mes_nome = dplyr::case_when(coluna == '<MR-11>'~ glue::glue('set/{exercicio - 1}'),
                                                  coluna == '<MR-10>'~ glue::glue('out/{exercicio - 1}'),
                                                  coluna == '<MR-9>'~ glue::glue('nov/{exercicio - 1}'),
                                                  coluna == '<MR-8>'~ glue::glue('dez/{exercicio - 1}'),
                                                  coluna == '<MR-7>'~ glue::glue('jan/{exercicio}'),
                                                  coluna == '<MR-6>'~ glue::glue('fev/{exercicio}'),
                                                  coluna == '<MR-5>'~ glue::glue('mar/{exercicio}'),
                                                  coluna == '<MR-4>'~ glue::glue('abr/{exercicio}'),
                                                  coluna == '<MR-3>'~ glue::glue('mai/{exercicio}'),
                                                  coluna == '<MR-2>'~ glue::glue('jun/{exercicio}'),
                                                  coluna == '<MR-1>'~ glue::glue('jul/{exercicio}'),
                                                  coluna == '<MR>'~ glue::glue('ago/{exercicio}'),
                                                  TRUE ~ coluna),
                      mes = dplyr::case_when(coluna == '<MR-11>'~9,
                                             coluna == '<MR-10>'~10,
                                             coluna == '<MR-9>'~11,
                                             coluna == '<MR-8>'~12,
                                             coluna == '<MR-7>'~1,
                                             coluna == '<MR-6>'~2,
                                             coluna == '<MR-5>'~3,
                                             coluna == '<MR-4>'~4,
                                             coluna == '<MR-3>'~5,
                                             coluna == '<MR-2>'~6,
                                             coluna == '<MR-1>'~7,
                                             coluna == '<MR>'~8),
                      valor = as.numeric(valor)
        )

      assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      rm(rgf_df0)

    } else if({{num_anexo}} == "RGF-Anexo%2001" & {periodo} == 1) {

      rgf_df0 <- rgf_df %>%
        dplyr::mutate(mes_nome = dplyr::case_when(coluna == '<MR-11>'~ glue::glue('mai/{exercicio - 1}'),
                                                  coluna == '<MR-10>'~ glue::glue('jun/{exercicio - 1}'),
                                                  coluna == '<MR-9>'~ glue::glue('jul/{exercicio - 1}'),
                                                  coluna == '<MR-8>'~ glue::glue('ago/{exercicio - 1}'),
                                                  coluna == '<MR-7>'~ glue::glue('set/{exercicio - 1}'),
                                                  coluna == '<MR-6>'~ glue::glue('out/{exercicio - 1}'),
                                                  coluna == '<MR-5>'~ glue::glue('nov/{exercicio - 1}'),
                                                  coluna == '<MR-4>'~ glue::glue('dez/{exercicio - 1}'),
                                                  coluna == '<MR-3>'~ glue::glue('jan/{exercicio}'),
                                                  coluna == '<MR-2>'~ glue::glue('fev/{exercicio}'),
                                                  coluna == '<MR-1>'~ glue::glue('mar/{exercicio}'),
                                                  coluna == '<MR>'~ glue::glue('abr/{exercicio}'),
                                                  TRUE ~ coluna),
                      mes = dplyr::case_when(coluna == '<MR-11>'~5,
                                             coluna == '<MR-10>'~6,
                                             coluna == '<MR-9>'~7,
                                             coluna == '<MR-8>'~8,
                                             coluna == '<MR-7>'~9,
                                             coluna == '<MR-6>'~10,
                                             coluna == '<MR-5>'~11,
                                             coluna == '<MR-4>'~12,
                                             coluna == '<MR-3>'~1,
                                             coluna == '<MR-2>'~2,
                                             coluna == '<MR-1>'~3,
                                             coluna == '<MR>'~4),
                      valor = as.numeric(valor)
        )

      assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      rm(rgf_df0)

    } else {

      rgf_df0 <- rgf_df
      assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
      rm(rgf_df0)

    }

    cli::cli_progress_done()

  }
}
