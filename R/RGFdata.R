RGFdata <- function(UF = NULL, ano = NULL, poder = NULL, quadrimestre = NULL, anexo = NULL){


  base_url_rgf <- "https://apidatalake.tesouro.gov.br/ords/siconfi/tt/rgf?"


  rgf_df <- data.frame(stringsAsFactors = FALSE)


  for(z in 1:length(ano)) {
    cli::cli_progress_step("EXTRAINDO {ano[[z]]}.", spinner = TRUE)


    step1 <- " "
    cli::cli_progress_bar("Unidade(s) Federativa(s) |", total = length(UF))
    for (j in 1:length(UF)) {
      step1 <- glue::glue("{UF[[j]]}")
      cli::cli_progress_update(set = j)


      for (i in 1:length(poder)) {

        exercicio <- ano[[z]]
        periodo = "Q"
        quadrimestre <- as.character(quadrimestre)
        tipo_relatorio <- "RGF"
        num_anexo <- glue::glue("RGF-Anexo%200",{{anexo}})
        esfera <- "E"
        cod_poder <- poder[[i]]
        ente <- UF[[j]]

        # montar a chamada à API
        chamada_api_rgf <- paste(base_url_rgf,
                                 "an_exercicio=", exercicio, "&",
                                 "in_periodicidade=", periodo,"&",
                                 "nr_periodo=", quadrimestre, "&",
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

  if({{num_anexo}} == "RGF-Anexo%2001"){
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

  } else {

    rgf_df0 <- rgf_df
    assign(glue::glue('RGF-Anexo_0{anexo}'), rgf_df0, envir=.GlobalEnv)
    rm(rgf_df0)

  }

  cli::cli_progress_done()

  cli::cli_alert_info("RESUMO:")
  cli::cli_div(theme = list(span.emph = list(color = "orange")))
  cli::cli_li('{.emph \nPeriodo: }')
  cli::cli_text('{length(ano)} ano(s).')

  cli::cli_li("{.emph \nUnidades federativas: }")
  cli::cli_text('{length(UF)} UF(s).')

  cli::cli_li("{.emph \nPoderes: }")
  cli::cli_text('{length(poder)} podere(s).')
  cli::cli_end()

}
