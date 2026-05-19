test_that("RGFdata retorna data.frame valido e com colunas corretas", {
  testthat::skip_if_offline() # Pula o teste se não houver internet

  # Fazemos UMA única requisição para testar tudo
  resultado <- RGFdata(cod.ibge = 52, year = 2023, power = "E", period = 1, annex = 1)

  # 1. Verifica se retornou data.frame
  expect_s3_class(resultado, "data.frame")

  # 2. Verifica se não retornou vazio
  expect_true(nrow(resultado) > 0)

  # 3. Verifica colunas base (flexibilizado para o padrão do Tesouro Nacional)
  expect_true("valor" %in% names(resultado))
  expect_true(any(c("instituicao", "id_ente", "cod_ibge") %in% names(resultado)))
})

test_that("RREOdata processa multiplos anos corretamente (Vetorizacao)", {
  testthat::skip_if_offline()

  # Requisita 2 bimestres para 1 município
  resultado <- RREOdata(cod.ibge = 5208707, year = 2023, period = c(1, 2), annex = 1)

  expect_s3_class(resultado, "data.frame")
  expect_true(nrow(resultado) > 0)

  # Garante que os dois períodos vieram na mesma tabela
  periodos_unicos <- unique(resultado$periodo)
  expect_true(all(c("1", "2") %in% periodos_unicos))
})
