test_that("API lida com Erro HTTP 500 (Internal Server Error) sem quebrar o loop", {

  # 1. Cria uma resposta HTTP falsa simulando a queda do servidor do SICONFI
  resposta_falsa_500 <- structure(
    list(
      url = "https://apidatalake.tesouro.gov.br",
      status_code = 500L,
      headers = list()
    ),
    class = "response"
  )

  # 2. Substitui temporariamente a função GET do httr apenas dentro deste bloco
  testthat::local_mocked_bindings(
    GET = function(...) return(resposta_falsa_500),
    .package = "httr"
  )

  # 3. Executa a função. Ela DEVE emitir um warning do cli e retornar um df vazio,
  # mas NÃO PODE emitir um erro fatal (stop).
  expect_message(
    resultado <- RGFdata(cod.ibge = 52, year = 2023, power = "E", period = 1, annex = 1),
    "No data was returned"
  )

  # 4. Verifica se a camada de proteção funcionou retornando um data.frame limpo (0 linhas)
  expect_s3_class(resultado, "data.frame")
  expect_equal(nrow(resultado), 0)
})

test_that("API lida com Timeout (Queda total de conexão) capturando no tryCatch", {

  # 1. Força a função GET a dar um erro crítico (simulando timeout de rede)
  testthat::local_mocked_bindings(
    GET = function(...) stop("Timeout was reached"),
    .package = "httr"
  )

  # 2. Testa se o tryCatch interno do motor (.fetch_siconfi_api) capturou o erro
  # e impediu o pacote de fechar a barra de progresso com falha geral.
  expect_message(
    resultado <- RREOdata(cod.ibge = 52, year = 2023, period = 1, annex = 1),
    "No data was returned"
  )

  expect_s3_class(resultado, "data.frame")
  expect_equal(nrow(resultado), 0)
})
