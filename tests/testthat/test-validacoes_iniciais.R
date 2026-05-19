test_that("siconfi_list retorna estrutura correta sem internet", {
  # Testa a leitura do RDS interno sem precisar bater na API
  df <- siconfi_list(action = "view")

  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 5000) # Garante que a base tem os municípios
})

test_that("RREOdata fail-fast captura inputs invalidos", {
  # Tentar relatório simplificado para a União (IBGE = 1)
  expect_error(
    RREOdata(cod.ibge = 1, year = 2023, period = 1, annex = 1, simplified = TRUE),
    "simplified publication only applies to municipalities"
  )

  # Passar IBGE com quantidade de dígitos incorreta
  expect_error(
    RREOdata(cod.ibge = 1234, year = 2023, period = 1, annex = 1),
    "Invalid `cod.ibge`"
  )
})

test_that("RGFdata fail-fast captura inputs invalidos", {
  # Tentar quadrimestre 3 para município com RGF simplificado (que é semestral)
  expect_error(
    RGFdata(cod.ibge = 5208707, year = 2023, power = "E", period = 3, annex = 1, simplified = TRUE),
    "Period 3 is invalid for simplified reports"
  )
})
