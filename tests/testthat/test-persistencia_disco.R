test_that("Salvamento em disco (.rds, .csv, .parquet) funciona", {
  testthat::skip_if_offline()

  # Cria arquivos temporários (serão deletados automaticamente pelo R depois)
  tmp_dir <- tempdir()
  file_rds <- file.path(tmp_dir, "teste.rds")
  file_csv <- file.path(tmp_dir, "teste.csv")
  file_parquet <- file.path(tempdir(), "rreo_estados_2024.parquet")

  # 1. Testa CSV e RDS
  df_invisivel <- RREOdata(cod.ibge = 52, year = 2023, period = 1, annex = 1, save_path = file_rds)
  expect_true(file.exists(file_rds))
  expect_s3_class(df_invisivel, "data.frame") # Garante que o retorno invisível é o df

  RREOdata(cod.ibge = 52, year = 2023, period = 1, annex = 1, save_path = file_csv)
  expect_true(file.exists(file_csv))

  # 2. Testa Parquet APENAS se o pacote arrow estiver instalado no ambiente de teste
  if (requireNamespace("arrow", quietly = TRUE)) {
    RREOdata(cod.ibge = 52, year = 2023, period = 1, annex = 1, save_path = file_parquet)
    expect_true(file.exists(file_parquet))
  }

  # 3. Testa download do siconfi_list
  pdf_path <- siconfi_list(action = "download", dest_dir = tmp_dir)
  expect_true(file.exists(pdf_path))
  expect_match(pdf_path, "\\.pdf$") # Confirma se a extensão é .pdf
})
