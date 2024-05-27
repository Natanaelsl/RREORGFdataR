test_that("check conection", {

  testthat::expect_true(RCurl::url.exists("https://apidatalake.tesouro.gov.br/docs/siconfi/"))

})


test_that("check data.frame", {

  anexo1 <- RREORGFdataR::RGFdata(cod.ibge = 52, year = 2023, power = 'E', period = 1, annex = 1, simplified = FALSE)
  anexo2 <- RREORGFdataR::RGFdata(cod.ibge = 52, year = 2023, power = 'E', period = 2, annex = 1, simplified = FALSE)
  anexo3 <- RREORGFdataR::RGFdata(cod.ibge = 52, year = 2023, power = 'E', period = 3, annex = 1, simplified = FALSE)

  testthat::expect_s3_class(anexo1, "data.frame")
  testthat::expect_s3_class(anexo2, "data.frame")
  testthat::expect_s3_class(anexo3, "data.frame")

})


test_that("check number of column", {

  anexo1 <- RREORGFdataR::RGFdata(cod.ibge = 52, year = 2023, power = 'E', period = 1, annex = 1, simplified = FALSE)
  anexo2 <- RREORGFdataR::RGFdata(cod.ibge = 52, year = 2023, power = 'E', period = 2, annex = 1, simplified = FALSE)
  anexo3 <- RREORGFdataR::RGFdata(cod.ibge = 52, year = 2023, power = 'E', period = 3, annex = 1, simplified = FALSE)

  testthat::expect_equal(ncol(anexo1), 17)
  testthat::expect_equal(ncol(anexo2), 17)
  testthat::expect_equal(ncol(anexo3), 17)

})
