# Define vinculos visiveis globais para evitar NOTEs no R CMD check
# causadas pela avaliacao nao-padrao (NSE) do dplyr/tidyverse.
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "coluna",
    "is_mr",
    "x_val",
    "mes_calc",
    "mes",
    "ano_ref",
    "valor",
    "codigo_ibge"
  ))
}
