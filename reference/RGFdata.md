# Extração Inteligente e Vetorizada de Dados do RGF (SICONFI)

**\[stable\]**

Realiza a extração automatizada e em lote dos dados do Relatório de
Gestão Fiscal (RGF) diretamente da API do SICONFI (Tesouro Nacional). A
função resolve nativamente grades paramétricas complexas (produto
cartesiano de anos, entes, poderes e períodos), otimiza o consumo de
memória RAM e processa de forma determinística as colunas temporais
móveis (como a apuração de meses de referência no Anexo 01).

## Usage

``` r
RGFdata(
  cod.ibge = NULL,
  year = NULL,
  power = NULL,
  period = NULL,
  annex = NULL,
  simplified = FALSE,
  save_path = NULL
)
```

## Arguments

- cod.ibge:

  Código IBGE do Ente. Vetor numérico ou caractere contendo os códigos
  dos entes. Aceita códigos de 1 dígito (União), 2 dígitos (Estados) ou
  7 dígitos (Municípios). Para obter todas as UF's de forma combinada
  utilize o atalho `"all_states"`. Para obter todos os municípios
  utilize `"all_muni"`. Se `simplified = TRUE`, selecione municípios
  compatíveis.

- year:

  Exercício do relatório. Vetor numérico contendo os anos desejados para
  a análise (ex: `2022:2024`).

- power:

  Código do poder. Vetor de caracteres indicando os poderes pretendidos.
  Valores disponíveis: `E` = Executivo, `L` = Legislativo, `J` =
  Judiciário, `M` = Ministério Público, `D` = Defensoria Pública.

- period:

  Quadrimestre ou semestre de referência do relatório dentro de um
  exercício. A periodicidade semestral é automaticamente selecionada se
  `simplified = TRUE` (valores aceitos: 1 ou 2). A periodicidade padrão
  do relatório é quadrimestral (valores aceitos: 1, 2 ou 3).

- annex:

  Anexos dos demonstrativos do RGF. Valores disponíveis: `1`, `2`, `3`,
  `4`, `5` ou `6`.

- simplified:

  Tipo do Demonstrativo. RGF Simplificado aplica-se apenas aos
  municípios com menos de 50 mil habitantes que optaram pela publicação
  semestral dos relatórios. Se `TRUE`, a periodicidade semestral será
  automaticamente assumida e o período 3 passará a ser inválido. Padrão
  é `FALSE`.

- save_path:

  Caminho de arquivo opcional em disco (caractere) para persistência
  imediata dos dados. Se a extensão informada for `.parquet`, o pacote
  exige a instalação do pacote `arrow`. Também aceita extensões `.rds` e
  `.csv`. Se omitido (`NULL`), os dados são retornados diretamente para
  a sessão do R.

## Value

Um objeto `data.frame` (ou um objeto invisível caso `save_path` seja
acionado) contendo as colunas padronizadas da API do SICONFI empilhadas
verticalmente de forma limpa, com as colunas do tipo `<MR-X>`
devidamente traduzidas para o formato de data real `mes/ano`.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Extração básica: Anexo 01 de Goiás (IBGE 52) para o 3º quadrimestre de 2023, todos os poderes
dados_rgf_go <- RGFdata(
  cod.ibge = 52,
  year = 2023,
  power = c("E", "L", "J", "D", "M"),
  period = 3,
  annex = 1,
  simplified = FALSE
)

# 2. Extração combinada multivariada em lote
# Baixa dados do Executivo e Legislativo para os anos 2022 e 2023 em múltiplos quadrimestres
dados_rgf_multi <- RGFdata(
  cod.ibge = c(5208707, 5201405),
  year = 2022:2023,
  power = c("E", "L"),
  period = c(1, 2),
  annex = 1
)

# 3. Ingestão para Data Lake local salvando diretamente em formato Parquet
RGFdata(
  cod.ibge = "all_states",
  year = 2024,
  power = "E",
  period = 1:3,
  annex = 2,
  save_path = "data/storage/rgf_executivo_estados_2024.parquet"
)
} # }
```
