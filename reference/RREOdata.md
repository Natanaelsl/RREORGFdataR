# Extração Inteligente e Vetorizada de Dados do RREO (SICONFI)

**\[stable\]**

Realiza a extração automatizada e em lote dos dados do Relatório
Resumido de Execução Orçamentária (RREO) Diretamente da API do SICONFI
(Tesouro Nacional). A função resolve nativamente grades paramétricas
complexas (produto cartesiano), otimiza o consumo de memória RAM e
processa de forma determinística as colunas temporais móveis (como os
últimos 12 meses do Anexo 03).

## Usage

``` r
RREOdata(
  cod.ibge = NULL,
  year = NULL,
  period = NULL,
  annex = NULL,
  simplified = FALSE,
  save_path = NULL
)
```

## Arguments

- cod.ibge:

  Vetor numérico ou caractere contendo os códigos IBGE dos entes
  federativos. Aceita códigos de 1 dígito (União), 2 dígitos (Estados)
  ou 7 dígitos (Municípios). Pode receber os atalhos estendidos
  `"all_states"` para extrair todas as Unidades da Federação de forma
  combinada ou `"all_muni"` para todos os municípios brasileiros.

- year:

  Vetor numérico contendo os anos/exercícios desejados para a análise
  (ex: `2022:2024`).

- period:

  Vetor numérico contendo os bimestres de referência do relatório dentro
  de um exercício. Valores típicos disponíveis: `1` a `6`. Caso
  `simplified = TRUE`, a periodicidade passa a ser semestral, aceitando
  os valores `1` ou `2`.

- annex:

  Vetor numérico mapeando os anexos dos demonstrativos do RREO a serem
  baixados (valores aceitos de `1` a `14` conforme disponibilidade na
  API do SICONFI).

- simplified:

  Lógico. Define se a consulta deve buscar a versão do RREO
  Simplificado. Esta opção aplica-se estritamente a municípios com menos
  de 50 mil habitantes que optaram formalmente pela publicação semestral
  dos seus relatórios fiscais. Padrão é `FALSE`.

- save_path:

  Caminho de arquivo opcional em disco (caractere) para persistência
  imediata dos dados consolidados. Se a extensão informada for
  `.parquet`, o pacote exige a instalação do pacote `arrow`. Também
  aceita extensões `.rds` e `.csv`. Se omitido (`NULL`), os dados são
  retornados diretamente para a sessão do R.

## Value

Um objeto `data.frame` (ou um objeto invisível caso `save_path` seja
acionado) contendo as colunas padronizadas da API do SICONFI empilhadas
verticalmente de forma limpa, com as colunas do tipo `<MR-X>`
devidamente traduzidas para o formato de data real `mes/ano`.

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Extração básica: Anexo 01 do Estado de Goiás (IBGE 52) para o 6º Bimestre de 2023
dados_goias <- RREOdata(
  cod.ibge = 52,
  year = 2023,
  period = 6,
  annex = 1
)

# 2. Extração combinada multivariada (Gera produto cartesiano interno)
# Baixa dados de Goiânia (5208707) e Aparecida (5201405), anos 2022 e 2023, bimestres 1 e 6
dados_multi <- RREOdata(
  cod.ibge = c(5208707, 5201405),
  year = 2022:2023,
  period = c(1, 6),
  annex = 3
)

# 3. Pipeline de Data Lake: Baixando dados em lote e salvando direto em Parquet
# Nota: Requer que o pacote 'arrow' esteja instalado no ambiente.
RREOdata(
  cod.ibge = "all_states",
  year = 2024,
  period = 1:6,
  annex = 1,
  save_path = "data/storage/rreo_estados_2024.parquet"
)

# 4. Fluxo de leitura local após salvar (Lazy Loading)
# gc() # Chama o garbage collector para limpar a RAM
# tab.parquet <- arrow::open_dataset("data/storage/rreo_estados_2024.parquet")
# tab.parquet |>
#  filter(id_ente == "52") |>
#  collect()
} # }
```
