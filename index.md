# RREORGFdataR

## Visão geral

**RREORGFdataR** é um pacote R focado em alta performance para a
extração, consolidação e automação de dados orçamentários e fiscais
provenientes da API do SICONFI
(<https://apidatalake.tesouro.gov.br/docs/siconfi/>). O pacote foi
projetado para analistas e pesquisadores que necessitam integrar grandes
volumes de dados (RREO e RGF) em fluxos de trabalho de Ciência de Dados
de forma eficiente. Saiba mais sobre a utilização do ‘RREORGFdataR’ em
<https://natanaelsl.com.br/project/rreorgf_pkg/> ou
<https://natanaelsl.github.io/RREORGFdataR/>.

- `RREOdata`: Realiza a extração automatizada e em lote dos dados do
  Relatório Resumido da Execução Orçamentária (*RREO*). A função utiliza
  uma arquitetura de malha paramétrica (produto cartesiano) para
  resolver múltiplas consultas em uma única chamada, otimizando o
  consumo de RAM através de pré-alocação de memória e processamento
  determinístico.

- `RGFdata`: Extrai de forma intuitiva os dados do Relatório de Gestão
  Fiscal (*RGF*). Compartilha a mesma engine de alta performance do
  `RREOdata`, permitindo a consulta multivariada de anos, entes
  federativos, poderes e períodos de referência com consistência total
  no formato dos dados.

- `siconfi_list`: Ferramenta de gestão de metadados. A opção
  `action = "view"` (padrão) carrega o dicionário de códigos do IBGE e
  SICONFI diretamente da estrutura interna do pacote. A opção
  `action = "download"` provê integração direta com o repositório do
  Tesouro Nacional, baixando o catálogo oficial em PDF com gerenciamento
  inteligente de caminhos de diretório (cross-platform).

## Principais Funcionalidades

*Extração Vetorizada*: Suporte a grades paramétricas complexas (produto
cartesiano de entes, anos e períodos) em uma única chamada.

*Gestão de Memória*: Arquitetura otimizada que utiliza pré-alocação e
processamento eficiente para evitar estouros de RAM.

*Persistência Direta*: Suporte nativo para exportação em formatos de
alta performance como Parquet (via `arrow`), RDS e CSV.

*UX Inteligente*: Interface CLI amigável com barra de progresso (ETA),
tratamento de erros `fail-fast` e macros para extração em lote (ex:
`all_states`, `all_muni`).

## Instalação

Você pode instalar a versão de desenvolvimento do `RREORGFdataR` no
[GitHub](https://github.com/) com:

``` r

# install.packages("devtools")
devtools::install_github("Natanaelsl/RREORGFdataR", build_vignettes = TRUE)
```

## Exemplo de Uso

Abaixo, um exemplo de como extrair dados em lote e salvar diretamente
para um pipeline de Data Lake:

``` r

library(RREORGFdataR)

# 1. Extração eficiente de dados do RGF (Estados, 2020-2023, 3º quadrimestre)
dados_rgf <- RGFdata(
  cod.ibge = 52,
  year = 2020:2023,
  power = c('E', 'L', 'J', 'D', 'M'),
  period = 3,
  annex = 1
)

# 2. Pipeline de Data Lake: Extração massiva e persistência em Parquet
RREOdata(
  cod.ibge = "all_states",
  year = 2024,
  period = 1:6,
  annex = 1,
  save_path = "data/rreo_estados_2024.parquet"
)
```

## Informações

### Sobre a API SICONFI

O SICONFI é a fonte oficial dos dados contábeis e fiscais do setor
público brasileiro. Este pacote abstrai a complexidade da API REST,
tratando automaticamente a tradução de colunas temporais (formatos
`<MR-X>`) para objetos de data nativos do R, facilitando análises
imediatas.

### Código das instituições

A tabela abaixo apresenta os códigos do IBGE e do Siconfi compatíveis
com a API, bem como a respectiva instituição. Disponível em:
<https://siconfi.tesouro.gov.br/siconfi/pages/public/arquivo/conteudo/Cod_instituicoes_siconfi.pdf>

  
