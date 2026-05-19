# Extração da Lista de Instituições e Códigos SICONFI

**\[stable\]**

Retorna o dicionário de dados contendo os códigos do IBGE e do SICONFI
das instituições federativas compatíveis com a API do Tesouro Nacional.
Permite visualizar a base empacotada em formato `data.frame` ou realizar
o download do arquivo oficial em PDF diretamente do site do Tesouro.

## Usage

``` r
siconfi_list(action = c("view", "download"), dest_dir = getwd())
```

## Arguments

- action:

  Caractere definindo a ação da função. Aceita `"view"` (padrão) para
  carregar e retornar o `data.frame` contendo a base de dados interna do
  pacote, ou `"download"` para baixar o arquivo PDF oficial do Tesouro
  Nacional.

- dest_dir:

  Caminho de diretório (caractere) indicando onde o arquivo PDF deve ser
  salvo caso `action = "download"`. O padrão é o diretório de trabalho
  atual ([`getwd()`](https://rdrr.io/r/base/getwd.html)). Ignorado se
  `action = "view"`.

## Value

Se `action = "view"`, retorna um objeto `data.frame` com os códigos. Se
`action = "download"`, salva o arquivo `.pdf` no diretório especificado
e retorna o caminho completo do arquivo invisivelmente
([`invisible()`](https://rdrr.io/r/base/invisible.html)).

## Examples

``` r
if (FALSE) { # \dontrun{
# 1. Visualizar o dicionário de códigos no console (retorna o data.frame)
df_codigos <- siconfi_list(action = "view")
head(df_codigos)

# 2. Baixar o arquivo PDF oficial para a pasta atual (Working Directory)
siconfi_list(action = "download")

# 3. Baixar o PDF para uma pasta específica do projeto (Cross-platform)
siconfi_list(
  action = "download",
  dest_dir = "C:/meus_dados/referencias"
)
} # }
```
