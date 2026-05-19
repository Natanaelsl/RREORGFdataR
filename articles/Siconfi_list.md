# siconfi_list: Gestão de Cadastros e Dicionários

## Introdução

Esta vinheta descreve o uso da função auxiliar
[`siconfi_list()`](https://natanaelsl.github.io/RREORGFdataR/reference/siconfi_list.md).
Seu objetivo é servir como uma tabela de dimensões ou dicionário de
consulta essencial para parametrização dos relatórios do SICONFI.

Para que as funções de extração
[`RGFdata()`](https://natanaelsl.github.io/RREORGFdataR/reference/RGFdata.md)
e
[`RREOdata()`](https://natanaelsl.github.io/RREORGFdataR/reference/RREOdata.md)
funcionem corretamente, o usuário deve informar o código numérico
estruturado pelo IBGE. Embora a memorização de códigos estaduais seja
simples (2 dígitos), rastrear os códigos municipais de 7 dígitos exige
tabelas de referência estáveis.

A função
[`siconfi_list()`](https://natanaelsl.github.io/RREORGFdataR/reference/siconfi_list.md)
resolve esse gargalo operando de duas maneiras: ela pode realizar uma
leitura offline instantânea de uma base local embutida no pacote (modo
`"view"`) ou efetuar o download seguro do manual metodológico em formato
PDF atualizado diretamente do Tesouro Nacional (modo `"download"`),
operando de forma 100% cross-platform.

Para iniciar, carregamos as ferramentas auxiliares:

``` r

library(RREORGFdataR)
library(dplyr)
```

## Exemplo 1: Carregamento e Consulta Offline (`action = "view"`)

O modo padrão `"view"` realiza o mapeamento direto sobre um arquivo RDS
embutido no pacote. Esse método não gera tráfego de rede, sendo ideal
para painéis de busca rápidos dentro de aplicações Shiny corporativas.

``` r

# Carrega o dicionário de instituições na sessão ativa
base_siconfi <- siconfi_list(action = "view")

# Buscando o código IBGE do município de 'Aparecida de Goiânia'
codigo_busca <- base_siconfi %>%
  filter(grepl("Aparecida de Goiânia", instituicao, ignore.case = TRUE)) %>%
  select(codigo_ibge, instituicao, uf)

print(codigo_busca)
```

## Exemplo 2: Download do Catálogo Técnico Oficial (`action = "download"`)

Caso o usuário necessite dispor do PDF original para auditoria legal ou
validação normativa, a função faz o download direto da URL do Tesouro de
forma programática, contornando a necessidade de abertura manual de
navegadores web.

``` r

# Realiza o download e grava no diretório de trabalho corrente
caminho_salvo <- siconfi_list(action = "download", dest_dir = getwd())

# O caminho físico completo é retornado invisivelmente para automações
message("Arquivo persistido em: ", caminho_salvo)
```

## Exemplo 3: Enriquecimento de Dados (Lookup Join)

Você pode utilizar a saída do
[`siconfi_list()`](https://natanaelsl.github.io/RREORGFdataR/reference/siconfi_list.md)
para enriquecer os outputs brutos extraídos da API, convertendo os
códigos de entes em nomes amigáveis de municípios ou regiões para
confecção de relatórios gerenciais finais.

``` r

# Exemplo de enriquecimento estrutural (Lookup)
base_cadastros <- siconfi_list(action = "view")

# Supondo que 'meus_dados_rreo' possui a coluna 'id_ente' vinda da API:
# dados_enriquecidos <- meus_dados_rreo %>%
#   left_join(base_cadastros, by = c("id_ente" = "codigo_ibge"))
```
