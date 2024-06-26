
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RREORGFdataR <a href="https://natanaelsl.com.br"><img src="man/figures/Logo_2.png" align="right" width="138" alt="RREORGFdataR_2 website" class="logo" style="margin-right: 5px;" /><img src="man/figures/logo.png" align="right" width="138" alt="RREORGFdataR_1 website" class="logo" /></a>

<!-- badges: start -->
<!-- [![CRAN/METACRAN Version](https://www.r-pkg.org/badges/version/geouy)](https://CRAN.R-project.org/package=geouy) -->
<!-- [![CRAN/METACRAN Total downloads](https://cranlogs.r-pkg.org/badges/grand-total/geouy?color=blue)](https://CRAN.R-project.org/package=geouy)  -->
<!-- [![CRAN/METACRAN downloads per month](https://cranlogs.r-pkg.org/badges/geouy?color=orange)](https://CRAN.R-project.org/package=geouy) -->
<!-- <br /> -->
<!-- [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/RichDeto/geouy?branch=master&svg=true)](https://ci.appveyor.com/project/RichDeto/geouy) -->
<!-- [![R](https://github.com/Natanaelsl/NCAGEDdataR/actions/workflows/r.yml/badge.svg)](https://github.com/Natanaelsl/NCAGEDdataR/actions/workflows/r.yml) -->

[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
![GitHub R package version (subdirectory of
monorepo)](https://img.shields.io/github/r-package/v/Natanaelsl/RREORGFdataR)
![GitHub
License](https://img.shields.io/github/license/Natanaelsl/RREORGFdataR)
![GitHub Repo
stars](https://img.shields.io/github/stars/Natanaelsl/pagedreport?color=orange)
<!-- badges: end -->

## Visão geral

**RREORGFdataR** é um pacote R que permite aos usuários acessar
facilmente o conjunto de dados do Relatório Resumido da Execução
Orçamentária (RREO) e do Relatório de Gestão Fiscal (RGF) utilizando a
API do SICONFI (<https://apidatalake.tesouro.gov.br/docs/siconfi/>).
Saiba mais sobre a utilização do ‘RREORGFdataR’ em
<https://natanaelsl.com.br/project/rreorgf_pkg/> ou
<https://natanaelsl.github.io/RREORGFdataR/>.

<!-- <img align="right" src="man/figures/Logo_1.png" alt="logo" width="180"><img align="right" src="man/figures/Logo_2.png" alt="logo" width="180"> -->

- `RREOdata`: Realiza a extração dos dados do Relatório Resumido da
  Execução Orçamentária (RREO) de maneira mais intuitiva e fácil
  utilizando a API do
  [SICONFI](https://apidatalake.tesouro.gov.br/docs/siconfi/)

- `RGFdata`: Realiza a extração dos dados do Relatório de Gestão Fiscal
  (RGF) de maneira mais intuitiva e fácil utilizando a API do
  [SICONFI](https://apidatalake.tesouro.gov.br/docs/siconfi/).

- `siconfi_list`: Opção padrão `NULL` retorna um `data.frame` contendo
  os códigos do IBGE e do Siconfi disponíveis para a API, juntamente com
  as instituições correspondentes. A escolha da opção `options = down`
  permite o download do arquivo (.pdf) disponibilizado pelo Tesouro
  Nacional.

<!-- <br /> -->

## Instalação

Você pode instalar a versão de desenvolvimento do `RREORGFdataR` no
[GitHub](https://github.com/) com:

``` r
# install.packages("devtools")
devtools::install_github("Natanaelsl/RREORGFdataR", build_vignettes = TRUE)
```

<!-- --- -->

## Exemplo

Este é um exemplo básico que mostra como resolver um problema comum:

``` r
## Carregando o pacote
library(RREORGFdataR)

## Download dos dados do Relatório Resumido da Execução Orçamentária (RREO).
# RREOdata()

## Extraindo dados do anexo 1 para o 3º quadrimestre do RGF de 2020 até 2023
## do Estado de Goiás para todos os poderes.
RGFdata(cod.ibge = 52,
        year = 2020:2023,
        power = c('E','L','J','D','M'),
        period = 3,
        annex = 1,
        simplified = FALSE)
        
## Extraindo dados do anexo 1 para o 3º quadrimestre do RGF de 2023
## de todos as UF's para todos os poderes.
RGFdata(cod.ibge = "all_states",
        year = 2023,
        power = c('E','L','J','D','M'),
        period = 3,
        annex = 1,
        simplified = FALSE)
```

<!-- --- -->

## Informações

### API SICONFI

O Tesouro Nacional disponibilizou a Application Programming Interface
(API) de dados abertos para atender à demanda por dados brutos oriundos
do Sistema de Informações Contábeis e Fiscais do Setor Público
Brasileiro – Siconfi. Por meio dessa ferramenta, é possível ao usuário
obter desde pequenas frações até grandes volumes de dados de todas as
informações inseridas pelos entes subnacionais no Siconfi.

### Vantagem

A sintaxe da função `RREORGFdataR` opera com a mesma lógica
independentemente da base de interesse, o que torna intuitivo o
download/extração de qualquer conjunto de dados usando uma única linha
de código.

### Código das instituições

A tabela abaixo apresenta os códigos do IBGE e do Siconfi compatíveis
com a API, bem como a respectiva instituição. Disponível em:
<https://siconfi.tesouro.gov.br/siconfi/pages/public/arquivo/conteudo/Cod_instituicoes_siconfi.pdf>

<br />

> ***NOTA:*** O referido pacote está em fase de construção podendo não
> ter todas as informações disponíveis para utilização.
