
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geobox

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

O objetivo do pacote *geobox* é permitir a manipulação reativa de
modelos de regressão linear. Assim, ao optar-se pela transformação de
escala de uma variável ou pela habilitação/desabilitação de um dado
(e/ou variável), os índices estatísticos referentes à aderência do
modelo à realidade são recalculados. Portanto, é possível verificar, em
tempo real, a influência das informações inseridas.

O pacote permite ainda a definição de elementos urbanos
georreferenciados em seu mapa interno. A reprodução desses objetos no
meio digital conduz à criação interativa de variáveis de localização que
podem influenciar a equação que espelha a realidade.

O *geobox* pode ser utilizado para a modelagem (por regressão linear) de
dados de qualquer natureza. Contudo, a primeira versão do programa foi
adaptada à NBR 14.653, que versa sobre a avaliação de imóveis.
Especificamente em seu Anexo A, essa norma congrega premissas e testes
estatísticos relativos à definição da regressão linear e/ou a suas boas
práticas.

## Instalação

A versão em desenvolvimento pode ser instalada por meio do
[GitHub](https://github.com/brunorzm/geobox):

``` r
install.packages("devtools")
devtools::install_github("brunorzm/geobox")
```

Ainda é possível o compartilhamento do pacote zipado (na extensão
*.tar.gz*) e instalação local, por meio das funções:

``` r
install.packages("devtools")
devtools::install_local("pasta/do/arquivo")
```

## Para executar o GEOBOX

Uma vez instalado, basta um único comando para executar a ferramenta:

``` r
geobox::iniciar_geobox()
```
