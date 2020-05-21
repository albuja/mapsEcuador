
<!-- README.md is generated from README.Rmd. Please edit that file -->
mapsEcuador
-----------

The goal of mapsEcuador package is provide simple functions to create printer ready Ecuador choropleth maps with your own data over a full customizable template. 

No GIS knowledge is required, just your data and basic R. Geographic data is internally used inside the package.

Installation
------------

You can install mapsEcuador from github with:

``` r
if (!require("devtools")) {
  install.packages("devtools")
  library("devtools")
}
devtools::install_github("albuja/mapsEcuador")
```

Example
--------

This is a simple example which shows you how to get a printer ready PDF Ecuador choropleth map with yor own data:

``` r
library(mapsEcuador)

# Ecuador choropleth map with dummy data

indicador <- runif(24, 10, 1000)
names(indicador) <- 1:24

coropleth_map_prov_pdf(values = indicador,
                       bins = 3,
                       map_title = 'Indicador ABC en Ecuador',
                       filename = 'mapa.pdf',
                       legend_title = 'Indicador')

```
