
<!-- README.md is generated from README.Rmd. Please edit that file -->
mapsEcuador
-----------

mapsEcuador R package provide simple functions to create printer ready PDF Ecuador maps with your own data over a full customizable template. 

No GIS knowledge is required. You need just provide data and geographic data will be internally added.

Base map shape files were downloaded from:
https://www.ecuadorencifras.gob.ec/documentos/web-inec/Cartografia/Clasificador_Geografico/2015/

Installation
------------

You can install mapsEcuador R package from github with:

``` r
if (!require("devtools")) {
  install.packages("devtools")
  library("devtools")
}
devtools::install_github("albuja/mapsEcuador")
```

Example
--------

This is simplest code needed to get a printer ready PDF Ecuador choropleth map with your own data, using all function default values.

``` r
library(mapsEcuador)

# Ecuador choropleth map with dummy data

values <- round(runif(24, 10, 1000), 3)
names(values) <- 1:24
choropleth_map_prov_pdf(values)

```
