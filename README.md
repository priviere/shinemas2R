# shinemas2R

`shinemas2R` is a R package that  queries the database SHiNeMaS ([Seeds History and Network Management System](https://sourcesup.renater.fr/frs/?group_id=2295)) and format data for specific R packages.

## Install

```r
library(devtools)
devtools::install_github("priviere/shinemas2R")
```

## Usage

`shinemas2R` has one function `shinemas()` which queries SHiNeMaS and formats data for specific R packages.
The query is based on a webservice developped within SHiNeMaS v2 by Laetitia Courgey and Yannick de Oliveira from [ABI-Soft](http://moulon.inrae.fr/equipes_transversales/abi/abisoft/) team at GQE-Le Moulon.
The following information are needed to connect to the database:  db_url, user, password and token.
The token can be taken through the web interface of SHiNeMaS by cliking "Get Token" on the top left side of the page.

## Packages supported

### PPBstats

[PPBstats](https://priviere.github.io/PPBstats_web_site/) is a R package for Participatory Plant Breeding statisticial analyses. 

Four queries are supported:

- `PPBstats_data_network_unipart_seed_lots`, for network analysis on
[unipart network for seed lots analysis](https://priviere.github.io/PPBstats_web_site/book/unipart-network-for-seed-lots-analysis.html#format-the-data), 
[unipart network for location analysis](https://priviere.github.io/PPBstats_web_site/book/unipart-network-for-location-analysis.html#format-the-data-1) and 
[bipart network analysis](https://priviere.github.io/PPBstats_web_site/book/bipart-network-analysis.html#format-the-data-2).
- `PPBstats_data_agro`, for [agronomic analysis](https://priviere.github.io/PPBstats_web_site/book/intro-agro.html#data-agro)
- `PPBstats_data_agro_SR`, for [response to selection analysis](https://priviere.github.io/PPBstats_web_site/book/family-4.html#response-to-selection)
- `PPBstats_data_agro_HA`, for [local adaptation analysis](https://priviere.github.io/PPBstats_web_site/book/family-4.html#study-local-adaptation)


## Examples


For example, first get the formated data set
```r
library(shinemas2R)
data_agro = shinemas2R::shinemas(
  db_url = "shinemas.local.seed",
  user = "wheat",
  password = "ppb",
  token = "1234",
  query = "PPBstats_data_agro"
  )
```

and then transform if for `PPBstats` format.

```r
library(PPBstats)
data = PPBstats::format_data_PPBstats(data_agro, type = "data_agro")
```



