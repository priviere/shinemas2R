# shinemas2R

`shinemas2R` is an R package that  queries the database SHiNeMaS ([Seeds History and Network Management System](https://sourcesup.renater.fr/frs/?group_id=2295)) and format data for specific R packages.

The R code is under licence GPL-3.

The copyright of the R code and the vignette are owned by Réseau Semences Paysannes and Institut National de la Recherche Agronomique

Note that an old verson of shinemas2R code is [here](https://github.com/priviere/shinemas2R_deprecated)

## Install

```r
devtools::install_github("priviere/shinemas2R")
```

## Try

`shinemas2R` has one function `shinemas()` which queries SHiNeMaS and formats data for specific R packages.
See `?shinemas` for more details.

Four queries give four formats for the R package [PPBstats](https://priviere.github.io/PPBstats_web_site/index.html)^[See [here](https://priviere.github.io/PPBstats_web_site/download.html) to install PPBstats].

For example, first get the formated data set
```r
library(shinemas2R)
data_agro = shinemas2R::shinemas(
  db = "shinemas.local.seed",
  id = "user",
  pwd = "toto",
  query = "PPBstats_data_agro"
  )
```

and then transform if for [`PPBstats` format](https://priviere.github.io/PPBstats_book/intro-agro.html#data-agro).

```r
library(PPBstats)
data = PPBstats::format_data_PPBstats(data_agro, type = "data_agro")
```

in order to apply the [`PPBstats` functions](https://priviere.github.io/PPBstats_book/intro-agro.html#workflow-agro) regarding your analysis.






