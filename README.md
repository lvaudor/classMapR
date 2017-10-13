# classMapR

an R-shiny application for the unsupervised hierarchical clustering of pixels in multilayered rasters.    

This classification is based on a hierarchical approach coupled to the calculation of the Gower's distance between pixels.

```r
classMapR()
```

## Installation

For now there is no stable version of this package on CRAN. If you want to install the dev version, make sure you have a very recent version of R (>3.4.0) and run:

```r
if(!require(devtools)){install.packages("devtools")}
devtools::install_github("lvaudor/classMapR")
```

The devtools::install_github command might fail due to connection issues if you access the internet through a proxy. If so, get your proxy settings from your organizations's IT services and run the following:

```r
library(httr)
set_config(use_proxy(url="xxxx",port=XXXX))
```

or, if your proxy requires authentification:

```r
library(httr)
set_config(use_proxy(url="xxxx",port=XXXX,username="xxxx",password="xxxx"))
```

## Use

To launch classMapR from your R console simply run:

```r
library(classMapR)
classMapR()
```

If when launching classMapR you get an error message like "The requested URL could not be retrieved" please check your internet explorer options, and make sure you are not using a proxy for your local connection. 
