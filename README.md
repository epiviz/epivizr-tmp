epivizR
========

## Interactive communication between `epiviz` and R/Bioconductor Sessions

The epivizr package implements two-way communication between the [R/Bioconductor](http://bioconductor.org) environment and [epiviz](http://epiviz.cbcb.umd.edu). Objects in the R environment can be displayed as tracks or scatterplots on Epiviz. Epivizr uses Websockets for communication between the browser Javascript client and the R environment using the same technology underlying the popular [Shiny](http://www.rstudio.com/shiny) system for authoring interactive web-based reports in R.

 

## Installation
Epivizr is available as an R package. We have extended some of the underlying Bioconductor code in the `IRanges` and `GenomicRanges` packages for more efficient performance. These will be merged to the Bioconductor codebase soon. In the meantime, we require that our versions of these packages are installed. Please follow the following directions within your R session to install epivizr. 

**YOU ARE USING DEVELOPMENT SOFTWARE, USE AT YOUR OWN PERIL.**

```{r}
install.packages("devtools")
library(devtools)
install_github("IRanges",user="hcorrada")
install_github("GenomicRanges",user="hcorrada")
install_github("epivizr", user="epiviz")
```

## Try it out

The easiest way to try it `epivizr` out is to follow the package vignette:

```{r}
# in R
browseVignettes("epivizr")
```

Or take a look on here: ADD LINK

```