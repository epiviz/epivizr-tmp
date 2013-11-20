epivizR
========

The `epivizr` Bioconductor package implements two-way communication between the [R/Bioconductor](http://bioconductor.org) environment and the [epiviz](http://epiviz.cbcb.umd.edu) web app for interactive data visualization. Objects in the R environment can be displayed as tracks or scatterplots on Epiviz. Epivizr uses Websockets for communication between the browser Javascript client and the R environment using the same technology underlying the popular [Shiny](http://www.rstudio.com/shiny) system for authoring interactive web-based reports in R.

 
## Installation and requirements
Epivizr is available as part of the [Bioconductor](http://bioconductor.org) project as of version 2.13 which requires R version 3.0.2. Make sure you [install](http://cran.r-project.org)
the proper version before continuing. To install `epivizr`:

```{r}
source("http://bioconductor.org/biocLite.R")
biocLite("epivizr")
```

## Try it out

The easiest way to try `epivizr` out is to follow the package vignette:

```{r}
require(epivizr)
browseVignettes("epivizr")
```

## A quick tour

You can get a quick tour of epiviz here: [http://youtu.be/099c4wUxozA](http://youtu.be/099c4wUxozA)

## Development version

This github repository contains the latest and greatest version of `epivizr` and is tracked by the devel version in Bioconductor (see
[http://bioconductor.org/developers/how-to/useDevel/](http://bioconductor.org/developers/how-to/useDevel/) for more info.

## More info

[Check out the `epiviz` project page on github](http:://github.com/epiviz)


