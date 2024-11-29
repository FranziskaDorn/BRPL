
<!-- README.md is generated from README.Rmd. Please edit that file -->

# socialsust

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/bivquant)](https://CRAN.R-project.org/package=bivquant)
<!-- badges: end -->

The goal of socialsust is to …

## Installation

You can install the development version of socialsust from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RMKruse/bivarte-discriminant-package")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
data("nutritionpoverty")
bivqfun(data = nutritionpoverty, var1="NUval", var2="INval", tau = 0.25)
```

<img src="man/figures/README-example-1.png" width="100%" />

    #> [1] "Funktioniert."
