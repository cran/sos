
<!-- README.md is generated from README.Rmd. Please edit then `knit` that file -->

# sos

<!-- badges: start -->

[![R-CMD-check](https://github.com/sbgraves237/sos/workflows/R-CMD-check/badge.svg)](https://github.com/sbgraves237/sos/actions)
<!-- badges: end -->

The `sos` package provides the fastest literature search I know for
anything statistical. It queries the `RSiteSearch` database and sorts
the results by package not just help page. The package includes a
vignette reprinted from [*The R
Journal*](https://journal.r-project.org/archive/2009/RJ-2009-017/RJ-2009-017.pdf).

## Installation

You can install the released version of sos from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("sos")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sbgraves237/sos")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(sos)
#> Loading required package: brew
#> 
#> Attaching package: 'sos'
#> The following object is masked from 'package:utils':
#> 
#>     ?
(PL <- findFn('Petal.Length'))
#> found 90 matches;  retrieving 5 pages
#> 2 3 4 5 
#> Downloaded 100 links in 63 packages.
#> Warning in file(templateFile, encoding = "utf-8", open = "r"): file("") only
#> supports open = "w+" and open = "w+b": using the former
#> Warning in print.packageSum(packageSum(x, title = titSum, ...)): Brew created a
#> file of size 0
#> Ignoring template.
```

The `print` method for an object of class `findFn`, like `PL`, opens two
taps in the default browser: The first has links to individual help
pages sorted by package. The second is a package summary.

The current version of `sos` extracts some information only from
installed packages. You can install the leading packages in a search as
follows:

``` r
installPackages(PL)
PL
#> Warning in file(templateFile, encoding = "utf-8", open = "r"): file("") only
#> supports open = "w+" and open = "w+b": using the former
#> Warning in print.packageSum(packageSum(x, title = titSum, ...)): Brew created a
#> file of size 0
#> Ignoring template.
```
