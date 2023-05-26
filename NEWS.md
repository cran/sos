# sos 2.1-6

* Fix problems identified in CRAN checks and add `try` to `*.Rd` files, so the package does not fail without adequate Internet support. 2023-05-24.  

# sos 2.1-1

* Fix an intermittent bug giving "Error in `data.frame(Package = pac, Function = fun, Date = as.POSIXct(Date,`  : arguments imply differing number of rows: 0, 1" in certain circumstances.  2022-03-29.  

# sos 2.1-0

* Update to fix problems with R 4.1.0, especially `???spline(2)` no longer recognized `maxPages`=2 and took the default instead.  Fixed. 2021-06-14.  

# sos 2.0-2

* Delete inappropriate file and directory. 2020-10-19 

# sos 2.0-1

* Fix CRAN check problems.  2020-10-18.

# sos 2.0-0

* print.findFn produces 2 pages on the default web browser:  one row per (1) matching help page and (2) package with a match.  

# sos 1.0-5

* RSiteSearch package name changed to `sos`

# sos 1.0-4

* Make `PackageSum2` generic to easily obtain an enhanced summary adding information from installed packages

# sos 1.0-3

* Add binary operators & and | as shorthands for `unionRSiteSearch` and `intersectRSiteSearch`

# sos 1.0-2

* Add `hits` function

# sos 1.0-1

* Fix a minor bug in `unionRSiteSearch`.

# sos 1.0-0

* Add `unionRSiteSearch`, `intersectRSiteSearch` and `PackageSum2`

# sos 0.1-6

* Add function `PackageSummary` to compute the summary
previously done inside `RSiteSearch.function`, converting it
to a `data.frame` and adding the `Date`.

# sos 0.1-5
* With zero hits, `RSiteSearch.function` still returns
an object of class `c('RSiteSearch', 'data.frame')`;
previously, it returned only a `data.frame` with zero rows.