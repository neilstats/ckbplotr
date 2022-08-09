
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ckbplotr <img src="man/figures/logo.png" align="right" width = "120" />

<!-- badges: start -->

[![R build
status](https://github.com/neilstats/ckbplotr/workflows/R-CMD-check/badge.svg)](https://github.com/neilstats/ckbplotr/actions)
[![Codecov test
coverage](https://codecov.io/gh/neilstats/ckbplotr/branch/main/graph/badge.svg)](https://codecov.io/gh/neilstats/ckbplotr?branch=main)
[![DOI](https://zenodo.org/badge/189028664.svg)](https://zenodo.org/badge/latestdoi/189028664)
<!-- badges: end -->

`ckbplotr` provides functions to help create and style plots in R. It is
being developed by, and primarily for, [China Kadoorie
Biobank](http://www.ckbiobank.org) researchers.

*This package is under development. If you find an error or bug or have
a suggestion for improvement please create an issue on GitHub or contact
the author at <neil.wright@ndph.ox.ac.uk> or
[@NeilStats](https://twitter.com/NeilStats).*

It can be used to: create plots of estimates and CIs against risk factor
levels…
<img src="man/figures/README-make_shape_plot-example-1.png" width="50%" style="display: block; margin: auto;" />

…create forest plots…
<img src="man/figures/README-example-forest-plot-1.png" width="90%" style="display: block; margin: auto;" />

…and convert other ggplots to CKB style.
<img src="man/figures/README-a-plot-1.png" width="80%" style="display: block; margin: auto;" />

## Installation

### From R-universe

The latest version of `ckbplotr` can be installed from the neilstats
R-universe using `install.packages()`.

``` r
install.packages('ckbplotr',
                 repos = c('https://neilstats.r-universe.dev', 'https://cloud.r-project.org'))
```

This will also install dependencies from the CRAN repository.

### From github

The latest version of `ckbplotr` can be installed from github using the
`remotes` package.

``` r
install.packages('remotes')
remotes::install_github('neilstats/ckbplotr')
```

## Get started

Read `vignette("ckbplotr")` to see how to use the `make_shape_plot()`,
`make_forest_plot()`, and `plot_like_ckb()` functions.

## ggplot2 code

The `make_shape_plot()` and `make_forest_plot()` functions return both a
plot and the ggplot2 code used to create the plot. In RStudio the
ggplot2 code used to create the plot will be shown in the Viewer pane
(with syntax highlighting if the
[highlights](https://cran.r-project.org/package=highlight) package is
installed).
