
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ckbplotr <img src="man/figures/logo.png" align="right" width = "120" />

<!-- badges: start -->

[![R build
status](https://github.com/neilstats/ckbplotr/workflows/R-CMD-check/badge.svg)](https://github.com/neilstats/ckbplotr/actions)
[![DOI](https://zenodo.org/badge/189028664.svg)](https://zenodo.org/badge/latestdoi/189028664)
<!-- badges: end -->

`ckbplotr` provides functions to help create and style plots in R. It is
developed by, and primarily for, [China Kadoorie
Biobank](http://www.ckbiobank.org) researchers.

## Key features

The package can be used to apply CKB style and to create plots by
generating ggplot2 code. Functions that create plots (such as
`shape_plot()` and `forest_plot()`) return both:

- **A ggplot2 plot**.
- **The ggplot2 code used to create the plot**, allowing users to see
  exactly how the plot has been created and adapt the code for other
  uses. In RStudio the code will also be shown in the Viewer pane.

## Installation

Install the latest version of `ckbplotr` from the neilstats R-universe:

``` r
install.packages('ckbplotr',
                 repos = c('https://neilstats.r-universe.dev',
                           'https://cloud.r-project.org'))
```

This will also install dependencies from the CRAN repository.

## Get started

Read `vignette("ckbplotr")` to get started.

## Citing ckbplotr

If you find this package useful, please consider citing as:

Wright N (2023). ckbplotr: Create CKB Plots.
<https://neilstats.github.io/ckbplotr/>,
<https://doi.org/10.5281/zenodo.6382217>.

## Package development

This package is under development. If you find an error or bug or have a
suggestion for improvement please [create an issue on
GitHub](https://github.com/neilstats/ckbplotr/issues) or contact the
author at
[@NeilStats@fediscience.org](https://fediscience.org/@neilstats).
