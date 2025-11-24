# ckbplotr

[![R build
status](https://github.com/neilstats/ckbplotr/workflows/R-CMD-check/badge.svg)](https://github.com/neilstats/ckbplotr/actions)
[![runiverse-name](https://neilstats.r-universe.dev/badges/:name)](https://neilstats.r-universe.dev)
[![runiverse-package](https://neilstats.r-universe.dev/badges/ckbplotr)](https://neilstats.r-universe.dev/ckbplotr)
[![DOI](https://zenodo.org/badge/189028664.svg)](https://zenodo.org/badge/latestdoi/189028664)

Use `ckbplotr` to create and style publication-quality plots. The
package is focused on plots for epidemiology and is developed by a
[China Kadoorie Biobank](http://www.ckbiobank.org) and [Oxford
Population Health](https://www.ndph.ox.ac.uk/) researcher.

## Installation

Install the latest version of `ckbplotr` from the neilstats R-universe:

``` r
install.packages('ckbplotr',
                 repos = c('https://neilstats.r-universe.dev',
                           getOption('repos')))
```

## Get started

Read
[`vignette("ckbplotr")`](https://neilstats.github.io/ckbplotr/articles/ckbplotr.md)
to get started.

Additional examples and tutorials for using the package can be found at
<https://neilstats.github.io/ckbplotr-book>.

## Key features

The package can be used to apply CKB style and to create plots by
generating ggplot2 code. Functions that create plots (such as
[`shape_plot()`](https://neilstats.github.io/ckbplotr/reference/shape_plot.md)
and
[`forest_plot()`](https://neilstats.github.io/ckbplotr/reference/forest_plot.md))
return both:

- **A ggplot2 plot**.
- **The ggplot2 code used to create the plot**, allowing users to see
  exactly how the plot has been created and adapt the code for other
  uses. In RStudio the code will also be shown in the Viewer pane.

## Citing ckbplotr

If you find this package useful, please consider citing as:

Wright N (2025). ckbplotr: Create Plots.
<https://neilstats.github.io/ckbplotr>,
<https://doi.org/10.5281/zenodo.6382217>.

## Package development

This package is under development. If you find an error or bug or have a
suggestion for improvement please [create an issue on
GitHub](https://github.com/neilstats/ckbplotr/issues) or contact the
author at
[@NeilStats@fediscience.org](https://fediscience.org/@neilstats).
