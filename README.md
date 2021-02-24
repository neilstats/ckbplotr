
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ckbplotr

`ckbplotr` provides functions to help create and style plots in R. It is
being developed by, and primarily for, [China Kadoorie
Biobank](http://www.ckbiobank.org) researchers.

## Installation

### Directly from github

The latest version of `ckbplotr` can be installed directly from github
using the `remotes` package.

``` r
install.packages('remotes')
remotes::install_github('neilstats/ckbplotr')
```

If you get an error that reads “Error: Failed to install ‘unknown
package’ from GitHub: HTTP error 404. No commit found for the ref
master” then make sure to update to the latest version of the `remotes`
package, or try `remotes::install_github('neilstats/ckbplotr@main')`.

If you get an error that reads “Error: (converted from warning) package
‘ggplot2’ was built under R version …” you can avoid this by first
running `Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS" = "true")`.
(This is a [known issue](https://github.com/r-lib/remotes/issues/403)
with the `remotes` package.)

### Or from source package

`ckbplotr` can also be installed from its source package. The R packages
`ggplot2`, `magrittr`, `readr`, `tibble`, `dplyr`, `purrr` and `rlang`
must first be installed. These are part of the collection of tidyverse
packages.

``` r
# The easiest way is to install the whole tidyverse:
install.packages("tidyverse")

# # Or install just these packages:
# install.packages(c("ggplot2", "readr", "dplyr", "purrr"))
```

Then `ckbplotr` can be installed from its source package using the code:

``` r
install.packages("ckbplotr.tar.gz", repos = NULL, type = "source")
```

Or, in RStudio, open the “Tools” menu and select “Install Packages…”. In
the “Install from…” box select “Package Archive File”, and in the
“Package archive” box browse to the ckbplotr.tar.gz file.

The source package for the latest release version is available
[here](https://github.com/neilstats/ckbplotr/releases/latest).

## The plot\_like\_ckb function

The `plot_like_ckb` function does three things to a ggplot2 plot:

1.  applies a CKB theme (i.e. change the overall appearance)
2.  extends the plotting area and manually adds axis lines (so that you
    can have a custom sized gap between the plotting area and the axes)
3.  applies a fixed aspect ratio

<img src="man/figures/README-a-plot-1.png" style="display: block; margin: auto;" />

## The make\_shape\_plot function

The `make_shape_plot` function creates a plot of estimates and CIs
against risk factor levels using the `ggplot2` package.

<img src="man/figures/README-make_shape_plot-example-1-1.png" style="display: block; margin: auto;" />

## The make\_forest\_plot function

The `make_forest_plot` function creates a forest plot using the
`ggplot2` graphics package. The function returns a named list
containing:

-   plot: the plot
-   code: ggplot2 code to generate the plot

In RStudio the ggplot2 code used to generate the plot will be shown in
the ‘Viewer’ pane. If modifications are needed to the plot, then this
code can be copied, edited, and run as needed.

<img src="man/figures/README-example-forest-plot-1.png" style="display: block; margin: auto;" />

### Expected warning

You may get the following warning when using this function.

> Vectorized input to `element_text()` is not officially supported.
> Results may be unexpected or may change in future versions of ggplot2.

## make\_jasper\_forest\_plot function

The `make_jasper_forest_plot` function requires the in-house Jasper
package, which is not publicly available.
