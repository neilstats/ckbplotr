# Fix panel width and height of a forest plot

**\[deprecated\]**

ckb_style(), plot_like_ckb() and shape_plot() have width and height
arguments and forest_plot() has panel.width and panel.height arguments.
These use ggh4x::force_panelsizes() to fix panel sizes.

## Usage

``` r
fix_panel(plot, width = NULL, height = NULL)
```

## Arguments

- plot:

  A plot (created by forest_plot()).

- width:

  Width of panels. (e.g unit(50, "mm"))

- height:

  Height of panels. (e.g unit(150, "mm"))

## Value

A gtable object
