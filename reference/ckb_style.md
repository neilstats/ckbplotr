# Make a ggplot into CKB style

Make a ggplot into CKB style

## Usage

``` r
ckb_style(
  xlims = NULL,
  ylims = NULL,
  gap = c(0.025, 0.025),
  ext = c(0, 0),
  ratio = 1.5,
  width = NULL,
  height = NULL,
  base_size = 11,
  base_line_size = base_size/22,
  ink = "black",
  paper = "white",
  colour = NULL,
  axis.title.margin = 1,
  plot.margin = margin(0.5, 1.5, 0.5, 0.5, "lines"),
  clip = "on"
)
```

## Arguments

- xlims:

  A numeric vector of length two. The limits of the x-axis.

- ylims:

  A numeric vector of length two. The limits of the y-axis.

- gap:

  A numeric vector of length two. The gap between plotting area and axis
  to the left and bottom of the plot, as a proportion of the x-axis
  length. (Default: c(0.025, 0.025))

- ext:

  A numeric vector of length two. The extensions to add to the right and
  top of the plot, as a proportion of the x-axis length. (Default: c(0,
  0))

- ratio:

  The ratio (y-axis:x-axis) to use for the plot. Ignored if both width
  and height are set. (Default: 1.5)

- width:

  A [`grid::unit`](https://rdrr.io/r/grid/unit.html) object to set the
  width of the plot (not including the gap or extension).

- height:

  A [`grid::unit`](https://rdrr.io/r/grid/unit.html) object to set the
  height of the plot (not including the gap or extension).

- base_size:

  Base font size, given in pts. (Default: 11)

- base_line_size:

  Base size for line elements. (Deault: base_size/22)

- ink, paper:

  Colour for foreground and background elements. (Defaults: "black" and
  "white")

- colour:

  Deprecated. Use `ink` instead.

- axis.title.margin:

  Margin between axis titles and plot. (Default: 1)

- plot.margin:

  Margin around entire plot (Default: margin(0.5, 0, 0.5, 0, "lines"))

- clip:

  Passed to clip argument of
  [`ggplot2::coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html).
  (Default: "on")
