# Make a shape plot with ggplot2

Make a shape plot with ggplot2

## Usage

``` r
shape_plot(
  data,
  col.x = "x",
  col.estimate = c("estimate", "est", "beta", "loghr"),
  col.stderr = c("stderr", "std.error", "std.err", "se"),
  col.lci = NULL,
  col.uci = NULL,
  col.n = NULL,
  exponentiate = FALSE,
  logscale = exponentiate,
  xlogscale = FALSE,
  scalepoints = FALSE,
  digits = 2,
  minse = NA,
  pointsize = 3 * base_size/11,
  col.group = NULL,
  shape = 15,
  plotcolour = "black",
  colour = plotcolour,
  cicolour = colour,
  fill = colour,
  ciunder = NULL,
  lines = c("none", "lmw", "lm", "connect"),
  xlims = NULL,
  ylims = NULL,
  height = NULL,
  width = NULL,
  gap = c(0.025, 0.025),
  ext = c(0.025, 0.025),
  ratio = 1.5,
  plot.margin = margin(0.5, 1.5, 0.5, 0.5, "lines"),
  clip = "off",
  base_size = 11,
  base_line_size = base_size/22,
  stroke = base_size/22,
  axis.title.margin = 1,
  xbreaks = NULL,
  ybreaks = NULL,
  xlab = "Risk factor",
  ylab = "Estimate (95% CI)",
  legend.name = "",
  legend.position = "top",
  title = NULL,
  quiet = FALSE,
  printplot = !quiet,
  showcode = !quiet,
  addaes = NULL,
  addarg = NULL,
  add = NULL,
  envir = NULL
)
```

## Arguments

- data:

  The data frame containing estimates to be plotted.

- col.x:

  Name of column that provides the x-axis value (e.g. exposure, risk
  factor, dependent variable). (Default: "x")

- col.estimate:

  Name of column that provides point estimates. (Default: "estimate")

- col.stderr:

  Name of column that provides standard errors. (Default: "stderr")

- col.lci:

  Name of column that provides lower limit of confidence intervals.

- col.uci:

  Name of column that provides upper limit of confidence intervals.

- col.n:

  Name of column that provides number to be plotted below CIs.

- exponentiate:

  Exponentiate estimates (and CIs) before plotting, use log scale on the
  axis. (Default: FALSE)

- logscale:

  Use log scale for the y axis. (Default: exponentiate)

- xlogscale:

  Use log scale for the x axis. (Default: FALSE)

- scalepoints:

  Should the points be scaled by inverse of the standard error?
  (Default: FALSE)

- digits:

  Number of digits to use in text of estimates.

- minse:

  Minimum standard error to use when scaling point size. (Default will
  use minimum in the data.)

- pointsize:

  The (largest) size of box to use for plotting point estimates.
  (Default: 3 \* base_size / 11)

- col.group:

  Name of column that groups the estimates. (Default: NULL)

- shape:

  Shape of points. An integer, or name of a column of integers.
  (Default: 15)

- plotcolour:

  Colour for non-data aspects of the plot. (Default: "black")

- colour:

  Colour of points. Name of a colour, or name of a column of colour
  names. (Default will use plotcolour)

- cicolour:

  Colour of CI lines. Colour of CI lines. Name of a colour, or name of a
  column of colour names. (Default will use plotcolour)

- fill:

  Fill colour of points. Fill colour of points. Name of a colour, or
  name of a column of colour names. (Default will use plotcolour)

- ciunder:

  Plot CI lines before points. A logical value, or name of a column of
  logical values. (Default will plot CI lines after points.)

- lines:

  Add lines to the plot. "lmw" = Linear fit through estimates, weighted
  by inverse variance. "lm" = Unweighted linear fit through estimates.
  "connect" = Lines connecting each estimate. (Default: "none")

- xlims:

  A numeric vector of length two. The limits of the x-axis.

- ylims:

  A numeric vector of length two. The limits of the y-axis.

- height:

  Panel height to use and apply different formatting to short CIs. A
  grid::unit() object, or if numeric is assumed to be in mm.

- width:

  Panel width.A grid::unit() object, or if numeric is assumed to be in
  mm.

- gap:

  A numeric vector of length two. The gap between plotting area and axis
  to the left and bottom of the plot, as a proportion of the x-axis
  length. (Default: c(0.025, 0.025))

- ext:

  A numeric vector of length two. The extensions to add to the right and
  top of the plot, as a proportion of the x-axis length. (Default:
  c(0.025, 0.025))

- ratio:

  The ratio (y-axis:x-axis) to use for the plot. (Default: 1.5)

- plot.margin:

  Plot margin, given as margin(top, right, bottom, left, units).
  (Default: margin(0.5, 1.5, 0.5, 0.5, "lines"))

- clip:

  Passed to clip argument of
  [`ggplot2::coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html).
  (Default: "on")

- base_size:

  Base font size, given in pts. (Default: 11)

- base_line_size:

  Base size for line elements. (Deault: base_size/22)

- stroke:

  Size of outline of shapes. (Default: base_size/22)

- axis.title.margin:

  Margin between axis titles and plot. (Default: 1)

- xbreaks:

  Breaks for the x axis. Passed to ggplots::scale_x_continuous.
  (Default: NULL)

- ybreaks:

  Breaks for the y axis. Passed to ggplots::scale_y_continuous.
  (Default: NULL)

- xlab:

  Label for x-axis. (Default: "Risk factor")

- ylab:

  Label for y-axis. (Default: "Estimate (95% CI)")

- legend.name:

  The name of the colour scale/legend for groups. (Default: "")

- legend.position:

  Position of the legend for groups ("none", "left", "right", "bottom",
  "top", or two-element numeric vector). (Default: "top")

- title:

  Plot title. (Default: NULL)

- quiet:

  Set to TRUE to not print the plot nor show generated code in the
  RStudio 'Viewer' pane. (Default: FALSE)

- printplot:

  Print the plot. (Default: !quiet)

- showcode:

  Show the ggplot2 code to generate the plot in RStudio 'Viewer' pane.
  (Default: !quiet)

- addaes, addarg, add:

  Methods for customising the plot. See documentation for details.

- envir:

  Environment in which to evaluate the plot code. May be useful when
  calling this function inside another function.

## Value

A list:

- plot:

  the plot

- code:

  ggplot2 code to generate the plot

## Notes

### Confidence intervals

When standard errors are supplied to the `shape_plot()` and
[`forest_plot()`](https://neilstats.github.io/ckbplotr/reference/forest_plot.md)
functions, confidence intervals are calculated as 95\\ using the Normal
approximation method (with critical value 1.96).

### Stroke

The `stroke` argument sets the stroke aesthetic for plotted shapes. See
<https://ggplot2.tidyverse.org/articles/ggplot2-specs.html> for more
details. The stroke size adds to total size of a shape, so unless
`stroke = 0` the scaling of size by inverse variance will be slightly
inaccurate (but there are probably more important things to worry
about).
