# CKB ggplot theme

A ggplot2 theme, based on theme_bw.

## Usage

``` r
theme_ckb(
  base_size = 11,
  base_line_size = base_size/22,
  ink = "black",
  paper = "white",
  colour = NULL,
  axis.title.margin = 1,
  plot.margin = margin(0.5, 1.5, 0.5, 0.5, "lines"),
  ...
)
```

## Arguments

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

- ...:

  Arguments passed to
  [`ggplot2::theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
  if using ggplot2 4.0.0 or later.
