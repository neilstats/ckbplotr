# Prepare figure for saving

Prepare figure for saving

## Usage

``` r
prepare_figure(
  figure,
  title = "",
  title.pos = grid::unit.c(unit(1.27/2, "cm"), unit(1, "npc") - unit(1.27/2, "cm")),
  title.just = c(0, 1),
  title.gpar = list(fontsize = 12, fontface = "bold"),
  footer = "",
  footer.pos = grid::unit.c(unit(1.27/2, "cm"), unit(1.27/3, "cm")),
  footer.just = c(0, 0),
  footer.gpar = list(fontsize = 9),
  margin = unit(c(2.27, 1.27, 1.27, 1.27), units = "cm"),
  size = NULL,
  valign = 0.5,
  halign = 0.5,
  pagesize = c("A4", "A5"),
  landscape = FALSE,
  pagedim = NULL
)
```

## Arguments

- figure:

  Plot (or graphical object).

- title:

  Title to be added to the page. (Default: "")

- title.pos:

  Position of the title text. Default is 1/4 inch from top left of page.
  (Default: unit.c(unit(1.27/2, "cm"), unit(1, "npc") - unit(1.27/2,
  "cm")))

- title.just:

  Justification of the title text. (Default: c(0, 1))

- title.gpar:

  Graphical parameters for title. (Default: list(fontsize = 12, fontface
  = "bold"))

- footer:

  Footer to be added to the page. (Default: "")

- footer.pos:

  Position of the footer text. Default is 1/6 inch from bottom and 1/4
  inch from left of page. (Default: unit.c(unit(1.27/2, "cm"),
  unit(1.27/3, "cm")))

- footer.just:

  Justification of the footer text. (Default: c(0, 0))

- footer.gpar:

  Graphical parameters for footer. (Default: list(fontsize = 9))

- margin:

  Margin to be placed around the plot. Default is 2.27cm top, 1.27cm
  (1/2 inch) other sides. (Default: unit(c(2.27, 1.27, 1.27, 1.27),
  units = "cm"))

- size:

  A unit vector of length two (width, height). Size of plot (a
  width/height larger than page weight/height minus margins will be
  ignored), centred within margins. By default, plot will fill the space
  within margins.

- valign:

  If size is set, where to place figure within margins. 1 = top, 0.5 =
  middle, 0 = bottom. (Default: 0.5)

- halign:

  If size is set, where to place figure within margins. 1 = right, 0.5 =
  middle, 0 = left (Default: 0.5)

- pagesize:

  Page size of output: "A4" or "A5". (Default: "A4")

- landscape:

  Landscape page orientation? (Default: False)

- pagedim:

  Dimensions (width, height) of output. Overrides pagesize and landscape
  arguments if used.
