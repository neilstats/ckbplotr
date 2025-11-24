# Output plots as files

Output plots as files

## Usage

``` r
save_figure(
  figure,
  filename,
  cropped = NULL,
  args = NULL,
  args_cropped = NULL,
  preview = FALSE,
  ...
)
```

## Arguments

- figure:

  Plot (or graphical object).

- filename:

  Name of file to create.

- cropped:

  Name of second output file of the figure without margins or title.

- args:

  List of arguments passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  for the main figure.

- args_cropped:

  List of arguments passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  for the cropped figure.

- preview:

  Preview the output in the RStudio Viewer pane. (Default: False)

- ...:

  Other arguments passed to
  [prepare_figure](https://neilstats.github.io/ckbplotr/reference/prepare_figure.md).
