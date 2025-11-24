# Create a plot preview and display it in the Viewer pane.

This function saves a ggplot2 plot to a temporary PNG file and then
embeds it in an HTML page, which is opened in the Viewer pane.

## Usage

``` r
ggpreview(...)
```

## Arguments

- ...:

  Arguments passed to `ggsave` and the PNG device function.

## Device

The plot is saved using `ggsave` with the `png` device, regardless of
what is specified in the call, so any arguments not used by `ggsave` or
`png` are ignored.
