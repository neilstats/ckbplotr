# Save plots to files

## ggsave

The easiest way to save a plot is to to use the
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
function. Remember that
[`shape_plot()`](https://neilstats.github.io/ckbplotr/reference/shape_plot.md)
and
[`forest_plot()`](https://neilstats.github.io/ckbplotr/reference/forest_plot.md)
both return a list of the plot and code, so you need to use `$plot` to
save just the plot to a file.

``` r
my_results <- data.frame(
  x   = c(  12,   14, 15.5,   18),
  est = c(0.05, 0.21, 0.15, 0.32),
  se  = c(0.05, 0.05, 0.05, 0.05)
)

my_plot <- shape_plot(my_results,
                      xlims = c(10, 20),
                      ylims = c(0.75, 2),
                      exponentiate = TRUE,
                      quiet = TRUE)
```

``` r
ggsave("myplot.png",
       plot = my_plot$plot,
       width = 14, height = 14, units = "cm")
```

Plots created with this package have transparent backgrounds. For a png
file output, you can use the `bg` argument to set the background colour:

``` r
ggsave("myplot.png",
       plot = my_plot$plot,
       width = 14, height = 14, units = "cm",
       bg = "white")
```

The ckbplotr function
[`ggpreview()`](https://neilstats.github.io/ckbplotr/reference/ggpreview.md)
can be used in place of
[`ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html) to
preview the output.

## Save with title and footnote

Use
[`save_figure()`](https://neilstats.github.io/ckbplotr/reference/save_figure.md)
to add a title and footer to a plot and save to a file. The following
code will save the plot (sized to 14 by 14 cm) in an A4 sized PDF file,
with title and footer.

``` r
save_figure(my_plot$plot,
            filename   = "Figure 1.pdf",
            title      = "Figure 1: My example shape plot",
            footer     = "An example footer text.",
            size       = unit(c(14, 14), "cm"))
```

![](save_plots_files/figure-html/unnamed-chunk-5-1.png)

The function has several arguments that allow for customization of
appearance and layout. For example: `valign` and `halign` to control the
position of the plot (if `size` is set); `landscape = TRUE` to create a
landscape page; and set `cropped` to be a file name to also save a plot
without additional margins, title or footer.

## Preview the output

Set `preview = TRUE` to view a preview of the output in the RStudio
Viewer pane. (Instead of creating a file, the figure is saved to a
temporary PNG file and shown.)

``` r
save_figure(my_plot$plot,
            filename   = "Figure 1.pdf",
            title      = "Figure 1: My example shape plot",
            footer     = "An example footer text.",
            size       = unit(c(14, 14), "cm"),
            preview    = TRUE)
```
