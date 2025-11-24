# Generated code

The
[`shape_plot()`](https://neilstats.github.io/ckbplotr/reference/shape_plot.md)
and
[`forest_plot()`](https://neilstats.github.io/ckbplotr/reference/forest_plot.md)
functions return both a plot and the code used to generate plot (which
will be shown in the RStudio Viewer pane). Examples of generated code
are shown below.

## Shape plot

``` r
my_results <- data.frame(
  x   = c(  12,   14, 15.5,   18),
  est = c(0.05, 0.21, 0.15, 0.32),
  se  = c(0.05, 0.05, 0.05, 0.05)
)

shape <- shape_plot(my_results,
                    xlims        = c(10, 20),
                    ylims        = c(0.75, 2),
                    exponentiate = TRUE,
                    printplot    = FALSE)
```

The code now stored in `shape$code` and shown in the RStudio Viewer pane
is:

    library(ggplot2)

    datatoplot <- my_results

    # Create the plot with main aesthetics
    plot <- ggplot(datatoplot, aes(x = x, y = exp(est))) +

      # Plot the point estimates [estimates.points]
      geom_point(aes(size   = 1),
                 shape  = 15,
                 colour = "black",
                 fill = "black",
                 stroke = 0.5) +
      
      # Plot point estimates text [estimates.text]
      geom_text(aes(y     = pmax(0.75, pmin(2, exp(est+1.96*se))),
                    label = format(round(exp(est), 2), nsmall = 2)),
                vjust = -0.8,
                size  = 3.092846,
                colour = "black") +
      
      # Plot the CIs [ci]
      geom_linerange(aes(ymin = pmin(2, pmax(0.75, exp(est-1.96*se))),
                         ymax = pmax(0.75, pmin(2, exp(est+1.96*se)))),
                     linewidth = 0.5) +
      
      # Add tiny segments with arrows when the CIs go outside axis limits [arrows]
      geom_segment(aes(y = y,
                       yend = yend),
                   data      = \(d) dplyr::bind_rows(dplyr::filter(d, exp(est+1.96*se) > 2) |> dplyr::mutate(y = 2 - 1e-6, yend = 2),
                                                  dplyr::filter(d, exp(est-1.96*se) < 0.75) |> dplyr::mutate(y = 0.75 + 1e-6, yend = 0.75)),
                   linewidth = 0.5,
                   arrow     = arrow(type = "closed", length = unit(4, "pt")),
                   na.rm     = TRUE) +
      
      # Set the scale for the size of boxes [scale.radius]
      scale_radius(guide  = "none",
                   limits = c(0, NA_real_),
                   range  = c(0, 3)) +
      
      # Set the scale for shape [scale.shape]
      scale_shape_identity() +
      
      # Set the scale for colour [scale.colour]
      scale_colour_identity() +
      
      # Set the scale for fill [scale.fill]
      scale_fill_identity() +
      
      # Set the y-axis scale [scale.y]
      scale_y_continuous(transform = "log") +
      
      # Add titles
      xlab("Risk factor") +
      ylab("Estimate (95% CI)") +
      
      # Plot like a CKB plot [ckb.style]
      ckbplotr::ckb_style(xlims          = c(10, 20),
                          ylims          = c(0.75, 2),
                          gap            = c(0.025, 0.025),
                          ext            = c(0.025, 0.025),
                          ratio          = 1.5,
                          base_size      = 11,
                          base_line_size = 0.5,
                          ink            = "black",
                          axis.title.margin = 1,
                          plot.margin    = margin(0.5, 1.5, 0.5, 0.5, "lines"),
                          clip           = "off") +
      
      # Add theme [theme]
      theme(legend.position = "top")
      

## Forest plot

``` r
my_results <- data.frame(
  subgroup   = c("men", "women", "35_49", "50_64", "65_79"),
  est        = c( 0.45,    0.58,    0.09,    0.35,     0.6),
  se         = c( 0.07,    0.06,    0.06,    0.05,    0.08)
)

forest <- forest_plot(my_results, printplot = FALSE)
```

The code now stored in `forest$code` and shown in the RStudio Viewer
pane is:

    library(ggplot2)

    # Prepare data to be plotted using ckbplotr::forest_data()
    datatoplot <- ckbplotr::forest_data(panels = my_results,
                                        panel.names = "1",
                                        col.estimate = "est",
                                        col.stderr = "se")

    # Create the ggplot
    ggplot(datatoplot, aes(y = row, x = estimate_transformed)) +

      # Put the different panels in side-by-side plots using facets [facet]
      facet_wrap(vars(panel), nrow = 1) +
      
      # Add a line at null effect [nullline]
      annotate(geom      = "segment",
               y         = 0.7,
               yend      = Inf,
               x         = 1,
               xend      = 1,
               linewidth = 0.5,
               colour    = "black") +
      
      # Plot points at the transformed estimates [points]
      geom_point(data   = \(x) dplyr::filter(x,
                                          estimate_transformed > 0.8,
                                          estimate_transformed < 2.2,
                                          !as_diamond),
                 shape  = 15,
                 size   = 3,
                 colour = "black",
                 fill   = "black",
                 stroke = 0,
                 na.rm  = TRUE) +
      
      # Plot the CIs [ci]
      geom_errorbar(aes(xmin = pmin(pmax(lci_transformed, 0.8), 2.2),
                        xmax = pmin(pmax(uci_transformed, 0.8), 2.2)),
                    data = \(x) dplyr::filter(x, !is.na(estimate_transformed), !as_diamond),
                    colour    = "black",
                    width     = 0,
                    linewidth = 0.5,
                    na.rm     = TRUE) +
      
      # Add columns to right side of panels [col.right, heading.right]
      ## column auto_estcolumn
      ckbplotr::geom_text_move(aes(y = row,
                                   x = 2.2,
                                   label = `auto_estcolumn`),
                               move_x  = unit(0.9, "mm"),
                               hjust   = 0,
                               size    = 3.092846,
                               colour  = "black",
                               na.rm   = TRUE,
                               parse   = FALSE) +
      ckbplotr::geom_text_move(aes(y     = - 0,
                                   x     = 2.2,
                                   label = title),
                               move_x  = unit(0.9, "mm"),
                               hjust    = 0,
                               vjust    = 0,
                               size     = 3.092846,
                               colour   = "black",
                               fontface = "bold",
                               lineheight = 1,
                               parse    = FALSE,
                               data = \(x) dplyr::tibble(panel = sort(unique(x[["panel"]])),
                                                      title = "HR (95% CI)")) +
      
      # Add xlab below each axis [xlab]
      ckbplotr::geom_text_move(aes(y = Inf,
                                   x = 1.32665,
                                   label = xlab),
                               hjust    = 0.5,
                               size     = 3.092846,
                               colour   = "black",
                               vjust    = 1,
                               move_y   = unit(-7.4228304, "mm"),
                               fontface = "bold",
                               data = \(x) dplyr::tibble(panel = sort(unique(x[["panel"]])),
                                                      xlab = "HR (95% CI)")) +
      
      # Set coordinate system [coord]
      coord_cartesian(clip = "off",
                      xlim = c(0.8, 2.2)) +
      
      # Set the scale for the x axis [scale.x]
      scale_x_continuous(trans  = "log",
                         limits = c(0.8, 2.2),
                         breaks = c(0.5, 1, 1.5, 2, 2.5),
                         expand = c(0,0)) +
      
      # Set the scale for the y axis [scale.y]
      scale_y_continuous(trans = "reverse",
                         breaks = attr(datatoplot, "rowlabels")$row,
                         labels = attr(datatoplot, "rowlabels")$row.label,
                         limits = c(max(attr(datatoplot, "rowlabels")$row) + 0.7, NA),
                         expand = c(0,0)) +
      
      # Control the overall look of the plot [theme]
      theme(text             = element_text(size = 11, colour = "black"),
            line             = element_line(linewidth = 0.5),
            panel.background = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title       = element_blank(),
            axis.line.x      = element_line(colour = "black", linewidth = 0.5, lineend = "round"),
            axis.title       = element_blank(),
            axis.ticks.x     = element_line(colour = "black"),
            axis.ticks.length.x = unit(2.75,  "pt"),
            axis.text.x      = element_text(colour = "black",
                                            margin = margin(t = 4.4),
                                            vjust  = 1),
            axis.ticks.y     = element_blank(),
            axis.ticks.length.y = unit(0, "pt"),
            axis.line.y      = element_blank(),
            axis.text.y      = marquee::element_marquee(hjust  = 0,
                                                        colour = "black",
                                                        margin = margin(r = 2.9, unit = "mm")),
            panel.border     = element_blank(),
            panel.spacing    = unit(24.5, "mm") + unit(5, "mm") + unit(2.9, "mm"),
            strip.background = element_blank(),
            strip.placement  = "outside",
            strip.text       = element_blank(),
            legend.position  = "none",
            plot.background  = element_blank(),
            plot.margin      = margin(2, 8, 2, 8, "mm") + unit(c(3.092846, 0, 6.185692, 0), "mm") + unit(c(0, 24.5, 0, 0), "mm"))
      
