
#' Make a shape plot with ggplot2
#'
#'
#' @param data The data frame containing estimates to be plotted.
#' @param col.x Name of column that provides the x-axis value (e.g. exposure, risk factor, dependent variable). (Default: "x")
#' @param col.estimate Name of column that provides point estimates.
#'   (Default: "estimate")
#' @param col.stderr Name of column that provides standard errors. (Default: "stderr")
#' @param col.lci Name of column that provides lower limit of confidence intervals.
#' @param col.uci Name of column that provides upper limit of confidence intervals.
#' @param col.n Name of column that provides number to be plotted below CIs.
#' @param col.group Name of column that groups the estimates. (Default: NULL)
#' @param exponentiate Exponentiate estimates (and CIs) before plotting,
#'   use log scale on the axis. (Default: TRUE)
#' @param scalepoints Should the points be scaled by inverse of the standard
#'   error? (Default: FALSE)
#' @param xlab Label for x-axis. (Default: "Risk factor")
#' @param ylab Label for y-axis. (Default: "Estimate (95\% CI)")
#' @param title Plot title. (Default: "Figure")
#' @param xlims A numeric vector of length two. The limits of the x-axis.
#' @param ylims A numeric vector of length two. The limits of the y-axis.
#' @param gap A numeric vector of length two. The gap between plotting area and axis to the left and bottom of the plot, as a proportion of the x-axis length. (Default: c(0.025, 0.025))
#' @param ext A numeric vector of length two. The extensions to add to the right and top of the plot, as a proportion of the x-axis length. (Default: c(0.025, 0.025))
#' @param ratio The ratio (y-axis:x-axis) to use for the plot. (Default: 1.5)
#'
#' @return A list:
#' \describe{
#'   \item{plot}{the plot}
#'   \item{code}{ggplot2 code to generate the plot}
#'}
#'
#' @import ggplot2
#' @export



make_shape_plot <- function(data,
                            col.x         = "x",
                            col.estimate  = "estimate",
                            col.stderr    = "stderr",
                            col.lci       = NULL,
                            col.uci       = NULL,
                            col.n         = NULL,
                            col.group     = NULL,
                            exponentiate  = FALSE,
                            scalepoints   = FALSE,
                            pointsize     = 3,
                            xlims,
                            ylims,
                            gap           = c(0.025, 0.025),
                            ext           = c(0.025, 0.025),
                            ratio         = 1.5,
                            xbreaks       = NULL,
                            ybreaks       = NULL,
                            xlab          = "Risk factor",
                            ylab          = "Estimate (95% CI)",
                            title         = "Figure"){

  # Check arguments
  if (!is.null(col.lci) &&  is.null(col.uci)) stop("col.lci and col.uci must both be specified")
  if ( is.null(col.lci) && !is.null(col.uci)) stop("col.lci and col.uci must both be specified")


  # Put column names in ``
  col.x        <- paste0("`", col.x, "`")
  col.estimate <- paste0("`", col.estimate, "`")
  col.stderr   <- paste0("`", col.stderr, "`")

  # Turn plot_like_ckb argument expression into strings
  xlims <- deparse(xlims)
  ylims <- deparse(ylims)
  gap   <- deparse(gap)
  ext   <- deparse(ext)
  ratio <- deparse(ratio)

  # Create strings for axis breaks
  scale_x_string <- NULL
  if (!is.null(xbreaks)){
    scale_x_string <- paste0('
  # Set the x-axis scale
  scale_x_continuous(breaks = ', deparse(xbreaks),') +
')
  }

  y_breaks_string <- NULL
  if (!is.null(ybreaks)){
    y_breaks_string <- paste0(',
                     breaks = ', deparse(ybreaks))
  }

  # Create strings for y-axis scale, estimates and CIs
  if (exponentiate == TRUE) {
    scale    <- "log"
    est_string <- paste0('exp(', col.estimate, ')')

    if (!is.null(col.lci)) {
      lci_string <- paste0('exp(`', col.lci,'`)')
      uci_string <- paste0('exp(`', col.uci,'`)')
    } else {
      lci_string <- paste0('exp(', col.estimate,'-1.96*', col.stderr,')')
      uci_string <- paste0('exp(', col.estimate,'+1.96*', col.stderr,')')
    }

  } else {
    scale    <- "identity"
    est_string <- col.estimate
    if (!is.null(col.lci)) {
      lci_string <- paste0("`", col.lci, "`")
      uci_string <- paste0("`", col.uci, "`")
    } else {
      lci_string <- paste0(col.estimate,'-1.96*', col.stderr)
      uci_string <- paste0(col.estimate,'+1.96*', col.stderr)
    }
  }

  # Create string for setting fill colour by group
  if (!is.null(col.group)) {
    group_string <- paste0(',
                   fill = as.factor(`', col.group,'`)')
    scale_fill_string <- '
  # Set the scale for fill colours
  scale_fill_grey(start = 0, end = 1, guide = FALSE) +
'
    fill_string <- ''
  } else {
    group_string <- ''
    scale_fill_string <- ''
    fill_string <- ', fill = "black"'
  }

  # Create string for plotting point estimates using geom_point
  if (scalepoints) {
    if (!is.null(col.lci)) {
      geom_point_string <- paste0('geom_point(aes(size = 1/(', col.estimate,' - `', col.lci,'`)), shape = 22', fill_string,')')
    } else {
      geom_point_string <- paste0('geom_point(aes(size = 1/', col.stderr,'), shape = 22', fill_string,')')
    }
  } else {
    geom_point_string <- paste0('geom_point(aes(size = 1), shape = 22', fill_string,')')
  }

  # Create string for plotting col.n under CIs
  if (!is.null(col.n)){
    n_events_string <- paste0('
  # Plot n events text
  geom_text(aes(y = ', lci_string, ',
            label = ', col.n, '),
            vjust = 1.8,
            size  = 3) +
')
  } else {
    n_events_string = ''
  }

  # Put together plot code with strings created above
  plotcode <- paste0('
# Create the ggplot
plot <- ggplot(data = ', deparse(substitute(data)),',
               aes(x = ', col.x, ', y = ', est_string, group_string, ')) +

  # Plot the CIs
  geom_linerange(aes(ymin = ', lci_string,',
                     ymax = ', uci_string,')) +

  # Plot the point estimates
  ', geom_point_string,' +

  # Plot point estimates text
  geom_text(aes(y = ', uci_string, ',
            label = format(round(', est_string, ', 2), nsmall = 2)),
            vjust = -0.8,
            size  = 3) +
  ', n_events_string, '
  # Set the scale for the size of boxes
  scale_radius(guide = "none",
               limits = c(0, NA),
               range = c(0, ', pointsize,')) +
  ', scale_fill_string, '
  # Set the y-axis scale
  scale_y_continuous(trans = "', scale,'"', y_breaks_string, ') +
  ', scale_x_string,'
  # Add titles
  xlab("', xlab,'") +
  ylab("', ylab,'") +
  ggtitle("', title,'")

# Plot like a CKB plot
plot_like_ckb(plot  = plot,
              xlims = ', xlims,',
              ylims = ', ylims,',
              gap   = ', gap,',
              ext   = ', ext,',
              ratio = ', ratio,')')

  # Write the ggplot2 code to a file in temp directory, and show in RStudio viewer.
  writeLines(paste("# ggplot2 code ------------------",
                   plotcode,
                   sep = "\n\n"),
             file.path(tempdir(), "plotcode.txt"))
  viewer <- getOption("viewer", default = function(url){})
  viewer(file.path(tempdir(), "plotcode.txt"))

  # Create the plot
  plot <- eval(parse(text = plotcode))
  print(plot)


  return(list(plot = plot,
              code = parse(text = plotcode)) )
}

