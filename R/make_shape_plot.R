
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
#' @param printplot Print the plot. (Default: TRUE)
#' @param showcode Show the ggplot2 code to generate the plot in RStudio 'Viewer' pane. (Default: TRUE)
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
                            exponentiate  = FALSE,
                            scalepoints   = FALSE,
                            pointsize     = 3,
                            col.group     = NULL,
                            col.shape     = NULL,
                            col.colour    = NULL,
                            col.cicolour  = col.colour,
                            col.fill      = NULL,
                            col.ciunder   = NULL,
                            xlims,
                            ylims,
                            gap           = c(0.025, 0.025),
                            ext           = c(0.025, 0.025),
                            ratio         = 1.5,
                            base_size     = 11,
                            base_line_size = base_size/22,
                            xbreaks       = NULL,
                            ybreaks       = NULL,
                            xlab          = "Risk factor",
                            ylab          = "Estimate (95% CI)",
                            title         = "Figure",
                            printplot     = TRUE,
                            showcode      = TRUE){

  # Check arguments
  if (!is.null(col.lci) &&  is.null(col.uci)) stop("col.lci and col.uci must both be specified")
  if ( is.null(col.lci) && !is.null(col.uci)) stop("col.lci and col.uci must both be specified")
  if (!is.null(col.group) && !is.null(col.fill)) stop("col.group and col.fill both control fill, so do not specify both")

  # Add empty string title if it is null
  if (is.null(title)) title <- ""

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

  # default values for aesthetics
  if (is.null(col.shape)) { col.shape <- 22 } else {col.shape <- paste0("`", col.shape, "`")}
  if (is.null(col.cicolour)) { col.cicolour <- "\"black\"" } else {col.cicolour <- paste0("`", col.cicolour, "`")}
  if (is.null(col.colour)) { col.colour <- "\"black\"" } else {col.colour <- paste0("`", col.colour, "`")}
  if (is.null(col.fill)) { col.fill <- "\"black\"" } else {col.fill <- paste0("`", col.fill, "`")}

  # Create strings for axis breaks
  scale_x_string <- NULL
  if (!is.null(xbreaks)){
    scale_x_string <- c('',
                        '  # Set the x-axis scale',
                        sprintf('scale_x_continuous(breaks = %s) +', deparse(xbreaks)))
  }

  y_breaks_string <- ''
  if (!is.null(ybreaks)){
    y_breaks_string <- sprintf(', breaks = %s', deparse(ybreaks))
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
    group_string <- sprintf(', group = as.factor(`%s`)', col.group)
    scale_fill_string <- c('',
                           '  # Set the scale for fill colours',
                           '  scale_fill_grey(start = 0, end = 1, guide = FALSE) +')
    fill_string <- sprintf('fill = as.factor(`%s`)', col.group)
  } else {
    group_string <- ''
    scale_fill_string <- '  scale_fill_identity() +'
    fill_string <- sprintf('fill = %s', col.fill)
  }

  # Create string for plotting point estimates using geom_point
  if (scalepoints) {
    if (!is.null(col.lci)) {
      geom_point_string <- c(sprintf(
                             '  geom_point(aes(size = 1/(%s - %s),', col.estimate, col.lci),
                             sprintf(
                               '                 shape = %s,', col.shape),
                             sprintf(
                               '                 colour = %s,', col.colour),
                             sprintf(
                               '                 %s)) +', fill_string)
      )
    } else {
      geom_point_string <- c(sprintf(
        '  geom_point(aes(size = 1/%s,', col.stderr),
        sprintf(
          '                 shape = %s,', col.shape),
        sprintf(
          '                 colour = %s,', col.colour),
        sprintf(
          '                 %s)) +', fill_string)
      )
    }
  } else {
    geom_point_string <- c('  geom_point(aes(size = 1,',
                           sprintf(
                           '                 shape = %s,', col.shape),
                           sprintf(
                           '                 colour = %s,', col.colour),
                           sprintf(
                           '                 %s)) +', fill_string)
                           )
  }

  # Create string for plotting col.n under CIs
  if (!is.null(col.n)){
    n_events_string <- c('',
                         '  # Plot n events text',
                         sprintf('  geom_text(aes(y = %s,', lci_string),
                         sprintf('            label = %s),', col.n),
                         '            vjust = 1.8,',
                         sprintf('            size  = %s) +', base_size/(11/3)))
  } else {
    n_events_string <- NULL
  }

  # Put together plot code with strings created above
  plotcode <- c('# Create the ggplot',
                sprintf('plot <- ggplot(data = %s,', deparse(substitute(data))),
                sprintf('               aes(x = %s, y = %s%s)) +', col.x, est_string, group_string),
                '',
                if (is.character(col.ciunder) && any(data[[col.ciunder]], na.rm = TRUE)){
                  c(
                    '  # Plot the CIs - before plotting points',
                    sprintf('  geom_linerange(data = ~ dplyr::filter(.x, `%s`),',col.ciunder),
                    sprintf('                 aes(ymin = %s,', lci_string),
                    sprintf('                     ymax = %s,', uci_string),
                    sprintf('                     colour = %s), ', col.cicolour),
                    sprintf('                     lwd = %s) +', base_line_size),
                    '')},
                '  # Plot the point estimates',
                geom_point_string,
                '',
                '  # Plot point estimates text',
                sprintf('  geom_text(aes(y = %s,', uci_string),
                sprintf('            label = format(round(%s, 2), nsmall = 2)),', est_string),
                '            vjust = -0.8,',
                sprintf('            size  = %s) +', base_size/(11/3)),
                n_events_string,
                '',
                '  # Set the scale for the size of boxes',
                '  scale_radius(guide  = "none",',
                '               limits = c(0, NA),',
                sprintf('               range  = c(0, %s)) +', pointsize),
                '',
                if (is.character(col.ciunder) && !all(data[[col.ciunder]], na.rm = TRUE)){
                  c(
                    '  # Plot the CIs - after plotting points',
                    sprintf('  geom_linerange(data = ~ dplyr::filter(.x, !`%s`),',col.ciunder),
                    sprintf('                 aes(ymin = %s,', lci_string),
                    sprintf('                     ymax = %s,', uci_string),
                    sprintf('                     colour = %s), ', col.cicolour),
                    sprintf('                     lwd = %s) +', base_line_size),
                    '')},
                if (is.null(col.ciunder)){
                  c(
                    '  # Plot the CIs',
                    sprintf('  geom_linerange(aes(ymin = %s,', lci_string),
                    sprintf('                     ymax = %s,', uci_string),
                    sprintf('                     colour = %s), ', col.cicolour),
                    sprintf('                     lwd = %s) +', base_line_size),
                    '')
                },
                '  # Use identity for aesthetic scales',
                '  scale_shape_identity() +',
                '  scale_colour_identity() +',
                scale_fill_string,
                '',
                '  # Set the y-axis scale',
                sprintf('  scale_y_continuous(trans = "%s"%s) +', scale, y_breaks_string),
                scale_x_string,
                '',
                '  # Add titles',
                sprintf('  xlab("%s") +', xlab),
                sprintf('  ylab("%s") +', ylab),
                sprintf('  ggtitle("%s")', title),
                '',
                '',
                '# Plot like a CKB plot',
                'plot_like_ckb(plot           = plot,',
                sprintf('              xlims          = %s,', xlims),
                sprintf('              ylims          = %s,', ylims),
                sprintf('              gap            = %s,', gap),
                sprintf('              ext            = %s,', ext),
                sprintf('              ratio          = %s,', ratio),
                sprintf('              base_size      = %s,', base_size),
                sprintf('              base_line_size = %s)', base_line_size))

  # Write the ggplot2 code to a file in temp directory, and show in RStudio viewer.
  if (showcode){
    writeLines(paste(plotcode,
                     collapse = "\n"),
               file.path(tempdir(), "plotcode.txt"))
    viewer <- getOption("viewer", default = function(url){})
    viewer(file.path(tempdir(), "plotcode.txt"))
  }

  # Create the plot
  plot <- eval(parse(text = plotcode))
  if (printplot){
    print(plot)
  }


  return(list(plot = plot,
              code = plotcode) )
}

