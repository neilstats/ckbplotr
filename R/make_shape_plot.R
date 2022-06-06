
#' Make a shape plot with ggplot2
#'
#' @inheritParams plot_like_ckb
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
#' @param shape Shape of points. An integer, or name of a column of integers. (Default will use shape 22 - squares with fill.)
#' @param plotcolour Colour for non-data aspects of the plot. (Default: "black")
#' @param colour Colour of points. Name of a colour, or name of a column of colour names. (Default will use plotcolour)
#' @param cicolour Colour of CI lines. Colour of CI lines. Name of a colour, or name of a column of colour names. (Default will use plotcolour)
#' @param fill Fill colour of points. Fill colour of points. Name of a colour, or name of a column of colour names. (Default will use plotcolour)
#' @param ciunder Plot CI lines before points. A logical value, or name of a column of logical values. (Default will plot CI lines after points.)
#' @param lines Plot lines (linear fit through estimates, weighted by inverse variance). (Default: FALSE)
#' @param exponentiate Exponentiate estimates (and CIs) before plotting,
#'   use log scale on the axis. (Default: FALSE)
#' @param logscale Use log scale for vertical axis. (Default: exponentiate)
#' @param scalepoints Should the points be scaled by inverse of the standard
#'   error? (Default: FALSE)
#' @param minse Minimum standard error to use when scaling point size. (Default will use minimum in the data.)
#' @param pointsize The (largest) size of box to use for plotting point
#'                  estimates. (Default: 3)
#' @param xlab Label for x-axis. (Default: "Risk factor")
#' @param ylab Label for y-axis. (Default: "Estimate (95\% CI)")
#' @param legend.name The name of the colour scale/legend for groups. (Default: "")
#' @param legend.position Position of the legend for groups ("none", "left", "right", "bottom", "top", or two-element numeric vector). (Default: "top")
#' @param title Plot title. (Default: "Figure")
#' @param xlims A numeric vector of length two. The limits of the x-axis.
#' @param ylims A numeric vector of length two. The limits of the y-axis.
#' @param xbreaks Breaks for the x axis. Passed to ggplots::scale_x_continuous. (Default: NULL)
#' @param ybreaks Breaks for the y axis. Passed to ggplots::scale_y_continuous. (Default: NULL)
#' @param gap A numeric vector of length two. The gap between plotting area and axis to the left and bottom of the plot, as a proportion of the x-axis length. (Default: c(0.025, 0.025))
#' @param ext A numeric vector of length two. The extensions to add to the right and top of the plot, as a proportion of the x-axis length. (Default: c(0.025, 0.025))
#' @param ratio The ratio (y-axis:x-axis) to use for the plot. (Default: 1.5)
#' @param stroke Size of outline of shapes. (Default: base_size/22)
#' @param printplot Print the plot. (Default: TRUE)
#' @param showcode Show the ggplot2 code to generate the plot in RStudio 'Viewer' pane. (Default: TRUE)
#' @param addcode A character vector of code to add to the generated code.
#'                The first element should be a regular expression.
#'                The remaining elements are added to the generated code just before the first match of a line (trimmed of  whitespace) with the regular expression. (Default: NULL)
#' @param addaes Specify additional aesthetics for some ggplot layers.
#' @param addarg Specify additional arguments for some ggplot layers.
#' @param envir Environment in which to evaluate the plot code. May be useful when calling this function inside another function.
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
                            logscale      = exponentiate,
                            scalepoints   = FALSE,
                            minse         = NA,
                            pointsize     = 3,
                            col.group     = NULL,
                            shape         = NULL,
                            plotcolour    = "black",
                            colour        = NULL,
                            cicolour      = colour,
                            fill          = NULL,
                            ciunder       = NULL,
                            lines         = FALSE,
                            xlims,
                            ylims,
                            gap           = c(0.025, 0.025),
                            ext           = c(0.025, 0.025),
                            ratio         = 1.5,
                            base_size     = 11,
                            base_line_size = base_size/22,
                            stroke        = base_size/22,
                            xbreaks       = NULL,
                            ybreaks       = NULL,
                            xlab          = "Risk factor",
                            ylab          = "Estimate (95% CI)",
                            legend.name   = "",
                            legend.position = "top",
                            title         = "Figure",
                            printplot     = TRUE,
                            showcode      = TRUE,
                            addcode       = NULL,
                            addaes        = NULL,
                            addarg        = NULL,
                            envir         = NULL){

  # Check arguments
  if (!is.null(col.lci) &&  is.null(col.uci)) stop("col.lci and col.uci must both be specified")
  if ( is.null(col.lci) && !is.null(col.uci)) stop("col.lci and col.uci must both be specified")
  if (!is.null(col.group) && !is.null(fill)) stop("col.group and fill both control fill, so do not specify both")

  # Put column names in `` if required
  col.x        <- fixsp(col.x)
  col.estimate <- fixsp(col.estimate)
  col.stderr   <- fixsp(col.stderr)

  # Turn plot_like_ckb argument expression into strings
  xlims <- deparse(xlims)
  ylims <- deparse(ylims)
  gap   <- deparse(gap)
  ext   <- deparse(ext)
  ratio <- deparse(ratio)

  # aesthetics: default value, match column name, or use argument itself
  shape.aes <- NULL
  if (is.null(shape)) {
    shape <- 22
  } else if (shape %in% names(data)){
    shape.aes <- fixsp(shape)
    shape <- NULL
  }

  plotcolour <- fixq(plotcolour)

  cicolour.aes <- NULL
  if (is.null(cicolour)) {
    cicolour <- plotcolour
  }
  else if (cicolour %in% names(data)){
    cicolour.aes <- fixsp(cicolour)
    cicolour <- NULL
  } else {
    cicolour <- fixq(cicolour)
  }

  colour.aes <- NULL
  if (is.null(colour)) {
    colour <- plotcolour
  } else if (colour %in% names(data)){
    colour.aes <- fixsp(colour)
    colour <- NULL
  } else {
    colour <- fixq(colour)
  }

  fill.aes <- NULL
  if (is.null(fill)) {
    fill <- plotcolour
  } else if (fill %in% names(data)){
    fill.aes <- fixsp(fill)
    fill <- NULL
  } else {
    fill <- fixq(fill)
  }

  # Check for log scale and exponentiate estimates
  if (logscale == TRUE){
    scale    <- "log"
  } else {
    scale    <- "identity"
  }
  if (exponentiate == TRUE) {
    est_string <- paste0('exp(', col.estimate, ')')
    if (!is.null(col.lci)) {
      lci_string <- paste0('exp(', fixsp(col.lci), ')')
      uci_string <- paste0('exp(', fixsp(col.uci), ')')
    } else {
      lci_string <- paste0('exp(', col.estimate,'-1.96*', col.stderr,')')
      uci_string <- paste0('exp(', col.estimate,'+1.96*', col.stderr,')')
    }
  } else {
    est_string <- col.estimate
    if (!is.null(col.lci)) {
      lci_string <- fixsp(col.lci)
      uci_string <- fixsp(col.uci)
    } else {
      lci_string <- paste0(col.estimate,'-1.96*', col.stderr)
      uci_string <- paste0(col.estimate,'+1.96*', col.stderr)
    }
  }

  # Check for using groups
  fill_string <- NULL
  fill_string.aes <- NULL
  if (!is.null(col.group)) {
    group_string <- sprintf(', group = as.factor(%s)', fixsp(col.group))
    scale_fill_string <- c('',
                           make_layer('# Set the scale for fill colours',
                                    f = "scale_fill_grey",
                                    arg = c("start = 0",
                                            "end   = 1",
                                            sprintf('name  = "%s"', legend.name)),
                                    br = FALSE))
    fill_string.aes <- sprintf('fill = as.factor(%s)', fixsp(col.group))
  } else {
    group_string <- ''
    scale_fill_string <- 'scale_fill_identity() +'
    fill_string <- sprintf('fill = %s', fill)
    fill_string.aes <-  sprintf('fill = %s', fill.aes)
  }


  # codetext - list of character vectors for writing plot code
  codetext <- list()


  # Write code to initiate the ggplot
  codetext$start.ggplot <- c(
    '# Create the ggplot',
    sprintf('plot <- ggplot(data = %s,', deparse(substitute(data))),
    indent(15,
           sprintf('aes(x = %s, y = %s%s)) +', col.x, est_string, group_string)),
    ''
  )


  # Write code for axes
  y_breaks_string <- ''
  if (!is.null(ybreaks)){
    y_breaks_string <- sprintf(', breaks = %s', deparse(ybreaks))
  }

  codetext$axes <- c(
    if (!is.null(xbreaks)){
      c('# Set the x-axis scale',
        sprintf('scale_x_continuous(breaks = %s) +', deparse(xbreaks)),
        '')
    },
    '# Set the y-axis scale',
    sprintf('scale_y_continuous(trans = "%s"%s) +', scale, y_breaks_string),
    '')


  # Write code for aesthetics scales
  codetext$scales <- c(
    make_layer(
    '# Set the scale for the size of boxes',
      f = "scale_radius",
      arg = c('guide  = "none"',
              sprintf('limits = c(0, %s)', deparse(1/minse)),
              sprintf('range  = c(0, %s)', pointsize))
    ),
    '# Use identity for aesthetic scales',
    'scale_shape_identity() +',
    'scale_colour_identity() +',
    scale_fill_string,
    '')


  # Write code for plotting lines
  if(lines) {
    codetext$lines <- make_layer(
      '# Plot lines (linear fit through estimates, weighted by inverse variance)',
      f = "stat_smooth",
      aes = c(addaes$lines,
              if (!is.null(col.lci)) {
        sprintf('weight = 1/((%s - %s)^2)', col.estimate, fixsp(col.lci))
      } else {
        sprintf('weight = 1/(%s^2)', col.stderr)
      }),
      arg = c(addarg$lines,
              'method   = "glm"',
              'formula  = y ~ x',
              'se       = FALSE',
              sprintf('colour = %s', plotcolour),
              'linetype = "dashed"',
              'size     = 0.25')
    )
  }


  # Write code for plotting point estimates using geom_point
  codetext$estimates.points <- make_layer(
    '# Plot the point estimates',
    f = "geom_point",
    aes = c(
      addaes$point,
      if (scalepoints) {
        if (!is.null(col.lci)) {
          sprintf('size = 1.96/(%s - %s)', col.estimate, fixsp(col.lci))
        } else {
          sprintf('size = 1/%s', col.stderr)
        }
      } else {
        'size = 1'
      },
      sprintf('shape = %s', shape.aes),
      sprintf('%s', fill_string.aes),
      sprintf('colour = %s', colour.aes)),
    arg = c(addarg$point,
            sprintf('shape = %s', shape),
            sprintf('colour = %s', colour),
            sprintf('%s', fill_string),
            sprintf('stroke = %s', stroke))
  )


  # Write code for plotting estimates text
  codetext$estimates.text <- make_layer(
    '# Plot point estimates text',
    f = "geom_text",
    aes = c(addaes$estimates,
            sprintf('y = %s', uci_string),
            sprintf('label = format(round(%s, 2), nsmall = 2)', est_string)),
    arg = c(addarg$estimates,
            'vjust = -0.8',
            sprintf('size  = %s', base_size/(11/3)),
            sprintf('colour = %s', plotcolour))
  )


  # Write code for plotting col.n under CIs
  if (!is.null(col.n)){
    codetext$n.events.text <- make_layer(
      '# Plot n events text',
      f = "geom_text",
      aes = c(addaes$n,
              sprintf('y = %s', lci_string),
              sprintf('label = %s', col.n)),
      arg = c(addarg$n,
              'vjust = 1.8',
              sprintf('size  = %s', base_size/(11/3)),
              sprintf('colour = %s', plotcolour))
    )
  }


  # Write code for plotting CIs
  codetext$cis.before <- make_layer(
    '# Plot the CIs',
    f = "geom_linerange",
    aes = c(addaes$ci,
            sprintf('ymin = %s', lci_string),
            sprintf('ymax = %s', uci_string),
            sprintf('colour = %s', cicolour.aes)),
    arg = c(addarg$ci,
            sprintf('colour = %s', cicolour),
            sprintf('lwd = %s', base_line_size))
  )

  if (isFALSE(ciunder) || is.null(ciunder)){
    codetext$cis.after <- codetext$cis.before
    codetext$cis.before <- NULL
  } else if (is.character(ciunder)){
    codetext$cis.before <- make_layer(
      '# Plot the CIs - before plotting points',
      f = "geom_linerange",
      aes = c(addaes$ci,
              sprintf('ymin = %s', lci_string),
              sprintf('ymax = %s', uci_string),
              sprintf('colour = %s', cicolour.aes)),
      arg = c(addarg$ci,
              sprintf('colour = %s', cicolour),
              sprintf('lwd = %s', base_line_size),
              sprintf('data = ~ dplyr::filter(.x, %s),', fixsp(ciunder)))
    )
    codetext$cis.after <- make_layer(
      '# Plot the CIs - after plotting points',
      f = "geom_linerange",
      aes = c(addaes$ci,
              sprintf('ymin = %s', lci_string),
              sprintf('ymax = %s', uci_string),
              sprintf('colour = %s', cicolour.aes)),
      arg = c(addarg$ci,
              sprintf('colour = %s', cicolour),
              sprintf('lwd = %s', base_line_size),
              sprintf('data = ~ dplyr::filter(.x, !%s),', fixsp(ciunder)))
    )
  }


  # Write code for titles
  codetext$titles <- c(
    '# Add titles',
    sprintf('xlab("%s") +', xlab),
    if (!is.null(title) && !title %in% c("", NA)){
      c(sprintf('ylab("%s") +', ylab),
        sprintf('ggtitle("%s")', title))
    } else {
      sprintf('ylab("%s")', ylab)
    },
    ''
  )

  # Write code to use plot_like_ckb function
  codetext$plot.like.ckb <- make_layer(
    '# Plot like a CKB plot',
    f = "ckbplotr::plot_like_ckb",
    arg = c('plot           = plot',
            sprintf('xlims          = %s', xlims),
            sprintf('ylims          = %s', ylims),
            sprintf('gap            = %s', gap),
            sprintf('ext            = %s', ext),
            sprintf('ratio          = %s', ratio),
            sprintf('base_size      = %s', base_size),
            sprintf('base_line_size = %s', base_line_size),
            sprintf('colour         = %s', plotcolour)),
    plus = TRUE
  )

  # Write code for theme
  codetext$theme <- make_layer(
    '# Add theme',
    f = "theme",
    arg = c(sprintf('legend.position = %s', deparse(legend.position))),
    plus = FALSE
  )

  # Create the plot code
  plotcode <- c(
    'library(ggplot2)',
    '',
    codetext$start.ggplot,
    indent(2,
           codetext$lines,
           codetext$cis.before,
           codetext$estimates.points,
           codetext$estimates.text,
           codetext$n.events.text,
           codetext$cis.after,
           codetext$scales,
           codetext$axes,
           codetext$titles),
    codetext$plot.like.ckb,
    indent(2, codetext$theme)
  )


  # add additional code
  if (!is.null(addcode)){
    plotcode <- append(plotcode, addcode[2:length(addcode)], grep(addcode[1], trimws(plotcode))[1]-1)
  }


  # Show code in RStudio viewer.
  if (showcode){ displaycode(plotcode) }


  # Create the plot
  plot <- eval(parse(text = plotcode), envir = envir)
  if (printplot){
    print(plot)
  }


  return(list(plot = plot,
              code = plotcode) )
}

