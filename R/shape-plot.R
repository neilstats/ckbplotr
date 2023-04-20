
#' Make a shape plot with ggplot2
#'
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
#' @param shape Shape of points. An integer, or name of a column of integers. (Default: 15)
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
#' @param digits Number of digits to use in text of estimates.
#' @param minse Minimum standard error to use when scaling point size. (Default will use minimum in the data.)
#' @param pointsize The (largest) size of box to use for plotting point
#'                  estimates. (Default: 3)
#' @param xlab Label for x-axis. (Default: "Risk factor")
#' @param ylab Label for y-axis. (Default: "Estimate (95% CI)")
#' @param legend.name The name of the colour scale/legend for groups. (Default: "")
#' @param legend.position Position of the legend for groups ("none", "left", "right", "bottom", "top", or two-element numeric vector). (Default: "top")
#' @param title Plot title. (Default: NULL)
#' @param xlims A numeric vector of length two. The limits of the x-axis.
#' @param ylims A numeric vector of length two. The limits of the y-axis.
#' @param height Panel height to use and apply different formatting to short CIs. A grid::unit() object, or if numeric is assumed to be in mm.
#' @param width Panel width.A grid::unit() object, or if numeric is assumed to be in mm.
#' @param xbreaks Breaks for the x axis. Passed to ggplots::scale_x_continuous. (Default: NULL)
#' @param ybreaks Breaks for the y axis. Passed to ggplots::scale_y_continuous. (Default: NULL)
#' @param gap A numeric vector of length two. The gap between plotting area and axis to the left and bottom of the plot, as a proportion of the x-axis length. (Default: c(0.025, 0.025))
#' @param ext A numeric vector of length two. The extensions to add to the right and top of the plot, as a proportion of the x-axis length. (Default: c(0.025, 0.025))
#' @param ratio The ratio (y-axis:x-axis) to use for the plot. (Default: 1.5)
#' @param stroke Size of outline of shapes. (Default: base_size/22)
#' @param quiet Set to TRUE to not print the plot nor show generated code in the RStudio 'Viewer' pane. (Default: FALSE)
#' @param printplot Print the plot. (Default: !quiet)
#' @param showcode Show the ggplot2 code to generate the plot in RStudio 'Viewer' pane. (Default: !quiet)
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



shape_plot <- function(data,
                       col.x         = "x",
                       col.estimate  = "estimate",
                       col.stderr    = "stderr",
                       col.lci       = NULL,
                       col.uci       = NULL,
                       col.n         = NULL,
                       exponentiate  = FALSE,
                       logscale      = exponentiate,
                       scalepoints   = FALSE,
                       digits        = 2,
                       minse         = NA,
                       pointsize     = 3,
                       col.group     = NULL,
                       shape         = 15,
                       plotcolour    = "black",
                       colour        = plotcolour,
                       cicolour      = colour,
                       fill          = colour,
                       ciunder       = NULL,
                       lines         = FALSE,
                       xlims,
                       ylims,
                       height        = NULL,
                       width         = NULL,
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
                       title         = NULL,
                       quiet         = FALSE,
                       printplot     = !quiet,
                       showcode      = !quiet,
                       addcode       = NULL,
                       addaes        = NULL,
                       addarg        = NULL,
                       envir         = NULL){

  # Check arguments ----
  if (!is.null(col.lci) &&  is.null(col.uci)) stop("col.lci and col.uci must both be specified")
  if ( is.null(col.lci) && !is.null(col.uci)) stop("col.lci and col.uci must both be specified")
  if (!is.null(col.group) && !missing(fill)) stop("col.group and fill both control fill, so do not specify both")
  if (missing(xlims)) stop("xlims must be specified")
  if (missing(ylims)) stop("ylims must be specified")

  ## check if confidence intervals may be hidden
  if (missing(height)){
    rlang::inform(c('i' = 'Narrow confidence interval lines may become hidden in the shape plot.',
                    'i' = 'Please check your final output carefully and see vignette("shape_confidence_intervals") for more details.'),
                  use_cli_format = TRUE,
                  .frequency = "once",
                  .frequency_id = "shape_narrow_cis")
  }

  if(!missing(height) && !missing(col.group) && !missing(cicolour)){
    warning("cicolour is ignored if using height and col.group")
  }


  # Aesthetics ----
  ##  match column name, or use argument itself

  ### shape
  if (missing(shape) && !is.null(col.group)){
    shape <- 22
  }
  if (!missing(shape) && shape %in% names(data)){
    shape <- list(aes = shape)
  } else {
    shape <- list(arg = shape)
  }

  ### cicolour
  if (all(cicolour %in% names(data))){
    cicolour <- list(aes = cicolour)
  } else {
    if (missing(cicolour)) {
      cicolour <- c(cicolour, "white")
      if (fill == "white") {
        cicolour <- c(cicolour[[1]], cicolour[[1]])
      }
    }
    cicolour <- list(arg = cicolour)
  }

  ### colour
  if (!missing(colour) && colour %in% names(data)){
    colour <- list(aes = colour)
  } else {
    colour <- list(arg = colour)
  }

  ### fill
  if (fill %in% names(data)){
    fill <- list(aes = fill)
  } else {
    fill <- list(arg = fill)
  }


  # String for point size aesthetic
  if (scalepoints) {
    if (!is.null(col.lci)) {
      size <- sprintf('2*1.96/(%s - %s)', column_name(col.uci), column_name(col.lci))
    } else {
      size <- sprintf('1/%s', column_name(col.stderr))
    }
  } else {
    size <- '1'
  }




  # Text size ----
  text_size <- round(base_size_to_text_size(base_size), 6)






  # Log scale and exponentiate estimates ----
  if (logscale == TRUE){
    scale <- "log"
  } else {
    scale <- "identity"
  }
  if (exponentiate == TRUE) {
    est_string <- paste0('exp(', column_name(col.estimate), ')')
    if (!is.null(col.lci)) {
      lci_string <- paste0('exp(', column_name(col.lci), ')')
      uci_string <- paste0('exp(', column_name(col.uci), ')')
    } else {
      lci_string <- paste0('exp(',
                           column_name(col.estimate),
                           '-1.96*',
                           column_name(col.stderr),
                           ')')
      uci_string <- paste0('exp(',
                           column_name(col.estimate),
                           '+1.96*',
                           column_name(col.stderr),
                           ')')
    }
  } else {
    est_string <- column_name(col.estimate)
    if (!is.null(col.lci)) {
      lci_string <- column_name(col.lci)
      uci_string <- column_name(col.uci)
    } else {
      lci_string <- paste0(column_name(col.estimate),
                           '-1.96*',
                           column_name(col.stderr))
      uci_string <- paste0(column_name(col.estimate),
                           '+1.96*',
                           column_name(col.stderr))
    }
  }


  # Aesthetic adjustments when using height ----
  if (!missing(height)) {
    if (!inherits(height, "unit")){
      height <- grid::unit(height, "mm")
    }
    cicolours <- c(quote_string(cicolour$arg), column_name(cicolour$aes))
    cicolour <- list(aes = "cicolour")
  }

  if (!missing(height)) {
    if (!missing(ciunder)) warning("ciunder ignored when using height")
    ciunder <- "ciunder"
  }


  # Width ----
  if (!missing(width) & !inherits(width, "unit")){
    width <- grid::unit(width, "mm")
  }


  # Using groups ----
  if (!is.null(col.group)) {

    if(!is.factor(data[[col.group]])) stop("col.group must be factor")
    group_string <- sprintf(', group = %s', column_name(col.group))
    scale_fill_string <- c('',
                           make_layer('# Set the scale for fill colours',
                                      f = "scale_fill_grey",
                                      arg = c("start = 0",
                                              "end   = 1",
                                              sprintf('name  = "%s"', legend.name)),
                                      br = FALSE))
    fill_string <- list(aes = sprintf('fill = %s', column_name(col.group)))
  } else {
    group_string <- ''
    scale_fill_string <- 'scale_fill_identity() +'
    fill_string <- list(aes = sprintf('fill = %s', column_name(fill$aes)),
                        arg = sprintf('fill = %s', quote_string(fill$arg)))
  }


  # Order for plotting CIs and points ----
  ci_order <- c("all", "null")
  if (isFALSE(ciunder) || is.null(ciunder)){
    ci_order <- c("null", "all")
  }
  if (is.character(ciunder)){
    ci_order <- c("before", "after")
  }


  # Create the plot code ----
  plotcode <- c(
    'library(ggplot2)',
    '',

    # start with data
    paste0('datatoplot <- ', deparse(substitute(data))),
    '',

    # code for CI colours if using height
    shape.cicolourcode(scale,
                       ylims,
                       lci_string,
                       uci_string,
                       pointsize,
                       size,
                       stroke,
                       height,
                       ratio,
                       gap,
                       ext,
                       shape,
                       cicolours,
                       col.group),

    ## code for CI under - if using height
    shape.ciundercode(height),

    ## start ggplot
    shape.start.ggplot(column_name(col.x),
                       est_string,
                       group_string),

    indent(2,

           ## add lines
           if(lines){
             shape.lines(addaes,
                         col.lci,
                         col.estimate,
                         col.stderr,
                         addarg,
                         plotcolour)
           },

           # CI lines plotted before points
           shape.cis(addaes,
                     lci_string,
                     uci_string,
                     cicolour,
                     addarg,
                     ciunder,
                     base_line_size,
                     type = ci_order[[1]]),

           # points for estimates
           shape.estimates.points(addaes,
                                  size,
                                  shape,
                                  fill_string,
                                  colour,
                                  addarg,
                                  stroke),

           # text above points
           shape.estimates.text(addaes,
                                uci_string,
                                est_string,
                                addarg,
                                text_size,
                                plotcolour,
                                digits),

           # number below points
           if (!is.null(col.n)){
             shape.n.events.text(addaes,
                                 lci_string,
                                 col.n,
                                 addarg,
                                 text_size,
                                 plotcolour)
           },

           # CI lines plotted after points
           shape.cis(addaes,
                     lci_string,
                     uci_string,
                     cicolour,
                     addarg,
                     ciunder,
                     base_line_size,
                     type = ci_order[[2]]),

           # scales
           shape.scales(deparse(1/minse), pointsize, scale_fill_string),

           # axes
           shape.axes(deparse(xbreaks), scale, deparse(ybreaks)),

           # titles
           shape.titles(xlab, title, ylab)),

    # plot_like_ckb()
    shape.plot.like.ckb(deparse(xlims),
                        deparse(ylims),
                        deparse(gap),
                        deparse(ext),
                        deparse(ratio),
                        width,
                        height,
                        base_size,
                        base_line_size,
                        plotcolour),

    # theme
    indent(2, shape.theme(legend.position))
  )


  # add additional code
  if (!is.null(addcode)){
    plotcode <- append(plotcode,
                       addcode[2:length(addcode)],
                       grep(addcode[1], trimws(plotcode))[1]-1)
  }


  # Show code in RStudio viewer.
  if (showcode){ displaycode(plotcode) }


  # If envir not provided, make new environment
  # with parent frame same as function call
  if(missing(envir)){envir <- new.env(parent = parent.frame())}

  # Create the plot
  plot <- eval(parse(text = plotcode), envir = envir)
  if (printplot){
    print(plot)
  }


  return(invisible(list(plot = plot,
                        code = plotcode)))
}



#' @describeIn shape_plot Synonym for `shape_plot()`
#' @export
make_shape_plot <- shape_plot
