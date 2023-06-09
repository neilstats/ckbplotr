#' Make a forest plot with ggplot2
#'
#' Creates a forest plot with ggplot
#'
#' The function returns the plot and ggplot2 code to create the plot.
#' In RStudio, the ggplot2 code will be shown in the viewer.
#'
#'
#'
#' @inheritParams forest_data
#' @inheritParams theme_ckb
#' @param logscale Use log scale on the axis, and add a line at null effect. (Default: exponentiate)
#' @param panel.headings Titles to be placed above each forest plot.
#' @param row.labels.heading Title to be placed above row labels.
#' @param estcolumn Include column of estimates and confidence intervals to the
#' right of each plot. (Default: TRUE)
#' @param col.right.parse A logical vector, the same length as col.right (+ 1 if estcolumn = TRUE).
#' Should the contents of the columns be parsed into expressions. (Default: FALSE)
#' @param col.left.pos A unit vector to position col.right columns.
#' @param col.right.pos A unit vector to position col.right columns.
#' @param col.left.hjust A numeric vector. The horizontal justification of
#' col.left columns. (Default: 1)
#' @param col.right.hjust A numeric vector. The horizontal justification of
#' col.right columns. (Default: 0)
#' @param col.left.heading A character vector of titles for col.left columns. (Default: "")
#' @param col.right.heading A character vector of titles for the column of estimates
#' (if estcolumn = TRUE) and col.right columns. (Default: "HR (95% CI)")
#' @param col.heading.space Position of the titles given by col.left.heading and
#' col.right.heading. Increase to move them up. (Default: 0)
#' @param title Title to appear at the top of the plot.
#' @param xlab Label to appear below the x-axis. (Default: "HR (95% CI)")
#' @param xlim A numeric vector. The limits of the x axis.
#' @param xticks A numeric vector. The tick points of the x axis.
#' @param nullval Add a vertical reference line at this value. (If logscale == TRUE then by default it will be added at 1, but use NA not to plot this line.)
#' @param pointsize The (largest) size of box to use for plotting point
#'                  estimates. (Default: 3)
#' @param shape Shape of points. An integer, or name of a column of integers. (Default: 15 (square))
#' @param plotcolour Colour for all parts of the plot. (Default: "black")
#' @param colour Colour of points. Name of a colour, or name of a column of colour names. (Default will use plotcolour.)
#' @param cicolour Colour of CI lines. Colour of CI lines. Name of a colour, or name of a column of colour names. (Default will use colour.)
#' @param fill Fill colour of points. Name of a colour, or name of a column of colour names. (Default will use colour.)
#' @param ciunder Plot CI lines before points. A logical value, or name of a column of logical values. (Default will plot CI lines after points.)
#' @param col.diamond Plot estimates and CIs as diamonds. Name of a column of logical values.
#' @param diamond Alternative to col.diamond. A character vectors identify the rows
#'                (using the key values) for which the estimate and CI should be plotted using a diamond.
#' @param col.bold Plot text as bold. Name of a column of logical values.
#' @param bold.labels A character vector identifying row labels (using key values) which should additionally be bold. (Default: NULL)
#' @param bottom.space Size of space between bottom row and axis. (Default: 0.7)
#' @param left.space Size of gap to leave to the left of panels.
#' @param right.space Size of gap to leave to the right of panels.
#' @param mid.space Size of additional gap to leave between panels. (Default: unit(5, "mm"))
#' @param plot.margin Plot margin, given as margin(top, right, bottom, left, units). (Default: margin(8, 8, 8, 8, "mm"))

#' @param panel.width Panel width to set and apply different formatting to narrow CIs. A grid::unit object, if a numeric is given assumed to be in mm.
#' @param panel.height Set height of panels. A grid::unit object, if a numeric is given assumed to be in mm.
#' @param stroke Size of outline of shapes. (Default: 0)
#' @param diamonds.linewidth Line width for diamonds. (Default: base_line_size)
#' @param quiet Set to TRUE to not print the plot nor show generated code in the RStudio 'Viewer' pane. (Default: FALSE)
#' @param printplot Print the plot. (Default: !quiet)
#' @param showcode Show the ggplot2 code to generate the plot in RStudio 'Viewer' pane. (Default: !quiet)
#' @param data.function Name of a function to apply to data frame before plotting.
#' @param addcode A character vector of code to add to the generated code.
#'                The first element should be a regular expression.
#'                The remaining elements are added to the generated code just before the first match of a line (trimmed of  whitespace) with the regular expression. (Default: NULL)
#' @param addaes Specify additional aesthetics for some ggplot layers.
#' @param addarg Specify additional arguments for some ggplot layers.
#' @param envir Environment in which to evaluate the plot code. May be useful when calling this function inside another function.
#' @param label.space DEPRECATED. Old method for specifying spacing.
#' @param panel.space DEPRECATED. Old method for specifying spacing.
#' @param margin DEPRECATED. Old method for specifying margins.
#' @param colheadings DEPRECATED.
#' @param boldheadings DEPRECATED.
#' @param units DEPRECATED
#' @param col.right.space DEPRACTED
#' @param col.left.space DEPRACTED
#' @param heading.space DEPRECATED. Even older method for specifying spacing.
#' @param plot.space DEPRECATED. Even older method for specifying spacing.
#'
#' @return A list:
#' \describe{
#'   \item{plot}{the plot}
#'   \item{code}{ggplot2 code to generate the plot}
#'}
#'
#' @import ggplot2
#' @export




forest_plot <- function(
    panels,
    row.labels    = NULL,
    row.labels.levels = c("heading1", "heading2", "heading3"),
    row.labels.heading = NULL,
    rows          = NULL,
    exponentiate  = TRUE,
    logscale      = exponentiate,
    panel.names   = NULL,
    panel.headings = NULL,
    col.key       = "key",
    col.estimate  = "estimate",
    col.stderr    = "stderr",
    col.lci       = NULL,
    col.uci       = NULL,
    col.left      = NULL,
    col.right     = NULL,
    col.right.parse   = FALSE,
    col.left.heading  = "",
    col.right.heading = "HR (95% CI)",
    col.left.pos    = NULL,
    col.right.pos   = NULL,
    col.left.hjust    = 1,
    col.right.hjust   = 0,
    col.heading.space = 0,
    estcolumn     = TRUE,
    col.keep      = NULL,
    ci.delim      = ", ",
    digits        = 2,
    title         = "",
    xlab          = "HR (95% CI)",
    xlim          = NULL,
    xticks        = NULL,
    nullval       = NULL,
    blankrows     = c(1, 1, 0, 0),
    col.diamond   = NULL,
    diamond       = NULL,
    col.bold      = NULL,
    bold.labels   = NULL,
    scalepoints   = FALSE,
    minse         = NULL,
    pointsize     = 3,
    shape     = 15,
    plotcolour = "black",
    colour    = plotcolour,
    cicolour  = colour,
    fill      = colour,
    ciunder   = NULL,
    addtext       = NULL,
    bottom.space  = 0.7,
    left.space    = NULL,
    right.space   = NULL,
    mid.space     = unit(5, "mm"),
    plot.margin   = margin(8, 8, 8, 8, "mm"),
    panel.width   = NULL,
    panel.height  = NULL,
    base_size     = 11,
    base_line_size = base_size/22,
    stroke        = 0,
    diamonds.linewidth = base_line_size,
    quiet         = FALSE,
    printplot     = !quiet,
    showcode      = !quiet,
    data.function = NULL,
    addcode       = NULL,
    addaes        = NULL,
    addarg        = NULL,
    envir         = NULL,
    cols          = panels,
    headings      = NULL,
    colnames      = NULL,
    colheadings   = colnames,
    boldheadings  = NULL,
    heading.space = NULL,
    panel.space   = NULL,
    label.space   = NULL,
    plot.space    = NULL,
    col.right.space = NULL,
    col.left.space  = NULL,
    margin          = NULL,
    units          = NULL
){


  # Legacy arguments ----
  if (!missing(cols)) {
    panels <- cols
    message("Note: cols argument is now called panels")
  }
  if (!missing(headings)) {
    row.labels <- headings
    message("Note: headings argument is now called row.labels")
  }
  if (!missing(colnames)) {
    panel.names <- colnames
    message("Note: colnames argument is now called panel.names")
  }
  if (!missing(colheadings)) {
    panel.headings <- colheadings
    message("Note: colheadings argument is now called panel.headings")
  }
  if (!missing(boldheadings)) {
    bold.labels <- boldheadings
    message("Note: boldheadings argument is now called bold.labels")
  }


  # Check arguments ----
  fixed_panel_width <- !missing(panel.width)
  fixed_panel_height <- !missing(panel.height)
  column_names_in_data <- names(panels[[1]])

  check_forest_plot_arguments (col.diamond,
                               diamond,
                               col.left,
                               col.right,
                               panels,
                               cicolour,
                               fixed_panel_width)




  # Check for scale of x axis and transformation of estimates ----
  axis_scale_fn         <- identity
  axis_scale_inverse_fn <- identity
  axis_scale            <- "identity"
  if (logscale == TRUE) {
    axis_scale            <- "log"
    axis_scale_fn         <- log
    axis_scale_inverse_fn <- exp
    if (missing(nullval)) {nullval <- 1}
  }

  tf           <- identity
  inv_tf       <- identity
  if (exponentiate == TRUE) {
    tf     <- exp
    inv_tf <- log
  }



  # Transpose column headings if a list ----
  if (purrr::is_list(col.right.heading)){ col.right.heading <- purrr::transpose(col.right.heading)}
  if (purrr::is_list(col.left.heading)){ col.left.heading <- purrr::transpose(col.left.heading)}



  # Identify columns to keep in data frame ----
  col.keep <- c(col.keep, col.diamond, col.bold)
  for (x in c(shape, unlist(cicolour), colour, unlist(fill), ciunder)){
    if (x %in% column_names_in_data){ col.keep <- append(col.keep, x) }
  }



  # Take first element if diamond is a list ----
  if (is.list(diamond)){ diamond <- diamond[[1]] }


  # Default panel.names ----
  if (is.null(panel.names)) { panel.names <- as.character(1:length(panels)) }


  # Panel headings ----
  if (is.null(panel.headings)) { panel.headings <- names(panels) }


  # Create lists for aesthetics/arguments ----
  ## match column name, or use argument itself

  ### shape
  shape_list <- list(arg = shape)
  if (!missing(shape) && shape %in% column_names_in_data){
    shape_list <- list(aes = shape)
  }

  ### cicolour
  if (all(cicolour %in% column_names_in_data)){
    cicolour_list <- list(aes = cicolour)
  } else {
    if (missing(cicolour)) {
      cicolour <- c(cicolour, if (fill == "white") cicolour else "white")
    }
    cicolour_list <- list(arg = cicolour)
  }
  if(is.list(cicolour)){cicolour_list <- list(arg = cicolour)}

  ### colour
  colour_list <- list(arg = colour)
  if (!missing(colour) && all(colour %in% column_names_in_data)){
    colour_list <- list(aes = colour)
  }

  ### fill
  fill_list <- list(arg = fill)
  if (!is.list(fill) && fill %in% column_names_in_data){
    fill_list <- list(aes = fill)
  }
  if (is.list(fill)){
    fill_list <- list(aes = "fill",
                      values = fill)
  }




  # Aesthetic adjustments for fixed panel width ----
  if (fixed_panel_width) {
    if (!inherits(panel.width, "unit")){
      panel.width <- grid::unit(panel.width, "mm")
    }
    cicolour_list <- list(aes = "cicolour",
                          values = c(quote_string(cicolour_list$arg),
                                     column_name(cicolour_list$aes)))

    if (missing(ciunder)) {
      ciunder <- c(TRUE, FALSE)
    }

    if (length(ciunder) > 1) {
      ciunder_orig <- ciunder
      ciunder <- "ciunder"
    }
  }






  # Order for plotting CIs and points ----
  ci_order <- c("all", "null")
  if (isFALSE(ciunder) || is.null(ciunder)){
    ci_order <- c("null", "all")
  }
  if (is.character(ciunder)){
    ci_order <- c("before", "after")
  }





  # Panel.height ----
  if (fixed_panel_height & !inherits(panel.height, "unit")){
    panel.height <- grid::unit(panel.height, "mm")
  }




  # Text size ----
  text_size <- round(base_size_to_text_size(base_size), 6)




  # Spacing ----

  ## handle old methods for horizontal spacing and column positioning >>>
  if(!is.null(margin) | !is.null(label.space) | !is.null(panel.space) |
     !is.null(plot.space) | !is.null(heading.space) |
     !is.null(col.right.space) | !is.null(col.left.space) |
     !is.null(col.left.space) | !is.null(col.right.space)){
    message("You're using old arguments for horizontal spacing and positioning.\n",
            "See the package documentation for details on current methods.\n",
            "For now, I will try to convert these.")

    ## spacing
    units       <- if(!is.null(units)){units}else{"lines"}
    margin      <- if(!is.null(margin)){margin}else{c(2,6,2,1)}
    label.space <- ifelse(is.null(label.space), ifelse(is.null(heading.space), 4, heading.space), label.space)
    panel.space <- ifelse(is.null(panel.space), ifelse(is.null(plot.space), 8, plot.space), panel.space)
    left.space  <- unit(label.space, units)
    right.space <- unit(margin[2] - margin[4], units)
    mid.space   <- unit(panel.space - as.numeric(left.space) - as.numeric(right.space), units)
    plot.margin <- unit(margin - c(0, as.numeric(right.space), 0, 0), units)

    ## positioning
    col.left.space  <- if(!is.null(col.left.space)){col.left.space}else{0.02}
    col.right.space <- if(!is.null(col.right.space)){col.right.space}else{0.02}
    col.left.pos <- unit(0, "mm")
    col.right.pos <- unit(0, "mm")
  } else {
    col.left.space <- 0
    col.right.space <- 0
  }
  ## <<< end of handling old methods


  if((is.null(right.space) & !is.null(col.right.pos)) |
     is.null(left.space) & !is.null(col.left.pos) ){
    message("Note: Automatic spacing does not account for specified col.left.pos and col.right.pos. Use left.space and right.space to set spacing manually.")
  }

  gettextwidths <- function(x){
    purrr::map_dbl(x, ~ max(purrr::map_dbl(., ~ grid::convertWidth(unit(1, "strwidth", data = as.character(.)),
                                                                   "mm",
                                                                   valueOnly = T))))
  }

  ## calculate automatic col.right.pos and col.right.space
  if (is.null(right.space) | is.null(col.right.pos) | is.null(left.space) | is.null(col.left.pos)){
    text_about_auto_spacing <- "Automatically calculated horizontal spacing and positioning:\n"
  }
  ### get maximum width of each columns (incl. heading)
  widths_of_columns <- gettextwidths(lapply(col.right, function(y) c(sapply(panels, function(x) x[[y]]))))
  estcolumn_width <- gettextwidths(paste0("9.",
                                          paste0(rep(9, digits), collapse = ""),
                                          "(9.",
                                          paste0(rep(9, digits), collapse = ""),
                                          ci.delim,
                                          "99.",
                                          paste0(rep(9, digits), collapse = ""),
                                          ")"))
  widths_of_columns <- c(if(estcolumn){estcolumn_width}, widths_of_columns)
  widths_of_column_headings <- gettextwidths(col.right.heading)
  widths_of_columns <- pmax(widths_of_columns, widths_of_column_headings)
  ### initial gap, then space for autoestcolumn, and gap between each column
  column_spacing <- cumsum(c(gettextwidths("I"),
                             widths_of_columns + gettextwidths("W")))
  ## adjust for hjust
  column_spacing <- column_spacing + c(widths_of_columns*col.right.hjust, 0)
  ### if no column to plot (i.e. length 1) then zero, if longer don't need extra space on last element
  if (length(column_spacing) == 1){column_spacing <- 0}
  if (length(column_spacing) > 1){column_spacing[length(column_spacing)] <- column_spacing[length(column_spacing)] - gettextwidths("W")}
  ### text on plot is 0.8 size, and adjust for base_size
  column_spacing <-  round(0.8 * base_size/grid::get.gpar()$fontsize * column_spacing, 1)
  if (is.null(right.space)){
    right.space <- unit(column_spacing[length(column_spacing)], "mm")
    text_about_auto_spacing <- c(text_about_auto_spacing, paste0("- right.space   = ", printunit(right.space)))
  }
  if (length(column_spacing) > 1){column_spacing <- column_spacing[-length(column_spacing)]}
  if (is.null(col.right.pos)){
    col.right.pos <- unit(column_spacing, "mm")
    text_about_auto_spacing <- c(text_about_auto_spacing, paste0("- col.right.pos = ", printunit(col.right.pos)))
  }

  ## calculate automatic col.left.pos and col.left.space
  ### get maximum width of each columns (incl. heading)
  widths_of_columns <- gettextwidths(lapply(col.left, function(y) c(sapply(panels, function(x) x[[y]]))))
  widths_of_column_headings <- gettextwidths(col.left.heading)
  widths_of_columns <- pmax(widths_of_columns, widths_of_column_headings)
  ### initial gap, and gap between each column
  column_spacing <- cumsum(c(gettextwidths("I"),
                             widths_of_columns + gettextwidths("W")))
  ## adjust for hjust
  column_spacing <- column_spacing + c(widths_of_columns*(1 - col.left.hjust), 0)
  ### if no column to plot (i.e. length 1) then width of W, if longer keep extra space on last element
  if (length(column_spacing) == 1){column_spacing <- gettextwidths("W")}
  # if (length(column_spacing) > 1){column_spacing[length(column_spacing)] <- column_spacing[length(column_spacing)] - gettextwidths("W")}
  ### text on plot is 0.8 size, and adjust for base_size
  column_spacing <-  round(0.8 * base_size/grid::get.gpar()$fontsize * column_spacing, 1)
  if (is.null(left.space)){
    left.space <- unit(column_spacing[length(column_spacing)], "mm")
    text_about_auto_spacing <- c(text_about_auto_spacing, paste0("- left.space    = ", printunit(left.space)))
  }
  if (length(column_spacing) > 1){column_spacing <- column_spacing[-length(column_spacing)]}
  if (is.null(col.left.pos)){
    col.left.pos <- unit(column_spacing, "mm")
    text_about_auto_spacing <- c(text_about_auto_spacing, paste0("- col.left.pos  = ", printunit(col.left.pos)))
  }


  # Calculate xfrom, xto, xmid, xticks ----
  ## xfrom, xto, etc. are used by other code sections, so this must come first
  if (is.null(xlim)) {
    if (is.null(col.lci)) {
      allvalues <- sapply(panels, function(x) c(tf(x[[col.estimate]] - 1.96 * x[[col.stderr]]),
                                                tf(x[[col.estimate]] + 1.96 * x[[col.stderr]])))
    } else {
      allvalues <- sapply(panels, function(x) c(tf(x[[col.lci]]),
                                                tf(x[[col.uci]])))
    }
    xlim <- range(pretty(allvalues))
    ## check for zero as axis limit when using exponential
    if (exponentiate & isTRUE(all.equal(0, xlim[[1]]))){
      xlim[[1]] <- min(allvalues, na.rm = TRUE)
    }
  }

  xfrom <- min(xlim)
  xto   <- max(xlim)
  xmid  <- round(axis_scale_inverse_fn((axis_scale_fn(xfrom) + axis_scale_fn(xto)) / 2), 6)
  if (is.null(xticks)) { xticks <- pretty(c(xfrom, xto)) }



  # Code for preparing data for plotting using forest_data() ----
  prep.data.code <- make_layer(
    name = '# Prepare data to be plotted using ckbplotr::forest_data()',
    plus = FALSE,
    f = 'datatoplot <- ckbplotr::forest_data',
    arg = c(
      if (!identical(row.labels,
                     eval(formals(ckbplotr::forest_data)[["row.labels"]]))){
        'row.labels = {
        if (!missing(headings)) {{
          paste(deparse(substitute(headings)), collapse = "")
        }} else {{
          paste(deparse(substitute(row.labels)), collapse = "")
        }}
        }'
      },
      argset(row.labels.levels),
      argset(rows),
      'panels = {
      if (!missing(cols)) {{
        paste(deparse(substitute(cols)), collapse = "")
      }} else {{
        paste(deparse(substitute(panels)), collapse = "")
      }}
      }',
      argset(panel.names),
      argset(col.key),
      argset(col.estimate),
      argset(col.stderr),
      argset(col.lci),
      argset(col.uci),
      argset(col.left),
      argset(col.right),
      argset(col.keep),
      argset(ci.delim),
      argset(digits),
      argset(exponentiate),
      argset(blankrows),
      argset(scalepoints),
      argset(minse),
      argset(bold.labels),
      if (!identical(addtext,
                     eval(formals(ckbplotr::forest_data)[["addtext"]]))){
        glue::glue('addtext = {paste(deparse(substitute(addtext)), collapse = "")}')
      }))




  # Create the plot code ----
  plotcode <- c(
    'library(ggplot2)',
    '',

    # code to prepare data for plotting using forest_data()
    prep.data.code,

    # fill may be a list
    if (!is.null(fill_list$values)){forest.fillcode(fill_list$values, panel.names)},

    # code for preparing data for diamonds
    if(!is.null(col.diamond) || !is.null(diamond)){
      forest.diamondscode(diamond,
                          col.diamond,
                          panel.names)
    },

    # code for CI colours if using panel.width
    if (fixed_panel_width) {
      forest.cicolourcode(axis_scale,
                          axis_scale_fn,
                          xto,
                          xfrom,
                          pointsize,
                          stroke,
                          panel.width,
                          shape_list,
                          cicolour_list,
                          panel.names)
    },

    ## code for CI under - if using panel.width
    if (exists("ciunder_orig")) {
      forest.ciundercode(ciunder_orig)
    },

    # code for user function on datatoplot
    glue::glue_safe('datatoplot <- {data.function}(datatoplot)'),
    '',

    # code to initiate the ggplot
    forest.start.ggplot(),

    indent(2,

           # the code to put panels in facets
           forest.facet(),

           # code for line at null
           if (!is.null(nullval)) { forest.nullline(nullval, base_line_size, plotcolour) },

           # code for CI lines plotted before points
           forest.cis(addaes,
                      cicolour_list,
                      addarg,
                      ciunder,
                      base_line_size,
                      xfrom,
                      xto,
                      type = ci_order[[1]]),

           # code to plot points
           forest.plot.points(addaes,
                              shape_list,
                              colour_list,
                              fill_list,
                              addarg,
                              xfrom,
                              xto,
                              stroke,
                              pointsize),

           # code for CI lines plotted after points
           forest.cis(addaes,
                      cicolour_list,
                      addarg,
                      ciunder,
                      base_line_size,
                      xfrom,
                      xto,
                      type = ci_order[[2]]),

           # code to add arrows to CIs
           forest.arrows(addaes, cicolour_list, addarg, base_line_size, xfrom, xto),

           # code for plotting diamonds
           if(!is.null(col.diamond) || !is.null(diamond)){
             forest.plotdiamondscode(colour_list,
                                     fill_list,
                                     diamonds.linewidth)
           },

           # code for scales and coordinates
           forest.scales.coords(xfrom, xto),

           # code for columns to right of panel
           if (!is.null(col.right) | estcolumn) {
             col.right.all <- c(if (estcolumn){"auto_estcolumn"}, col.right)
             forest.columns.right(col.right.all,
                                  col.right.pos,
                                  col.right.heading,
                                  col.right.hjust,
                                  col.bold,
                                  col.right.parse,
                                  col.right.space,
                                  addaes,
                                  addarg,
                                  xto,
                                  xfrom,
                                  text_size,
                                  plotcolour,
                                  col.heading.space,
                                  axis_scale_fn,
                                  axis_scale_inverse_fn)
           },

           # code for columns to left of panel
           if (!is.null(col.left)) {
             forest.columns.left(col.left,
                                 col.left.pos,
                                 col.left.heading,
                                 col.left.hjust,
                                 col.bold,
                                 col.left.space,
                                 addaes,
                                 addarg,
                                 xfrom,
                                 xto,
                                 text_size,
                                 plotcolour,
                                 col.heading.space,
                                 axis_scale_fn,
                                 axis_scale_inverse_fn)
           },

           # code for addtext
           if (!is.null(addtext)){
             forest.addtext(xto,
                            xfrom,
                            col.right.space,
                            col.bold,
                            col.right.parse,
                            col.right.pos,
                            col.right.hjust,
                            text_size,
                            plotcolour,
                            axis_scale_fn,
                            axis_scale_inverse_fn)
           },

           # code for x-axis labels and panel headings
           forest.xlab.panel.headings(addaes,
                                      xmid,
                                      addarg,
                                      text_size,
                                      plotcolour,
                                      xlab,
                                      panel.headings,
                                      col.heading.space),

           # code for the axes
           forest.axes(axis_scale,
                       xticks,
                       row.labels.heading,
                       bottom.space,
                       col.heading.space),

           # code for panel size
           if (fixed_panel_width | fixed_panel_height){
             forest.panel.size(panel.width,
                               panel.height)
           },

           # code for the plot title
           if (title != ""){forest.title(title)},

           # Write code for the theme
           forest.theme(base_size,
                        plotcolour,
                        base_line_size,
                        title,
                        left.space,
                        right.space,
                        substitute(mid.space),
                        substitute(plot.margin))
    )
  )

  # Add additional code ----
  if (!is.null(addcode)){
    plotcode <- append(plotcode, addcode[2:length(addcode)], grep(addcode[1], trimws(plotcode))[1]-1)
  }

  # Show code in RStudio viewer ----
  if (showcode){ displaycode(plotcode, text_about_auto_spacing) }

  # If envir not provided, make new environment ----
  # with parent frame same as function call
  if(missing(envir)){envir <- new.env(parent = parent.frame())}

  # Create plot and print ----
  plot <- eval(parse(text = plotcode), envir = envir)
  if (printplot){
    print(plot)
  }

  # Return invisible ----
  return(invisible(list(plot = plot,
                        code = plotcode)))
}



#' @describeIn forest_plot Synonym for `forest_plot()`
#' @export
make_forest_plot <- forest_plot



#' Check arguments of forest_plot()
#'
#' @keywords internal
#' @noRd
check_forest_plot_arguments <- function(col.diamond,
                                        diamond,
                                        col.left,
                                        col.right,
                                        panels,
                                        cicolour,
                                        fixed_panel_width,
                                        call = rlang::caller_env()) {
  if (!is.null(col.diamond) &&  !is.null(diamond)){
    rlang::abort("Use either col.diamond or diamond, not both.", call = call)
  }

  for (c in c(col.left, col.right)){
    if (any(unlist(lapply(panels, function(x) !c %in% names(x))))){
      rlang::abort(glue::glue("Column '{c}' does not exist in every panels data frame."),
                   call = call)
    }
  }

  ## check if cicolour is a list (or longer than 1) but not using panel.width
  if ((is.list(cicolour) | length(cicolour) > 1) & !fixed_panel_width){
    rlang::abort("cicolour should be a list (or longer than 1) only when using panel.width",
                 call = call)
  }

  ## check if confidence intervals may be hidden
  if (!fixed_panel_width){
    rlang::inform(c('i' = 'Narrow confidence interval lines may become hidden in the forest plot.',
                    'i' = 'Please check your final output carefully and see vignette("forest_confidence_intervals") for more details.'),
                  use_cli_format = TRUE,
                  .frequency = "once",
                  .frequency_id = "forest_narrow_cis")
  }
}

