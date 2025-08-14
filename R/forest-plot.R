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
#' @param logscale
#' Use log scale on the axis, and add a line at null effect. (Default: exponentiate)
#' @param panel.headings
#' Titles to be placed above each forest plot.
#' @param panel.headings.align
#' Panel headings are by default centred over the plotting area ("panel").
#' Set to "plot" to centre over plotting area and text columns.
#' @param row.labels.heading
#' Title to be placed above row labels.
#' @param estcolumn
#' Include column of estimates and confidence intervals to the
#' right of each plot. (Default: TRUE)
#' @param right.parse
#' A logical vector, the same length as col.right (+ 1 if estcolumn = TRUE).
#' Should the contents of the columns be parsed into expressions. (Default: FALSE)
#' @param left.pos,right.pos
#' A unit vector to position col.left/col.right columns.
#' @param left.hjust,right.hjust
#' A numeric vector. The horizontal justification of
#' col.left/col.right columns. (Default: 1)
#' @param left.gap,right.gap
#' A character vector of length two. The two characters control the gaps between
#' the first text column and the panel, and successive text columns.
#' (Default: c("I", "W"))
#' @param left.heading,right.heading
#' Headings for columns.
#' @param heading.space
#' Position of the titles given by left.heading and
#' right.heading. Increase to move them up. (Default: 0)
#' @param heading.rule
#' Include a horizontal rule below column headings? (Default: FALSE)
#' Can be TRUE/FALSE, or a grid::unit() object specifying
#' extension beyond the panel edges (length 1 for symmetric,
#' or 2 for left and right).
#' @param heading.rule
#' Include a horizontal rule below column headings? (Default: FALSE)
#' @param title
#' Title to appear at the top of the plot.
#' @param xlab
#' Label to appear below the x-axis. (Default: "HR (95% CI)")
#' @param xlim
#' A numeric vector. The limits of the x axis.
#' @param xticks
#' A numeric vector. The tick points of the x axis.
#' @param nullval
#' Add a vertical reference line at this value. (If logscale == TRUE then by default it will be added at 1, but use NA not to plot this line.)
#' @param pointsize
#' The (largest) size of box to use for plotting point estimates. (Default: 3)
#' @param shape
#' Shape of points. An integer, or name of a column of integers. (Default: 15 (square))
#' @param plotcolour
#' Colour for all parts of the plot. (Default: "black")
#' @param colour
#' Colour of points. Name of a colour, or name of a column of colour names. (Default will use plotcolour.)
#' @param cicolour
#' Colour of CI lines. Colour of CI lines. Name of a colour, or name of a column of colour names. (Default will use colour.)
#' @param fill
#' Fill colour of points. Name of a colour, or name of a column of colour names. (Default will use colour.)
#' @param ciunder
#' Plot CI lines before points. A logical value, or name of a column of logical values. (Default will plot CI lines after points.)
#' @param col.bold
#' Plot text as bold. Name of a column of logical values.
#' @param bottom.space
#' Space between bottom row and axis. (Default: 0.7)
#' @param left.space,right.space,mid.space
#' Space to the left/right/between panels.
#' (Default mid.space: unit(5, "mm"))
#' @param plot.margin
#' Plot margin, given as margin(top, right, bottom, left, units). (Default: margin(8, 8, 8, 8, "mm"))
#' @param panel.width,panel.height
#' Set width/height of panels. A grid::unit object, if a numeric is given assumed to be in mm.
#' If panel.width is used, will alsovapply different formatting to narrow CIs.
#' @param base_size
#' base font size, given in pts.
#' @param base_line_size
#' base size for line elements
#' @param stroke
#' Size of outline of shapes. (Default: 0)
#' @param diamonds.linewidth
#' Line width for diamonds. (Default: base_line_size)
#' @param quiet
#' Set to TRUE to not print the plot nor show generated code in the RStudio 'Viewer' pane. (Default: FALSE)
#' @param printplot
#' Print the plot. (Default: !quiet)
#' @param showcode
#' Show the ggplot2 code to generate the plot in RStudio 'Viewer' pane. (Default: !quiet)
#' @param data.function
#' Name of a function to apply to data frame before plotting.
#' @param addaes,addarg,add
#' Methods for customising the plot. See documentation for details.
#' @param envir
#' Environment in which to evaluate the plot code.
#' May be useful when calling this function inside another function.
#' @param blankrows
#' DEPRECATED
#' @param col.right.parse Deprecated. Use `right.parse` instead.
#' @param col.left.heading Deprecated. Use `left.heading` instead.
#' @param col.right.heading Deprecated. Use `right.heading` instead.
#' @param col.left.pos Deprecated. Use `left.pos` instead.
#' @param col.right.pos Deprecated. Use `right.pos` instead.
#' @param col.left.hjust Deprecated. Use `left.hjust` instead.
#' @param col.right.hjust Deprecated. Use `right.hjust` instead.
#' @param col.left.gap Deprecated. Use `left.gap` instead.
#' @param col.right.gap Deprecated. Use `right.gap` instead.
#' @param col.heading.space Deprecated. Use `heading.space` instead.
#' @param col.heading.rule Deprecated. Use `heading.rule` instead.
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
    row.labels         = NULL,
    row.labels.levels  = NULL,
    rows               = NULL,
    row.labels.heading = NULL,
    row.labels.space   = c(0, 1, 0, 0),
    exponentiate       = TRUE,
    logscale           = exponentiate,
    panel.names        = NULL,
    panel.headings     = NULL,
    panel.headings.align  = c("panel", "plot"),
    col.key            = "key",
    col.estimate       = c("estimate", "est", "beta", "loghr"),
    col.stderr         = c("stderr", "std.error", "std.err", "se"),
    col.lci            = NULL,
    col.uci            = NULL,
    col.left           = NULL,
    col.right          = NULL,
    left.parse        = FALSE,
    right.parse        = FALSE,
    left.heading       = "",
    right.heading      = as.list(xlab),
    left.pos           = NULL,
    right.pos          = NULL,
    left.hjust         = 1,
    right.hjust        = 0,
    left.gap           = c("I", "W"),
    right.gap          = c("I", "W"),
    heading.space      = 0,
    heading.rule       = FALSE,
    estcolumn          = TRUE,
    col.keep           = NULL,
    ci.delim           = ", ",
    digits             = 2,
    title              = "",
    xlab               = "HR (95% CI)",
    xlim               = NULL,
    xticks             = NULL,
    nullval            = NULL,
    col.diamond        = NULL,
    diamond            = NULL,
    col.bold           = NULL,
    bold.labels        = NULL,
    scalepoints        = FALSE,
    minse              = NULL,
    pointsize          = 3,
    shape              = 15,
    plotcolour         = "black",
    colour             = plotcolour,
    cicolour           = colour,
    fill               = colour,
    ciunder            = NULL,
    addtext            = NULL,
    bottom.space       = 0.7,
    left.space         = NULL,
    right.space        = NULL,
    mid.space          = unit(5, "mm"),
    plot.margin        = margin(2, 8, 2, 8, "mm"),
    panel.width        = NULL,
    panel.height       = NULL,
    base_size          = 11,
    base_line_size     = base_size/22,
    stroke             = 0,
    diamonds.linewidth = base_line_size,
    quiet              = FALSE,
    printplot          = !quiet,
    showcode           = !quiet,
    data.function      = NULL,
    addaes             = NULL,
    addarg             = NULL,
    add                = NULL,
    envir              = NULL,
    blankrows          = NULL,

    # Deprecated arguments
    col.right.parse   = NULL,
    col.left.heading  = NULL,
    col.right.heading = NULL,
    col.left.pos      = NULL,
    col.right.pos     = NULL,
    col.left.hjust    = NULL,
    col.right.hjust   = NULL,
    col.left.gap      = NULL,
    col.right.gap     = NULL,
    col.heading.space = NULL,
    col.heading.rule  = NULL
){

  # Helper for consistent deprecation messaging
  deprecate_arg <- function(old, new) {
    cli::cli_warn(c(
      "x" = sprintf("`%s` is deprecated.", old),
      ">" = sprintf("Please use {.arg %s} instead.", new)
    ))
  }

  if (!is.null(col.right.parse)) {
    deprecate_arg("col.right.parse", "right.parse")
    right.parse <- col.right.parse
  }
  if (!is.null(col.left.heading)) {
    deprecate_arg("col.left.heading", "left.heading")
    left.heading <- col.left.heading
  }
  if (!is.null(col.right.heading)) {
    deprecate_arg("col.right.heading", "right.heading")
    right.heading <- col.right.heading
  }
  if (!is.null(col.left.pos)) {
    deprecate_arg("col.left.pos", "left.pos")
    left.pos <- col.left.pos
  }
  if (!is.null(col.right.pos)) {
    deprecate_arg("col.right.pos", "right.pos")
    right.pos <- col.right.pos
  }
  if (!is.null(col.left.hjust)) {
    deprecate_arg("col.left.hjust", "left.hjust")
    left.hjust <- col.left.hjust
  }
  if (!is.null(col.right.hjust)) {
    deprecate_arg("col.right.hjust", "right.hjust")
    right.hjust <- col.right.hjust
  }
  if (!is.null(col.left.gap)) {
    deprecate_arg("col.left.gap", "left.gap")
    left.gap <- col.left.gap
  }
  if (!is.null(col.right.gap)) {
    deprecate_arg("col.right.gap", "right.gap")
    right.gap <- col.right.gap
  }
  if (!is.null(col.heading.space)) {
    deprecate_arg("col.heading.space", "heading.space")
    heading.space <- col.heading.space
  }
  if (!is.null(col.heading.rule)) {
    deprecate_arg("col.heading.rule", "heading.rule")
    heading.rule <- col.heading.rule
  }


  # If envir not provided, make new environment ----
  # with parent frame same as function call
  if(missing(envir)){envir <- new.env(parent = parent.frame())}

  # When xlim is a list ----
  if (is.list(xlim)){
    original_arguments <- as.list(match.call())[-1]
    return(forest_plot_list_xlim(
      ## original arguments passed to
      ## separate forest_plot() calls
      original_arguments,
      ## arguments that need to be evaluated first
      ## because they are used by forest_plot_list_xlim()
      xlim,
      xticks,
      panels,
      xlab,
      nullval,
      left.heading,
      right.heading,
      panel.headings,
      ## the new environment in which to evaluate forest_plot()
      envir))
  }

  # Check arguments ----
  panels_list <- panels
  if (is.data.frame(panels)) {
    panels_list <- list(panels)
  }

  fixed_panel_width <- !missing(panel.width)
  fixed_panel_height <- !missing(panel.height)
  column_names_in_data <- purrr::reduce(lapply(panels_list, names), intersect)

  # blankrows no longer used
  if (!missing(blankrows)){
    row.labels.space <- blankrows
    rlang::warn("Note: blankrows argument now called row.labels.space")
  }

  ## check col.left and col.right columns exist
  for (c in c(col.left, col.right)){
    if (!c %in% column_names_in_data){
      rlang::abort(glue::glue("Column '{c}' does not exist in every panels data frame."))
    }
  }

  ## check if cicolour is a list (or longer than 1) but not using panel.width
  if ((is.list(cicolour) | length(cicolour) > 1) & !fixed_panel_width){
    rlang::abort("cicolour should be a list (or longer than 1) only when using panel.width")
  }

  ## check if confidence intervals may be hidden
  if (!fixed_panel_width){
    rlang::inform(c('i' = 'Narrow confidence interval lines may become hidden in the forest plot.',
                    'i' = 'Please check your final output carefully and see vignette("forest_confidence_intervals") for more details.'),
                  use_cli_format = TRUE,
                  .frequency = "once",
                  .frequency_id = "forest_narrow_cis")
  }




  # Match estimate and stderr column names ----
  if (length(col.estimate[col.estimate %in% column_names_in_data]) == 0) {
    rlang::abort(glue::glue("Column '{col.estimate}' does not exist in panels data frame."))
  }
  col.estimate <- col.estimate[col.estimate %in% column_names_in_data][[1]]

  if (!is.null(col.lci) | !is.null(col.uci)) {
    for (x in c(col.lci, col.uci)){
      if (!x %in% column_names_in_data){
        rlang::abort(glue::glue("Column '{x}' does not exist in panels data frame."))
      }
    }
  } else {
    if (length(col.stderr[col.stderr %in% column_names_in_data]) == 0) {
      rlang::abort(glue::glue("Column '{col.stderr}' does not exist in panels data frame."))
    }
    col.stderr <- col.stderr[col.stderr %in% column_names_in_data][[1]]
  }


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
  if (purrr::is_list(right.heading)){ right.heading <- purrr::transpose(right.heading)}
  if (purrr::is_list(left.heading)){ left.heading <- purrr::transpose(left.heading)}



  # Identify columns to keep in data frame ----
  col.keep <- c(col.keep, col.diamond, col.bold)
  for (x in c(shape, unlist(cicolour), colour, unlist(fill), ciunder)){
    if (x %in% column_names_in_data){ col.keep <- append(col.keep, x) }
  }


  # Make right.parse and left.parse character vectors ----
  if (is.logical(right.parse)) {
    right.parse <- ifelse(right.parse, "col", "none")
  }
  if (is.logical(left.parse)) {
    left.parse <- ifelse(left.parse, "col", "none")
  }


  # Take first element if diamond is a list ----
  if (is.list(diamond)){ diamond <- diamond[[1]] }


  # Default panel.names ----
  if (is.null(panel.names)) { panel.names <- as.character(1:length(panels_list)) }


  # Panel headings ----
  if (is.null(panel.headings)) { panel.headings <- names(panels_list) }
  panel.headings.align <- match.arg(panel.headings.align)

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
      if (!is.list(fill)) {
        cicolour <- c(cicolour, if (fill == "white" & !shape %in% c(15, "square", "circle")) cicolour else "white")
      } else {
        cicolour <- lapply(fill,
                           \(x) c(cicolour, if (x == "white" & !shape %in% c(15, "square", "circle")) cicolour else "white") )
      }
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
    fill_aes <- c(
      'dplyr::case_when(',
      purrr::map_chr(1:length(fill),
                     \(i) glue::glue('panel == {quote_string(panel.names[[{i}]])} ~ {quote_string(fill[[{i}]][1])}, ')
      ),
      'TRUE ~ "black")'
    )
    fill_list <- list(string_aes = paste(fill_aes, collapse = ""))
  }

  # Aesthetic adjustments for fixed panel width ----
  if (fixed_panel_width) {
    if (!inherits(panel.width, "unit")){
      panel.width <- grid::unit(panel.width, "mm")
    }
    cicolour_list <- list(string_aes = forest.cicolour(c(quote_string(cicolour_list$arg),
                                                         column_name(cicolour_list$aes)),
                                                       panel.names))

    if (missing(ciunder)) {
      ciunder <- c(TRUE, FALSE)
    }

    if (length(ciunder) > 1) {
      ciunder <- glue::glue('dplyr::if_else(narrowci, ',
                            '{ciunder[length(ciunder)]}, ',
                            '{ciunder[1]})')
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
  horizontal_spacing <- get_horizontal_spacing(
    right.space,
    right.pos,
    left.space,
    left.pos,
    col.right,
    panels_list,
    digits,
    ci.delim,
    estcolumn,
    right.heading,
    right.hjust,
    base_size,
    col.left,
    left.heading,
    left.hjust,
    left.gap,
    right.gap)
  text_about_auto_spacing <- horizontal_spacing$text_about_auto_spacing
  right.pos <- horizontal_spacing$right.pos
  left.pos <- horizontal_spacing$left.pos
  right.space <- horizontal_spacing$right.space
  left.space <- horizontal_spacing$left.space
  right.space.inner <- horizontal_spacing$right.space.inner
  left.space.inner <- horizontal_spacing$left.space.inner

  ## vertical space for panel headings
  space_for_panel_headings <- 1 * text_size
  if (!all(panel.headings == "")){
    space_for_panel_headings <- 4 * text_size
  }



  # Calculate xfrom, xto, xmid, xticks ----
  ## xfrom, xto, etc. are used by other code sections, so this must come first
  if (is.null(col.lci)) {
    allvalues <- unlist(lapply(panels_list, function(x) c(tf(x[[col.estimate]] - 1.96 * x[[col.stderr]]),
                                                          tf(x[[col.estimate]] + 1.96 * x[[col.stderr]]))),
                        use.names = FALSE)
  } else {
    allvalues <- unlist(lapply(panels_list, function(x) c(tf(x[[col.lci]]),
                                                          tf(x[[col.uci]]))),
                        use.names = FALSE)
  }
  allvalues_range <- range(pretty(allvalues))
  ## check for zero as axis limit when using log scale
  if (logscale & isTRUE(all.equal(0, allvalues_range[[1]]))){
    allvalues_range[[1]] <- min(allvalues, na.rm = TRUE)
  }

  ## set xlim, and create xfrom and xto
  if (is.null(xlim)){
    xlim <- allvalues_range
  }
  xfrom <- min(xlim)
  xto   <- max(xlim)

  ## check xfrom and xto for zero and logscale
  if (logscale & (isTRUE(all.equal(0, xfrom)) | isTRUE(all.equal(0, xfrom)))){
    rlang::abort("Axis limit cannot be zero if plotting on a log scale.")
  }

  xmid  <- round(axis_scale_inverse_fn((axis_scale_fn(xfrom) + axis_scale_fn(xto)) / 2), 6)
  if (is.null(xticks)) { xticks <- pretty(c(xfrom, xto)) }

  values_outside_xlim <- min(allvalues_range) < xfrom | max(allvalues_range) > xto


  # Code for preparing data for plotting using forest_data() ----
  prep.data.code <- make_layer(
    name = '# Prepare data to be plotted using ckbplotr::forest_data()',
    plus = FALSE,
    f = 'datatoplot <- ckbplotr::forest_data',
    arg = c(
      'panels = {deparse1(substitute(panels), collapse = "")}',
      if (!is.null(row.labels)){'row.labels = {deparse1(substitute(row.labels), collapse = "")}'},
      argset(row.labels.levels),
      argset(rows),
      argset(row.labels.space),
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
      argset(scalepoints),
      argset(minse),
      argset(bold.labels),
      argset(diamond),
      argset(col.diamond),
      argset(addtext)))


  # Include automatic est column ----
  col.right.all <- c(if (estcolumn){"auto_estcolumn"}, col.right)


  # Plot margin and space between panels
  mid.space <- substitute(mid.space)
  plot.margin <- substitute(plot.margin)

  # Deparse add objects ----
  if (!is.null(add$start)){
    add$start <- deparse1(substitute(add)$start)
  }
  if (!is.null(add$end)){
    add$end <- deparse1(substitute(add)$end)
  }

  # Create plot specification list ----
  spec <- tibble::lst(
    add,
    addaes,
    addarg,
    addtext,
    axis_scale,
    axis_scale_fn,
    axis_scale_inverse_fn,
    base_line_size,
    base_size,
    bold.labels,
    bottom.space,
    ci_order,
    ci.delim,
    cicolour_list,
    ciunder,
    col.bold,
    col.diamond,
    col.estimate,
    heading.rule,
    heading.space,
    col.keep,
    col.key,
    col.lci,
    col.left,
    left.heading,
    left.hjust,
    left.parse,
    left.pos,
    col.right,
    col.right.all,
    right.heading,
    right.hjust,
    right.parse,
    right.pos,
    col.stderr,
    col.uci,
    colour_list,
    data.function,
    diamond,
    diamonds.linewidth,
    digits,
    estcolumn,
    exponentiate,
    fill_list,
    fixed_panel_height,
    fixed_panel_width,
    left.space,
    left.space.inner,
    mid.space,
    minse,
    nullval,
    panel.headings,
    panel.headings.align,
    panel.height,
    panel.names,
    panel.width,
    panels,
    plot.margin,
    plotcolour,
    pointsize,
    right.space,
    right.space.inner,
    row.labels,
    row.labels.heading,
    row.labels.levels,
    row.labels.space,
    rows,
    scalepoints,
    shape_list,
    space_for_panel_headings,
    stroke,
    text_size,
    title,
    values_outside_xlim,
    xfrom,
    xlab,
    xmid,
    xticks,
    xto
  )

  # Create the plot code ----
  plotcode <- c(
    'library(ggplot2)',
    '',
    prep.data.code,               # prepare data for plotting using forest_data()
    forest.narrowci(spec),        # identify narrows CIs when using panel.width
    function.data.function(spec), # user function on datatoplot
    forest.start.ggplot(),        # initiate the ggplot
    indent(2,
           forest.add.start(spec),                      # add$start
           forest.facet(),                              # facets
           forest.nullline(spec),                       # line at null
           forest.cis(spec, type = spec$ci_order[[1]]), # CI lines plotted before points
           forest.plot.points(spec),                    # points
           forest.cis(spec, type = spec$ci_order[[2]]), # CI lines plotted after points
           forest.arrows(spec),                         # arrows for CIs
           forest.plotdiamondscode(spec),               # diamonds
           forest.scales(spec),                         # scales and coordinates
           forest.columns.right(spec),                  # columns to right of panel
           forest.columns.left(spec),                   # columns to left of panel
           forest.addtext(spec),                        # addtext
           forest.column.headings.rule(spec),
           forest.xlab.panel.headings(spec),            # x-axis labels and panel headings
           forest.axes(spec),                           # axes
           forest.panel.size(spec),                     # panel size
           forest.title(spec),                          # plot title
           forest.theme(spec),                          # theme
           forest.add.end(spec)                         # add$end
    )
  )

  # Show code in RStudio viewer ----
  if (showcode){ displaycode(plotcode, text_about_auto_spacing) }


  # Create plot and print ----
  plot <- eval(parse(text = plotcode), envir = envir)
  if (printplot){ print(plot) }

  # Return invisible ----
  return(invisible(list(plot = plot,
                        code = plotcode)))
}


#' Get widths of text strings
#'
#' @keywords internal
#' @noRd
gettextwidths <- function(x){
  purrr::map_dbl(x, \(i) max(purrr::map_dbl(i, \(j) grid::convertWidth(unit(1, "strwidth", data = as.character(j)),
                                                                       "mm",
                                                                       valueOnly = T))))
}


#' Horizontal spacing for text columns in forest_plot()
#'
#' @keywords internal
#' @noRd
get_horizontal_spacing <- function(right.space,
                                   right.pos,
                                   left.space,
                                   left.pos,
                                   col.right,
                                   panels_list,
                                   digits,
                                   ci.delim,
                                   estcolumn,
                                   right.heading,
                                   right.hjust,
                                   base_size,
                                   col.left,
                                   left.heading,
                                   left.hjust,
                                   left.gap,
                                   right.gap) {
  if((is.null(right.space) & !is.null(right.pos)) |
     is.null(left.space) & !is.null(left.pos) ){
    message("Note: Automatic spacing does not account for specified left.pos and right.pos. Use left.space and right.space to set spacing manually.")
  }

  text_size_scaling <- 0.8 * base_size/grid::get.gpar()$fontsize

  ## calculate automatic right.pos and right.space
  text_about_auto_spacing <- NULL
  if (is.null(right.space) | is.null(right.pos) | is.null(left.space) | is.null(left.pos)){
    text_about_auto_spacing <- "Automatically calculated horizontal spacing and positioning:\n"
  }
  ### get maximum width of each columns (incl. heading)
  widths_of_columns <- gettextwidths(lapply(col.right, function(y) unlist(lapply(panels_list, function(x) x[[y]]), use.names = FALSE)))
  estcolumn_width <- gettextwidths(paste0("9.",
                                          paste0(rep(9, digits), collapse = ""),
                                          "(9.",
                                          paste0(rep(9, digits), collapse = ""),
                                          ci.delim,
                                          "99.",
                                          paste0(rep(9, digits), collapse = ""),
                                          ")"))
  widths_of_columns <- c(if(estcolumn){estcolumn_width}, widths_of_columns)
  widths_of_column_headings <- gettextwidths(right.heading)
  widths_of_columns <- pmax(widths_of_columns, widths_of_column_headings)
  ### initial gap, then space for autoestcolumn, and gap between each column
  column_spacing <- cumsum(c(gettextwidths(right.gap[[1]]),
                             widths_of_columns[-length(widths_of_columns)] + gettextwidths(right.gap[[2]]),
                             widths_of_columns[length(widths_of_columns)]))
  ### if no column to plot (i.e. length 1) then zero, if longer don't need extra space on last element
  if (length(widths_of_columns) == 0){column_spacing <- 0}
  ## adjust for hjust
  column_spacing <- column_spacing + c(widths_of_columns*right.hjust, 0)
  column_spacing_half_outer_gap <- column_spacing
  column_spacing_half_outer_gap[length(column_spacing_half_outer_gap)] <- column_spacing_half_outer_gap[length(column_spacing_half_outer_gap)] + 0.5 * gettextwidths(right.gap[[2]])
  ### text on plot is 0.8 size, and adjust for base_size
  column_spacing <-  round(text_size_scaling * column_spacing, 1)
  column_spacing_half_outer_gap <-  round(text_size_scaling * column_spacing_half_outer_gap, 1)
  if (is.null(right.space)){
    right.space.inner <- unit(column_spacing_half_outer_gap[length(column_spacing_half_outer_gap)], "mm")
    right.space <- unit(column_spacing[length(column_spacing)], "mm")
    text_about_auto_spacing <- c(text_about_auto_spacing, glue::glue("`right.space   = {printunit(right.space)}`<br>"))
  } else {
    right.space.inner <- right.space - unit(0.5 * 0.8 * base_size/grid::get.gpar()$fontsize * gettextwidths(right.gap[[2]]), "mm")
  }
  if (length(column_spacing) > 1){column_spacing <- column_spacing[-length(column_spacing)]}
  if (is.null(right.pos)){
    right.pos <- unit(column_spacing, "mm")
    text_about_auto_spacing <- c(text_about_auto_spacing, glue::glue("`right.pos = {printunit(right.pos)}`<br>"))
  }

  ## calculate automatic left.pos and left.space
  ### get maximum width of each columns (incl. heading)
  widths_of_columns <- gettextwidths(lapply(col.left, function(y) unlist(lapply(panels_list, function(x) x[[y]]), use.names = FALSE)))
  widths_of_column_headings <- gettextwidths(left.heading)
  widths_of_columns <- pmax(widths_of_columns, widths_of_column_headings)
  ### initial gap, and gap between each column
  column_spacing <- cumsum(c(gettextwidths(left.gap[[1]]),
                             widths_of_columns + gettextwidths(left.gap[[2]])))
  ### but if no column to plot (i.e. length 1) then just left.gap[[2]]
  if (length(widths_of_columns) == 0){column_spacing <- gettextwidths(left.gap[[2]])}
  ## adjust for hjust
  column_spacing <- column_spacing + c(widths_of_columns*(1 - left.hjust), 0)
  column_spacing_half_outer_gap <- column_spacing
  column_spacing_half_outer_gap[length(column_spacing_half_outer_gap)] <- column_spacing_half_outer_gap[length(column_spacing_half_outer_gap)] - 0.5 * gettextwidths(left.gap[[2]])
  ### text on plot is 0.8 size, and adjust for base_size
  column_spacing <- round(text_size_scaling * column_spacing, 1)
  column_spacing_half_outer_gap <- round(text_size_scaling * column_spacing_half_outer_gap, 1)
  if (is.null(left.space)){
    left.space.inner <- unit(column_spacing_half_outer_gap[length(column_spacing_half_outer_gap)], "mm")
    left.space <- unit(column_spacing[length(column_spacing)], "mm")
    text_about_auto_spacing <- c(text_about_auto_spacing, glue::glue("`left.space    = {printunit(left.space)}`<br>"))
  } else {
    left.space.inner <- left.space - unit(0.5 * 0.8 * base_size/grid::get.gpar()$fontsize * gettextwidths(left.gap[[2]]), "mm")
  }
  if (length(column_spacing) > 1){column_spacing <- column_spacing[-length(column_spacing)]}
  if (is.null(left.pos)){
    left.pos <- unit(column_spacing, "mm")
    text_about_auto_spacing <- c(text_about_auto_spacing, glue::glue("`left.pos  = {printunit(left.pos)}`<br>"))
  }

  return(list(text_about_auto_spacing = text_about_auto_spacing,
              right.pos = right.pos,
              left.pos = left.pos,
              right.space = right.space,
              right.space.inner = right.space.inner,
              left.space = left.space,
              left.space.inner = left.space.inner))
}
