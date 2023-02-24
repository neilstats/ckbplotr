
#' Prepares data set for a forest plot
#'
#'
#' @param panels A list of data frames. These should include columns or point
#'   estimates, and standard errors or confidence interval limits. If you
#'   specify a row.labels data frame, then they must also all contain a key column
#'   with the same name (which can be specified by col.key).
#' @param col.key Name of column that links the results given in each data frame
#'   provided in panels and the labels given in row.labels.
#'
#'   If row.labels data frame is not given, then this column will be used as row labels.
#'
#'   (Default: "key")
#' @param row.labels A data frame that contains the labels to be used for the
#'   rows of the plot. Use NA if a lower level heading is not required for a given row.
#' @param row.labels.levels A character vector up to length 3. The names of columns in row.labels
#'   to use as headings/subheadings/labels for labelling rows. (Default: c("heading1", "heading2", "heading3"))
#' @param rows A character vector. The top level labels of rows
#'   to be included in the plot.
#' @param panel.names A character vector. The names to be used for each forest plot panel.
#'   If none provided, then they will be numbered 1, 2, 3 ...
#' @param col.estimate Name of column that provides point estimates.
#'   (Default: "estimate")
#' @param col.stderr Name of column that provides standard errors. (Default: "stderr")
#' @param col.lci Name of column that provides lower limit of confidence intervals.
#' @param col.uci Name of column that provides upper limit of confidence intervals.
#' @param col.left Names of columns to be printed to the left of the plot.
#' @param col.right Names of columns to be printed to the right of the plot.
#' @param col.keep Names of additional columns to be kept in returned data frame.
#' @param ci.delim Character string to separate lower and upper limits of
#'   confidence interval. (Default: ", ")
#' @param digits Number of digits after decimal point to show for estimates and confidence intervals. (Default: 2)
#' @param exponentiate Exponentiate estimates (and CIs) before plotting. (Default: TRUE)
#' @param blankrows A numeric vector of length 4 specifying the number of blank rows
#'   after a heading1, at the end of a heading1 'section', after
#'   a heading2, and at the end of a heading2 'section. (Default: c(1, 1, 0, 0))
#' @param scalepoints Should the points be scaled by inverse of the standard
#'   error? (Default: FALSE)
#' @param minse Minimum standard error to use when scaling point size. (Default will use minimum in the data.)
#' @param addtext A list of data frames. List must be the same length as panels.
#'   Data frames should contain a column with the name specified in col.key,
#'   and one or more of:
#'
#'   1. a column named 'text' containing character strings
#'
#'   2. columns named 'het_dof', 'het_stat', and 'het_p' containing character strings
#'
#'   3. columns names 'trend_stat' and 'trend_p' containing character strings
#'
#'   The character strings, heterogeneity test, and trend test results will
#'   be plotted in the column of estimates and CIs, below the row with the key
#'   given in the col.key column.
#' @param cols DEPRECATED.
#' @param headings DEPRECATED.
#' @param colnames DEPRECATED.
#'
#' @return A dataset from which a forest plot can be generated.
#'
#'
#' @keywords internal
#'
#' @importFrom rlang .data
#' @importFrom utils compareVersion
#' @importFrom utils packageVersion
#'
#' @export

forest_data <- function(
    panels,
    col.key       = "key",
    row.labels    = NULL,
    row.labels.levels = c("heading1", "heading2", "heading3"),
    rows          = NULL,
    panel.names   = NULL,
    col.estimate  = "estimate",
    col.stderr    = "stderr",
    col.lci       = NULL,
    col.uci       = NULL,
    col.left      = NULL,
    col.right     = NULL,
    col.keep      = NULL,
    ci.delim      = ", ",
    digits        = 2,
    exponentiate  = TRUE,
    blankrows     = c(1, 1, 0, 0),
    scalepoints   = FALSE,
    minse         = NULL,
    addtext       = NULL,
    cols          = panels,
    headings      = NULL,
    colnames      = NULL,
    bold.labels   = NULL
){

  # legacy arguments
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

  # check arguments
  if (!is.null(col.lci) &&  is.null(col.uci)) stop("col.lci and col.uci must both be specified")
  if ( is.null(col.lci) && !is.null(col.uci)) stop("col.lci and col.uci must both be specified")
  if (is.null(panel.names)) { panel.names <- as.character(1:length(panels)) }
  if (!is.character(panel.names)) stop("panel.names must be a character vector")
  if (!all(!duplicated(panel.names))) stop("panel.names must be unique")
  if (length(panels) != length(panel.names)) stop("panels and panel.names must be the same length")
  if (!(length(blankrows == 4) & is.numeric(blankrows))) stop("blankrows must be a length 4 vector")
  if(length(row.labels.levels) > 3) stop("row.labels.levels has maximum length of 3")
  if(!is.null(row.labels) & !all(row.labels.levels %in% names(row.labels))) stop("row.labels.levels must be columns in row.labels")
  if(!is.null(row.labels) & !all(sapply(row.labels[row.labels.levels], is.character))) stop("row.labels.levels columns must be character")

  # Make vector of keys after which extra rows are added for addtext
  addtextcols <- tibble::tibble(text = character(),
                                het_dof = character(),
                                het_stat = character(),
                                het_p = character(),
                                trend_stat = character(),
                                trend_p = character())
  extrarowkeys <- c()
  if (!is.null(addtext)) {
    for (i in 1:length(addtext)) {
      addtext[[i]] <- dplyr::bind_rows(addtextcols, addtext[[i]]) %>%
        dplyr::mutate(addtext = dplyr::case_when(
          !is.na(text) ~ paste0("'", text, "'"),
          !is.na(het_stat) ~ paste0("paste('Heterogeneity: ', chi[",
                                    het_dof,
                                    "]^2,'=",
                                    het_stat,
                                    " (p",
                                    het_p,
                                    ")', sep='')"),
          !is.na(trend_stat) ~ paste0("paste('Trend: ', chi[1]^2,'=",
                                      trend_stat,
                                      " (p",
                                      trend_p,
                                      ")', sep='')")
        )) %>%
        dplyr::select(key = !!rlang::sym(col.key),
                      .data$addtext) %>%
        dplyr::mutate(key = as.character(.data$key)) %>%
        dplyr::group_by(.data$key) %>%
        dplyr::mutate(addtextrow = 1:dplyr::n() - 1) %>%
        dplyr::ungroup()
    }
    extrarowkeys <- purrr::reduce(purrr::map(addtext,
                                             ~ dplyr::count(., .data$key)),
                                  dplyr::bind_rows) %>%
      dplyr::group_by(.data$key) %>%
      dplyr::summarise(n = max(.data$n))
    extrarowkeys <- rep(extrarowkeys$key, extrarowkeys$n)
  }

  # create data frame of row numbers and labels
  if (is.null(row.labels)) {
    out <- panels[[1]] %>%
      dplyr::mutate(row.label = !!rlang::sym(col.key),
                    key = !!rlang::sym(col.key),
                    row.height = NA,
                    spacing_row = FALSE) %>%
      dplyr::select(.data$row.label, .data$key, .data$row.height, .data$spacing_row)
  } else {

    if (is.null(rows)) stop("argument rows must be given if row.labels is used")
    if (!col.key %in% names(row.labels)) stop(paste0(col.key, " must be a column in ",  deparse(substitute(row.labels))))

    for (panel in panels) {
      if (!col.key %in% names(panel)) stop(paste0(col.key, " must be a column in every data frame given in panels"))
    }

    for (head1 in rows) {
      if (!(head1 %in% row.labels[[ row.labels.levels[[1]] ]])) {
        stop(paste(head1,"is not in",  row.labels.levels[[1]], "column of", deparse(substitute(row.labels))))
      }
    }

    ## make row.labels.levels length 3
    row.labels.levels <- c(row.labels.levels, rep("NAcol", max(0, 3-length(row.labels.levels))))

    ## create key and heading* columns
    row.labels <- dplyr::mutate(row.labels,
                                key = !!rlang::sym(col.key),
                                NAcol = NA_character_) %>%
      dplyr::mutate(heading2 = dplyr::if_else(is.na(!!rlang::sym(row.labels.levels[[2]])) & !is.na(!!rlang::sym(row.labels.levels[[3]])), !!rlang::sym(row.labels.levels[[3]]), !!rlang::sym(row.labels.levels[[2]])),
                    heading3 = dplyr::if_else(is.na(!!rlang::sym(row.labels.levels[[2]])) & !is.na(!!rlang::sym(row.labels.levels[[3]])), NA_character_, !!rlang::sym(row.labels.levels[[3]]))) %>%
      dplyr::mutate(heading1 = dplyr::if_else(is.na(!!rlang::sym(row.labels.levels[[1]])) & !is.na(.data$heading2), .data$heading2, !!rlang::sym(row.labels.levels[[1]])),
                    heading2 = dplyr::if_else(is.na(!!rlang::sym(row.labels.levels[[1]])) & !is.na(.data$heading2), NA_character_, .data$heading2))

    ## keep only rows where heading1 is in rows
    row.labels <- dplyr::left_join(tibble::tibble(heading1 = rows),
                                   row.labels,
                                   by = "heading1")

    ## function to add headings/subheadings for row labels
    add_heading <- function(data, heading, blank_after_heading, blank_after_section){
      if(all(is.na(data$row.label))){
        out <- dplyr::mutate(data, row.label = !!heading)
      } else {
        out <- dplyr::add_row(data,
                              row.label = !!heading,
                              spacing_row = FALSE,
                              .before = 1)
        if (blank_after_heading > 0){
            out <- tibble::add_row(out,
                                   row.label = "",
                                   row.height = blank_after_heading,
                                   spacing_row = TRUE,
                                   .before = 2)
        }
      }
      if (blank_after_section > 0){
          out <- tibble::add_row(out,
                                 row.label = "",
                                 row.height = blank_after_section,
                                 spacing_row = TRUE)
      }
      out
    }

    ## add headings/subheadings for row labels
    out <- row.labels %>%
      dplyr::mutate(row.label = .data$heading3,
                    row.height = NA,
                    spacing_row = FALSE) %>%
      dplyr::group_by(.data$heading1, .data$heading2) %>%
      tidyr::nest() %>%
      dplyr::mutate(res = purrr::map(.data$data,
                                     ~ add_heading(.,
                                                   .data$heading2,
                                                   blankrows[[3]],
                                                   blankrows[[4]]))) %>%
      dplyr::select(-.data$data) %>%
      tidyr::unnest(cols = "res") %>%
      dplyr::group_by(.data$heading1) %>%
      tidyr::nest() %>%
      dplyr::mutate(res = purrr::map(.data$data,
                                     ~ add_heading(.,
                                                   .data$heading1,
                                                   blankrows[[1]],
                                                   blankrows[[2]]))) %>%
      dplyr::select(-.data$data) %>%
      tidyr::unnest(cols = "res") %>%
      dplyr::ungroup()
  }

  # Add extra rows for addtext
  out <- dplyr::mutate(out, extrarowkey = NA_character_)
  if (!is.null(addtext)) {
    for (k in 1:length(extrarowkeys)) {
      out <- out %>%
        dplyr::add_row(row.label = "",
                       extrarowkey = paste0(extrarowkeys[[k]]),
                       spacing_row = FALSE,
                       .after = which(out$key == extrarowkeys[[k]]))
    }
  }

  out <- out %>%
    dplyr::group_by(.data$extrarowkey) %>%
    dplyr::mutate(addtextrow = 1:dplyr::n() - 1) %>%
    dplyr::ungroup()

  # remove any blank rows  bottom if needed
  while (utils::tail(out$row.label, 1) == "" & is.na(utils::tail(out$extrarowkey, 1))) {
    out <- dplyr::slice(out, 1:(dplyr::n() - 1))
  }

  out <- out %>%
    dplyr::mutate(row = cumsum(dplyr::coalesce(.data$row.height, 1))) %>%
    dplyr::filter(!.data$spacing_row) %>%
    dplyr::select(.data$row, .data$row.label, .data$key, .data$extrarowkey, .data$addtextrow)

  # make datatoplot
  datatoplot <- tibble::tibble()

  for (i in 1:length(panels)) {
    if (!is.null(col.lci)) {
      panels[[i]] <- panels[[i]] %>%
        dplyr::select(key = !!rlang::sym(col.key),
                      !!!rlang::syms(col.left),
                      estimate = !!rlang::sym(col.estimate),
                      lci      = !!rlang::sym(col.lci),
                      uci      = !!rlang::sym(col.uci),
                      !!!rlang::syms(col.right),
                      !!!rlang::syms(col.keep))
    } else {
      panels[[i]] <- panels[[i]] %>%
        dplyr::select(key = !!rlang::sym(col.key),
                      !!!rlang::syms(col.left),
                      estimate = !!rlang::sym(col.estimate),
                      stderr   = !!rlang::sym(col.stderr),
                      !!!rlang::syms(col.right),
                      !!!rlang::syms(col.keep))
    }

    out1 <- merge(out, panels[[i]], by = "key", all.x = TRUE) %>%
      dplyr::mutate(panel = panel.names[[i]])

    if (!is.null(addtext)){
      out1 <- merge(out1, addtext[[i]],
                    by.x = c("extrarowkey", "addtextrow"),
                    by.y = c("key", "addtextrow"),
                    all.x = TRUE)
    } else {
      out1 <- dplyr::mutate(out1, addtext = as.character(NA))
    }

    datatoplot <- dplyr::bind_rows(datatoplot, out1)
  }


  if (exponentiate == TRUE) {
    tf       <- exp
    inv_tf   <- log
  } else {
    tf       <- identity
    inv_tf   <- identity
  }

  # Make 'panel' a factor, so that facet panels will be in the correct order
  datatoplot <- datatoplot %>%
    dplyr::mutate(panel = factor(panel,
                                 levels = panel.names,
                                 labels = panel.names,
                                 ordered = TRUE))


  # Adding CIs and text to show estimate and CI
  if (!is.null(col.lci)) {
    datatoplot <- datatoplot %>%
      dplyr::mutate(estimate_transformed = tf(.data$estimate),
                    lci_transformed = tf(.data$lci),
                    uci_transformed = tf(.data$uci)
      )
    if (is.null(minse)){
      minse <- min((datatoplot$estimate - datatoplot$lci)/1.96, na.rm = TRUE)
    } else {
      if (minse > min((datatoplot$estimate - datatoplot$lci)/1.96, na.rm = TRUE)) stop("minse is larger than the minimum standard error in the data")
    }
    datatoplot$size <- 1.96*minse/(datatoplot$estimate - datatoplot$lci)
  } else {
    datatoplot <- datatoplot %>%
      dplyr::mutate(estimate_transformed = tf(.data$estimate),
                    lci_transformed = tf(.data$estimate - 1.96*.data$stderr),
                    uci_transformed = tf(.data$estimate + 1.96*.data$stderr)
      )
    if (is.null(minse)){
      minse <- min(datatoplot$stderr, na.rm = TRUE)
    } else {
      if (minse > min(datatoplot$stderr, na.rm = TRUE)) stop("minse is larger than the minimum standard error in the data")
    }
    datatoplot$size <- minse/datatoplot$stderr
  }

  datatoplot <- datatoplot %>%
    dplyr::mutate(auto_estcolumn = dplyr::case_when(
      !is.na(estimate) ~ paste0(format(round(estimate_transformed, digits), nsmall = digits, trim = T),
                                " (",
                                format(round(lci_transformed, digits), nsmall = digits, trim = T),
                                ci.delim,
                                format(round(uci_transformed, digits), nsmall = digits, trim = T),
                                ")"))) %>%
    dplyr::select(-.data$extrarowkey, -.data$addtextrow) %>%
    dplyr::arrange(panel, row)


  if (!scalepoints) {
    datatoplot$size <- 1
  }

  rowlabels <- datatoplot %>%
    dplyr::group_by(.data$row) %>%
    dplyr::summarise(row.label = dplyr::first(.data$row.label),
                     bold = all(is.na(.data$estimate_transformed) | all(.data$key %in% bold.labels)),
                     .groups = "drop") %>%
    dplyr::mutate(row.label = dplyr::if_else(.data$bold & .data$row.label != "",
                                             paste0("**", .data$row.label, "**"),
                                             as.character(.data$row.label))) %>%
    dplyr::arrange(.data$row) %>%
    dplyr::select(.data$row, .data$row.label)

  attr(datatoplot, "rowlabels") <- rowlabels

  return(datatoplot)
}


#' @describeIn forest_data Synonym for `forest_data()`
#' @export
make_forest_data <- forest_data







































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
#' @param stroke Size of outline of shapes. (Default: 0)
#' @param quiet Set to TRUE to not print the plot nor show generated code in the RStudio 'Viewer' pane. (Default: FALSE)
#' @param printplot Print the plot. (Default: !quiet)
#' @param showcode Show the ggplot2 code to generate the plot in RStudio 'Viewer' pane. (Default: !quiet)
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
    base_size     = 11,
    base_line_size = base_size/22,
    stroke        = 0,
    quiet         = FALSE,
    printplot     = !quiet,
    showcode      = !quiet,
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
  if (!missing(col.diamond) &&  !missing(diamond)) stop("Use either col.diamond or diamond, not both.")

  for (c in c(col.left, col.right)){
    if (any(unlist(lapply(panels, function(x) !c %in% names(x))))){
      stop("Column '", c, "' does not exist in every panels data frame.")
    }
  }

  ## check if cicolour is a list (or longer than 1) but not using panel.width
  if ((is.list(cicolour) | length(cicolour) > 1) & missing(panel.width)){
    stop("cicolour should be a list (or longer than 1) only when using panel.width")
  }

  ## check if confidence intervals may be hidden
  if (missing(panel.width)){
    rlang::inform(c('i' = 'Narrow confidence interval lines may become hidden in the forest plot.',
                    'i' = 'Please check your final output carefully and see vignette("forest_confidence_intervals") for more details.'),
                  use_cli_format = TRUE,
                  .frequency = "once",
                  .frequency_id = "forest_narrow_cis")
  }

  # Check for log scale ----
  if (logscale == TRUE) {
    tf       <- exp
    inv_tf   <- log
    scale    <- "log"
    if (missing(nullval)) {nullval <- 1}
  } else {
    tf       <- identity
    inv_tf   <- identity
    scale    <- "identity"
  }



  # Transpose column headings if a list ----
  if (purrr::is_list(col.right.heading)){ col.right.heading <- purrr::transpose(col.right.heading)}
  if (purrr::is_list(col.left.heading)){ col.left.heading <- purrr::transpose(col.left.heading)}



  # Identify columns to keep in data frame ----
  col.keep <- c(col.keep, col.diamond, col.bold)
  for (x in c(shape, unlist(cicolour), colour, unlist(fill), ciunder)){
    if (x %in% names(panels[[1]])){ col.keep <- append(col.keep, x) }
  }



  # Take first element if diamond is a list ----
  if (is.list(diamond)){ diamond <- diamond[[1]] }


  # Default panel.names ----
  if (is.null(panel.names)) { panel.names <- as.character(1:length(panels)) }




  # Handling aesthetics ----
  ## match column name, or use argument itself

  ### shape
  if (!missing(shape) && shape %in% names(panels[[1]])){
    shape <- list(aes = shape)
  } else {
    shape <- list(arg = shape)
  }

  ### cicolour
  if(is.list(cicolour)){
    cicolour <- list(arg = cicolour)
  } else if (all(cicolour %in% names(panels[[1]]))){
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
  if (!missing(colour) && all(colour %in% names(panels[[1]]))){
    colour <- list(aes = colour)
  } else {
    colour <- list(arg = colour)
  }

  ### fill
  if (!is.list(fill) && fill %in% names(panels[[1]])){
    fill <- list(aes = fill)
  } else {
    fill <- list(arg = fill)
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
    text_auto_spacing <- "Automatically calculated horizontal spacing and positioning:\n"
  }
  ### get maximum width of each columns (incl. heading)
  colspaces <- gettextwidths(lapply(col.right, function(y) c(sapply(panels, function(x) x[[y]]))))
  estcolumnwidth <- gettextwidths(paste0("9.",
                                         paste0(rep(9, digits), collapse = ""),
                                         "(9.",
                                         paste0(rep(9, digits), collapse = ""),
                                         ci.delim,
                                         "99.",
                                         paste0(rep(9, digits), collapse = ""),
                                         ")"))
  colspaces <- c(if(estcolumn){estcolumnwidth}, colspaces)
  headspaces <- gettextwidths(col.right.heading)
  colspaces <- pmax(colspaces, headspaces)
  ### initial gap, then space for autoestcolumn, and gap between each column
  colspaceauto <- cumsum(c(gettextwidths("I"),
                           colspaces + gettextwidths("W")))
  ## adjust for hjust
  colspaceauto <- colspaceauto + c(colspaces*col.right.hjust, 0)
  ### if no column to plot (i.e. length 1) then zero, if longer don't need extra space on last element
  if (length(colspaceauto) == 1){colspaceauto <- 0}
  if (length(colspaceauto) > 1){colspaceauto[length(colspaceauto)] <- colspaceauto[length(colspaceauto)] - gettextwidths("W")}
  ### text on plot is 0.8 size, and adjust for base_size
  colspaceauto <-  round(0.8 * base_size/grid::get.gpar()$fontsize * colspaceauto, 1)
  if (is.null(right.space)){
    right.space <- unit(colspaceauto[length(colspaceauto)], "mm")
    text_auto_spacing <- c(text_auto_spacing, paste0("- right.space   = ", printunit(right.space)))
  }
  if (length(colspaceauto) > 1){colspaceauto <- colspaceauto[-length(colspaceauto)]}
  if (is.null(col.right.pos)){
    col.right.pos <- unit(colspaceauto, "mm")
    text_auto_spacing <- c(text_auto_spacing, paste0("- col.right.pos = ", printunit(col.right.pos)))
  }

  ## calculate automatic col.left.pos and col.left.space
  ### get maximum width of each columns (incl. heading)
  colspaces <- gettextwidths(lapply(col.left, function(y) c(sapply(panels, function(x) x[[y]]))))
  headspaces <- gettextwidths(col.left.heading)
  colspaces <- pmax(colspaces, headspaces)
  ### initial gap, and gap between each column
  colspaceauto <- cumsum(c(gettextwidths("I"),
                           colspaces + gettextwidths("W")))
  ## adjust for hjust
  colspaceauto <- colspaceauto + c(colspaces*(1 - col.left.hjust), 0)
  ### if no column to plot (i.e. length 1) then width of W, if longer keep extra space on last element
  if (length(colspaceauto) == 1){colspaceauto <- gettextwidths("W")}
  # if (length(colspaceauto) > 1){colspaceauto[length(colspaceauto)] <- colspaceauto[length(colspaceauto)] - gettextwidths("W")}
  ### text on plot is 0.8 size, and adjust for base_size
  colspaceauto <-  round(0.8 * base_size/grid::get.gpar()$fontsize * colspaceauto, 1)
  if (is.null(left.space)){
    left.space <- unit(colspaceauto[length(colspaceauto)], "mm")
    text_auto_spacing <- c(text_auto_spacing, paste0("- left.space    = ", printunit(left.space)))
  }
  if (length(colspaceauto) > 1){colspaceauto <- colspaceauto[-length(colspaceauto)]}
  if (is.null(col.left.pos)){
    col.left.pos <- unit(colspaceauto, "mm")
    text_auto_spacing <- c(text_auto_spacing, paste0("- col.left.pos  = ", printunit(col.left.pos)))
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
  xmid  <- round(tf((inv_tf(xfrom) + inv_tf(xto)) / 2), 6)
  if (is.null(xticks)) { xticks <- pretty(c(xfrom, xto)) }


  # Aesthetic adjustments when using panel.width ----
  if (!missing(panel.width)) {
    if (!inherits(panel.width, "unit")){
      panel.width <- grid::unit(panel.width, "mm")
    }
    cicolours <- c(quote_string(cicolour$arg), column_name(cicolour$aes))
    cicolour <- list(aes = "cicolour")

    if (missing(ciunder)) {
      ciunder <- c(TRUE, FALSE)
    }
  }

  if (!missing(panel.width) && length(ciunder) > 1) {
    ciunder_orig <- ciunder
    ciunder <- "ciunder"
  }

  if (is.list(fill$arg)){
    fill_orig <- fill$arg
    fill <- list(aes = "fill")
  }


  # Code for preparing data for plotting using forest_data() ----
  prep.data.code <- make_layer(
    name = '# Prepare data to be plotted using ckbplotr::forest_data()',
    plus = FALSE,
    f = 'datatoplot <- ckbplotr::forest_data',
    arg = c(
      if (!identical(row.labels,
                     eval(formals(ckbplotr::forest_data)[["row.labels"]]))){
        sprintf('row.labels = %s',
                if (!missing(headings)) {
                  paste(deparse(substitute(headings)), collapse = '')
                } else {
                  paste(deparse(substitute(row.labels)), collapse = '')
                }
        )
      },
      argset(row.labels.levels),
      argset(rows),
      sprintf('panels = %s',
              if (!missing(cols)) {
                paste(deparse(substitute(cols)), collapse = '')
              } else {
                paste(deparse(substitute(panels)), collapse = '')
              }
      ),
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
        sprintf('addtext = %s',
                paste(deparse(substitute(addtext)), collapse = ''))
      }))


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

    # code to prepare data for plotting using forest_data()
    prep.data.code,

    # fill may be a list
    if (exists("fill_orig")){forest.fillcode(fill_orig, panel.names)},

    # code for preparing data for diamonds
    if(!is.null(col.diamond) || !is.null(diamond)){
      forest.diamondscode(diamond, col.diamond, panel.width, cicolours, panel.names)
    },

    # code for CI colours if using panel.width
    forest.cicolourcode(scale,
                        inv_tf,
                        xto,
                        xfrom,
                        pointsize,
                        stroke,
                        panel.width,
                        shape,
                        cicolours,
                        panel.names),

    ## code for CI under - if using panel.width
    if (exists("ciunder_orig")) {
      forest.ciundercode(ciunder_orig)
    },

    # code to initiate the ggplot
    forest.start.ggplot(),

    indent(2,

           # the code to put panels in facets
           forest.facet(),

           # code for line at null
           if (!is.null(nullval)) { forest.nullline(nullval, base_line_size, plotcolour) },

           # code for CI lines plotted before points
           forest.cis(addaes,
                      cicolour,
                      addarg,
                      ciunder,
                      base_line_size,
                      type = ci_order[[1]]),

           # code to plot points
           forest.plot.points(addaes,
                              shape,
                              colour,
                              fill,
                              addarg,
                              xfrom,
                              xto,
                              stroke,
                              pointsize),

           # code for CI lines plotted after points
           forest.cis(addaes,
                      cicolour,
                      addarg,
                      ciunder,
                      base_line_size,
                      type = ci_order[[2]]),

           # code to add arrows to CIs
           forest.arrows(addaes, cicolour, addarg, base_line_size, xfrom, xto),

           # code for plotting diamonds
           if(!is.null(col.diamond) || !is.null(diamond)){
             forest.plotdiamondscode(cicolour, fill, stroke)
           },

           # code for scales and coordinates
           forest.scales.coords(xfrom, xto),

           # code for columns to right of panel
           if (!is.null(col.right) | estcolumn) {
             col.right.all <- c(if (estcolumn){"auto_estcolumn"}, col.right)
             forest.col.right.line(col.right.all,
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
                                   panel.names,
                                   tf,
                                   inv_tf)
           },

           # code for columns to left of panel
           if (!is.null(col.left)) {
             forest.col.left.line(col.left,
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
                                  panel.names,
                                  tf,
                                  inv_tf)
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
                            tf,
                            inv_tf)
           },

           # code for x-axis labels and panel headings
           forest.xlab.panel.headings(addaes,
                                      xmid,
                                      addarg,
                                      text_size,
                                      plotcolour,
                                      panel.names,
                                      xlab,
                                      panel.headings,
                                      col.heading.space),

           # code for the axes
           forest.axes(scale, xticks, bottom.space),

           # code for panel width
           forest.panel.width(panel.width),

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
  if (showcode){ displaycode(plotcode, text_auto_spacing) }

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


