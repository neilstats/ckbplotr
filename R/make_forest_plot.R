
#' Prepares data set for a forest plot
#'
#' \code{make_forest_data}
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

make_forest_data <- function(
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
  colnames      = NULL
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
  extrarowkeys <- c()
  addtextcols <- tibble::tibble(text = character(),
                                het_dof = character(),
                                het_stat = character(),
                                het_p = character(),
                                trend_stat = character(),
                                trend_p = character())
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
        dplyr::mutate(key = as.character(.data$key))

      extrarowkeys <- c(extrarowkeys, addtext[[i]][["key"]])
    }
  }
  extrarowkeys <- unique(extrarowkeys)

  # create data frame of row numbers and labels
  if (is.null(row.labels)) {
    out <- panels[[1]] %>%
      dplyr::mutate(row.label = !!rlang::sym(col.key),
                    key = !!rlang::sym(col.key)) %>%
      dplyr::select(.data$row.label, .data$key)
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
        out <- dplyr::add_row(data, row.label = !!heading, .before = 1)
        if (blank_after_heading > 0){
          for (i in 1:blank_after_heading) {
            out <- tibble::add_row(out, row.label = "", .before = 2)
          }
        }
      }
      if (blank_after_section > 0){
        for (i in 1:blank_after_section) {
          out <- tibble::add_row(out, row.label = "")
        }
      }
      out
    }

    ## add headings/subheadings for row labels
    out <- row.labels %>%
      dplyr::mutate(row.label = .data$heading3) %>%
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
                       .after = which(out$key == extrarowkeys[[k]]))
    }
  }

  # add a blank heading at bottom if needed
  if (utils::tail(out$row.label, 1) != "") {
    out <- out %>%
      tibble::add_row(row.label = "")
  }

  out <- out %>%
    dplyr::mutate(row = 1:dplyr::n()) %>%
    dplyr::select(.data$row, .data$row.label, .data$key, .data$extrarowkey)

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
      out1 <- merge(out1, addtext[[i]], by.x = "extrarowkey", by.y = "key", all.x = TRUE)
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
    dplyr::select(-.data$extrarowkey) %>%
    dplyr::arrange(panel, row)


  if (!scalepoints) {
    datatoplot$size <- 1
  }

  return(datatoplot)
}









































#' Make a forest plot with ggplot2
#'
#' Creates a forest plot with ggplot
#'
#' The function returns the plot and ggplot2 code to create the plot.
#' In RStudio, the ggplot2 code will be shown in the viewer.
#'
#'
#'
#' @inheritParams make_forest_data
#' @inheritParams theme_ckb
#' @param logscale Use log scale on the axis, and add a line at null effect. (Default: exponentiate)
#' @param panel.headings Titles to be placed above each forest plot. (Default: panel.names)
#' @param estcolumn Include column of estimates and confidence intervals to the
#' right of each plot. (Default: TRUE)
#' @param col.right.parse A logical vector, the same length as col.right (+ 1 if estcolumn = TRUE).
#' Should the contents of the columns be parsed into expressions. (Default: All FALSE, except estcolumn.)
#' @param col.left.pos A unit vector to position col.right columns.
#' @param col.right.pos A unit vector to position col.right columns.
#' @param col.left.hjust A numeric vector. The horizontal justification of
#' col.left columns. (Default: 1)
#' @param col.right.hjust A numeric vector. The horizontal justification of
#' col.right columns. (Default: 0)
#' @param col.left.heading A character vector of titles for col.left columns. (Default: "")
#' @param col.right.heading A character vector of titles for the column of estimates
#' (if estcolumn = TRUE) and col.right columns. (Default: "HR (95\% CI)")
#' @param col.heading.space Position of the titles given by col.left.heading and
#' col.right.heading. Increase to move them up. (Default: 0)
#' @param title Title to appear at the top of the plot.
#' @param xlab Label to appear below the x-axis. (Default: "HR (95\% CI)")
#' @param xlim A numeric vector. The limits of the x axis.
#' @param xticks A numeric vector. The tick points of the x axis.
#' @param nullval Add a vertical reference line at this value. (If logscale == TRUE then by default it will be added at 1, but use NA not to plot this line.)
#' @param pointsize The (largest) size of box to use for plotting point
#'                  estimates. (Default: 3)
#' @param shape Shape of points. An integer, or name of a column of integers. (Default will use shape 22 - squares with fill.)
#' @param colour Colour of points. Name of a colour, or name of a column of colour names. (Default will use black.)
#' @param cicolour Colour of CI lines. Colour of CI lines. Name of a colour, or name of a column of colour names. (Default will use black.)
#' @param fill Fill colour of points. Fill colour of points. Name of a colour, or name of a column of colour names. (Default will use black.)
#' @param ciunder Plot CI lines before points. A logical value, or name of a column of logical values. (Default will plot CI lines after points.)
#' @param col.diamond Plot estimates and CIs as diamonds. Name of a column of logical values.
#' @param diamond Alternative to col.diamond. A character vectors identify the rows
#'                (using the key values) for which the estimate and CI should be plotted using a diamond.
#' @param col.bold Plot text as bold. Name of a column of logical values.
#' @param bold.labels A character vector identifying row labels (using key values) which should additionally be bold. (Default: NULL)
#' @param left.space Size of gap to leave to the left of panels. (Default: 1 + 2*length(col.left))
#' @param right.space Size of gap to leave to the right of panels. (Default: 5 + 2*length(col.right))
#' @param mid.space Size of additional gap to leave between panels. (Default: unit(5, "mm"))
#' @param plot.margin Plot margin, given as margin(top, right, bottom, left, units). (Default: margin(8, 8, 8, 8, "mm"))

#' @param panel.width Panel width to assume and apply different formatting to narrow CIs. Unit is "mm".
#' @param stroke Size of outline of shapes. (Default: 0)
#' @param printplot Print the plot. (Default: TRUE)
#' @param showcode Show the ggplot2 code to generate the plot in RStudio 'Viewer' pane. (Default: TRUE)
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




make_forest_plot <- function(
  panels,
  row.labels    = NULL,
  row.labels.levels = c("heading1", "heading2", "heading3"),
  rows          = NULL,
  exponentiate  = TRUE,
  logscale      = exponentiate,
  panel.names   = NULL,
  panel.headings = panel.names,
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
  shape     = NULL,
  colour    = NULL,
  cicolour  = colour,
  fill      = NULL,
  ciunder   = NULL,
  addtext       = NULL,
  left.space    = NULL,
  right.space   = NULL,
  mid.space     = unit(5, "mm"),
  plot.margin   = margin(8, 8, 8, 8, "mm"),
  panel.width   = NULL,
  base_size     = 11,
  base_line_size = base_size/22,
  stroke        = 0,
  printplot     = TRUE,
  showcode      = TRUE,
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
  if (!missing(colheadings)) {
    panel.headings <- colheadings
    message("Note: colheadings argument is now called panel.headings")
  }
  if (!missing(boldheadings)) {
    bold.labels <- boldheadings
    message("Note: boldheadings argument is now called bold.labels")
  }

  # check arguments
  if (!missing(col.diamond) &&  !missing(diamond)) stop("Use either col.diamond or diamond, not both.")

  for (c in c(col.left, col.right)){
    if (any(unlist(lapply(panels, function(x) !c %in% names(x))))){
      stop("Column '", c, "' does not exist in every panels data frame.")
    }
  }

  # take first element if diamond is a list
  if (is.list(diamond)){ diamond <- diamond[[1]] }


  # transpose column headings if a list
  if (purrr::is_list(col.right.heading)){ col.right.heading <- purrr::transpose(col.right.heading)}
  if (purrr::is_list(col.left.heading)){ col.left.heading <- purrr::transpose(col.left.heading)}


  # Check for log scale
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


  # Identify columns to keep in data frame
  col.keep <- c(col.keep, col.diamond, col.bold)
  for (x in c(shape, cicolour, colour, fill, ciunder)){
    if (x %in% names(panels[[1]])){ col.keep <- append(col.keep, x) }
  }


  # make default panel.names
  if (is.null(panel.names)) { panel.names <- as.character(1:length(panels)) }


  # aesthetics: default value, match column name, or use argument itself
  shape.aes <- NULL
  if (is.null(shape)) {
    shape <- 15
  } else if (shape %in% names(panels[[1]])){
    shape.aes <- fixsp(shape)
    shape <- NULL
  }

  cicolour.aes <- NULL
  if (is.null(cicolour)) {
    cicolour <- fixq("black")
  }
  else if (all(cicolour %in% names(panels[[1]]))){
    cicolour.aes <- fixsp(cicolour)
    cicolour <- NULL
  } else {
    cicolour <- fixq(cicolour)
  }

  colour.aes <- NULL
  if (is.null(colour)) {
    colour <- fixq("black")
  } else if (all(colour %in% names(panels[[1]]))){
    colour.aes <- fixsp(colour)
    colour <- NULL
  } else {
    colour <- fixq(colour)
  }

  fill.aes <- NULL
  if (is.null(fill)) {
    fill <- fixq("black")
  } else if (fill %in% names(panels[[1]])){
    fill.aes <- fixsp(fill)
    fill <- NULL
  } else {
    fill <- fixq(fill)
  }

  if (is.null(col.bold)) { col.bold <- FALSE } else {col.bold <- fixsp(col.bold)}

  # codetext - list of character vectors for writing plot code
  codetext <- list()


  # spacing

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
    codetext$spacing <- "## Automatically calculated horizontal spacing and positioning:"
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
    codetext$spacing <- c(codetext$spacing, paste0("## right.space   = ", printunit(right.space)))
  }
  if (length(colspaceauto) > 1){colspaceauto <- colspaceauto[-length(colspaceauto)]}
  if (is.null(col.right.pos)){
    col.right.pos <- unit(colspaceauto, "mm")
    codetext$spacing <- c(codetext$spacing, paste0("## col.right.pos = ", printunit(col.right.pos)))
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
    codetext$spacing <- c(codetext$spacing, paste0("## left.space    = ", printunit(left.space)))
  }
  if (length(colspaceauto) > 1){colspaceauto <- colspaceauto[-length(colspaceauto)]}
  if (is.null(col.left.pos)){
    col.left.pos <- unit(colspaceauto, "mm")
    codetext$spacing <- c(codetext$spacing, paste0("## col.left.pos  = ", printunit(col.left.pos)))
  }


  # Write code for the axes
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

  if (is.null(xticks)) {
    xticks <- pretty(c(xfrom, xto))
  }

  codetext$axes <- c(
    make_layer(
      '# Set the scale for the x axis (the estimates and CIs)',
      f = "scale_x_continuous",
      arg = c(sprintf('trans  = "%s"', scale),
              paste0("breaks = ",paste(deparse(xticks), collapse = "")),
              'expand = c(0,0)')
    ),
    make_layer(
      '# Set the scale for the y axis (the rows)',
      f = "scale_y_continuous",
      arg = c('breaks = -1:-max(datatoplot$row)',
              'labels = rowlabels',
              'limits = c(-max(datatoplot$row), NA),',
              'expand = c(0,0)')
    )
  )


  codetext$prep.data <- make_layer(
    name = '# Prepare data to be plotted using ckbplotr::make_forest_data()',
    plus = FALSE,
    f = 'datatoplot <- ckbplotr::make_forest_data',
    arg = c(
      if (!identical(row.labels,
                     eval(formals(ckbplotr::make_forest_data)[["row.labels"]]))){
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
      if (!identical(addtext,
                     eval(formals(ckbplotr::make_forest_data)[["addtext"]]))){
        sprintf('addtext = %s',
                paste(deparse(substitute(addtext)), collapse = ''))
      }))


  # Write code to create a vector of row labels
  codetext$row.labels.vec <- c(
    '# Get a character vector of the row labels, so these can be used in the plot',
    'rowlabels <- datatoplot %>%',
    indent(14,
           'dplyr::group_by(row) %>%',
           'dplyr::summarise(row.label = dplyr::first(row.label),',
           indent(17, sprintf('bold = all(is.na(estimate_transformed) | all(key %%in%% %s)),',
                              ds(bold.labels)),
                  '.groups = "drop") %>%'),
           'dplyr::mutate(row.label = dplyr::if_else(bold & row.label != "",',
           indent(41, 'paste0("**", row.label, "**"),',
                  'as.character(row.label))) %>% '),
           'dplyr::arrange(row) %>%',
           'dplyr::pull(row.label)'),
    ''
  )


  # Write code to identify CIs that extend outside axis limits
  codetext$check.cis <- c(
    '# Identify any CIs that extend outside axis limits',
    'datatoplot <- datatoplot %>%',
    indent(16,
           sprintf('dplyr::mutate(cioverright  = (uci_transformed > %s),', xto),
           indent(14,
                  sprintf('uci_transformed = pmin(uci_transformed, %s),', xto),
                  sprintf('lci_transformed = pmin(lci_transformed, %s),', xto),
                  sprintf('cioverleft  = (lci_transformed < %s),', xfrom),
                  sprintf('lci_transformed = pmax(lci_transformed, %s),', xfrom),
                  sprintf('uci_transformed = pmax(uci_transformed, %s))', xfrom))),
    '')


  # Write code for plotting diamonds
  codetext$diamondscode <- NULL
  codetext$plotdiamondscode <- NULL
  if(!is.null(col.diamond) || !is.null(diamond)){
    if (!is.null(diamond)){
      codetext$diamondscode <- c(
        '# Create data frame for diamonds to be plotted',
        'diamonds <- datatoplot %>%',
        indent(2,
               sprintf('dplyr::filter(key %%in%% %s) %%>%%', deparse(diamond)),
               'dplyr::mutate(x1 = lci_transformed,',
               indent(14,
                      'x2 = estimate_transformed,',
                      'x3 = uci_transformed,',
                      'x4 = estimate_transformed) %>%'),
               'tidyr::gather(part, x, x1:x4) %>%',
               'dplyr::arrange(panel, row, part) %>%',
               'dplyr::mutate(y = - row + c(0, -0.25, 0, 0.25))'),
        '',
        '# Remove plotting of points if a diamond is to be used',
        'datatoplot <- datatoplot %>% ',
        sprintf(
          '  dplyr::mutate(estimate_transformed = dplyr::if_else(key %%in%% %s, as.numeric(NA), estimate_transformed),', deparse(diamond)),
        sprintf(
          '                lci_transformed = dplyr::if_else(key %%in%% %s, as.numeric(NA), lci_transformed),', deparse(diamond)),
        sprintf(
          '                uci_transformed = dplyr::if_else(key %%in%% %s, as.numeric(NA), uci_transformed))', deparse(diamond)),
        ''
      )
    } else {
      codetext$diamondscode <- c(
        '# Create data frame for diamonds to be plotted',
        'diamonds <- datatoplot %>%',
        indent(2,
               sprintf('dplyr::filter(%s == TRUE) %%>%%', fixsp(col.diamond)),
               'dplyr::mutate(x1 = lci_transformed,',
               indent(14,
                      'x2 = estimate_transformed,',
                      'x3 = uci_transformed,',
                      'x4 = estimate_transformed) %>%'),
               'tidyr::gather(part, x, x1:x4) %>%',
               'dplyr::arrange(panel, row, part) %>%',
               'dplyr::mutate(y = - row + c(0, -0.25, 0, 0.25))'),
        '',
        '# Remove plotting of points if a diamond is to be used',
        sprintf('if (any(datatoplot[["%s"]])) {', col.diamond),
        indent(2,
               sprintf('  datatoplot[!is.na(datatoplot[["%s"]]) & datatoplot[["%s"]],]$estimate_transformed <- NA', col.diamond, col.diamond),
               sprintf('  datatoplot[!is.na(datatoplot[["%s"]]) & datatoplot[["%s"]],]$lci_transformed <- NA', col.diamond, col.diamond),
               sprintf('  datatoplot[!is.na(datatoplot[["%s"]]) & datatoplot[["%s"]],]$uci_transformed <- NA', col.diamond, col.diamond)),
        '}',
        ''
      )
    }

    codetext$plotdiamondscode <- make_layer(
      '# Add diamonds',
      f = 'geom_polygon',
      aes = c('x = x, y = y, group = row',
              sprintf('colour = %s', cicolour.aes[1]),
              sprintf('fill = %s', fill.aes)),
      arg = c('data = diamonds',
              sprintf('colour = %s', cicolour[1]),
              sprintf('fill = %s', fill),
              sprintf('size = %s', stroke))
    )
  }


  # CI colour code - if using panel.width
  if (is.numeric(panel.width)) {
    cicolours <- c(cicolour, cicolour.aes)
    codetext$cicolourcode <- c(
      '# Create column for CI colour',
      'datatoplot <- datatoplot %>%',
      indent(2,
             sprintf('dplyr::mutate(cicolour =  dplyr::if_else((%s(uci_transformed) - %s(lci_transformed)) <= ',
                     scale, scale),
             indent(24,
                    sprintf('size * %s * dplyr::recode(%s, `22` = 0.6694, .default = 0.7553),',
                            (inv_tf(xto) - inv_tf(xfrom)) * pointsize / panel.width, c(shape, shape.aes)),
                    sprintf('%s,', cicolours[length(cicolours)]),
                    sprintf('%s))', cicolours[1]))),
      ''
    )
    cicolour.aes <- "cicolour"
    cicolour <- NULL
  }


  # CI under code - if using panel.width
  if (is.numeric(panel.width) && length(ciunder) > 1) {
    codetext$ciundercode <- c(
      '# Create column for CI under',
      'datatoplot <- datatoplot %>%',
      indent(2,
             sprintf('dplyr::mutate(ciunder =  dplyr::if_else((%s(uci_transformed) - %s(lci_transformed)) <= ',
                     scale, scale),
             indent(25,
                    sprintf('size * %s * dplyr::recode(%s, `22` = 0.6694, .default = 0.7553),',
                            (inv_tf(xto) - inv_tf(xfrom)) * pointsize / panel.width, c(shape, shape.aes)),
                    sprintf('%s,', ciunder[length(ciunder)]),
                    sprintf('%s))', ciunder[1]))),
      ''
    )
    ciunder <- "ciunder"
  }


  # Write code to initiate the ggplot
  codetext$start.ggplot <- c(
    '# Create the ggplot',
    'ggplot(datatoplot, aes(y=-row, x=estimate_transformed)) +',
    ''
  )


  # Write the code to put panels in facets
  codetext$facet <- make_layer(
    '# Put the different panels in side-by-side plots using facets',
    f = 'facet_wrap',
    arg = c('~panel, nrow = 1')
  )


  # Write code for line at null
  codetext$nullline <- if (!is.null(nullval)) {
    make_layer(
      '# Add a line at null effect',
      f = "annotate",
      arg = c('geom = "segment"',
              'y = -0.7, yend = -Inf',
              sprintf('x = %s, xend = %s', nullval, nullval),
              sprintf('size = %s', base_line_size))
    )
  }

  # Write code for plotting CIs
  codetext$plot.cis.before <- make_layer(
    '# Plot the CIs',
    f = 'geom_errorbar',
    aes = c(addaes$ci,
            'xmin = lci_transformed',
            'xmax = uci_transformed',
            sprintf('colour = %s', cicolour.aes[1])),
    arg = c(addarg$ci,
            'data = ~ dplyr::filter(.x, !is.na(estimate_transformed))',
            sprintf('colour = %s', cicolour[1]),
            'width = 0',
            sprintf('size = %s', base_line_size),
            'na.rm = TRUE')
  )

  if (isFALSE(ciunder) || is.null(ciunder)){
    codetext$plot.cis.after <- codetext$plot.cis.before
    codetext$plot.cis.before <- NULL
  } else if (is.character(ciunder)){
    codetext$plot.cis.before <- make_layer(
      '# Plot the CIs - before plotting points',
      f = 'geom_errorbar',
      aes = c(addaes$ci,
              'xmin = lci_transformed',
              'xmax = uci_transformed',
              sprintf('colour = %s', cicolour.aes[1])),
      arg = c(addarg$ci,
              sprintf('data = ~ dplyr::filter(.x, !is.na(estimate_transformed) & %s)', ciunder),
              sprintf('colour = %s', cicolour[1]),
              sprintf('size = %s', base_line_size),
              'width = 0',
              'na.rm = TRUE')
    )
    codetext$plot.cis.after <- make_layer(
      '# Plot the CIs - after plotting points',
      f = 'geom_errorbar',
      aes = c(addaes$ci,
              'xmin = lci_transformed',
              'xmax = uci_transformed',
              sprintf('colour = %s', cicolour.aes[1])),
      arg = c(addarg$ci,
              sprintf('data = ~ dplyr::filter(.x, !is.na(estimate_transformed) & !%s)', ciunder),
              sprintf('colour = %s', cicolour[1]),
              sprintf('size = %s', base_line_size),
              'width = 0',
              'na.rm = TRUE')
    )
  }


  # Write code to plot points
  codetext$plot.points <- c(
    make_layer(
      c('# Plot points at the transformed estimates',
        '## Scale by inverse of the SE'),
      f = 'geom_point',
      aes = c(addaes$point,
              'size = size',
              sprintf('shape = %s', shape.aes),
              sprintf('colour = %s', colour.aes),
              sprintf('fill = %s', fill.aes)),
      arg = c(addarg$point,
              sprintf('data = ~ dplyr::filter(.x, estimate_transformed > %s, estimate_transformed < %s)',
                      xfrom, xto),
              sprintf('shape = %s', shape),
              sprintf('colour = %s', colour),
              sprintf('fill = %s', fill),
              sprintf('stroke = %s', stroke),
              'na.rm = TRUE')
    ),
    make_layer(
      c('# Scale the size of points by their side length',
        '# and make the scale range from zero upwards'),
      f = 'scale_radius',
      arg = c('limits = c(0, 1)',
              sprintf('range = c(0, %s)', pointsize))
    )
  )


  # Write code to add arrows to CIs
  codetext$arrows <- c(
    make_layer(
      '# Add tiny segments with arrows when the CIs go outside axis limits',
      f = 'geom_segment',
      aes = c(addaes$ci,
              'y = -row',
              'yend = -row',
              'x = uci_transformed-0.000001',
              'xend = uci_transformed',
              sprintf('colour = %s', cicolour.aes[1])),
      arg = c(addarg$ci,
              'data = ~ dplyr::filter(.x, cioverright == TRUE)',
              sprintf('colour = %s', cicolour[1]),
              sprintf('size = %s', base_line_size),
              sprintf('arrow = arrow(type = "closed", length = unit(%s, "pt"))', 8 * base_line_size),
              'na.rm = TRUE'),
      br = FALSE
    ),
    make_layer(
      f = 'geom_segment',
      aes = c(addaes$ci,
              'y = -row',
              'yend = -row',
              'x = lci_transformed+0.000001',
              'xend = lci_transformed',
              sprintf('colour = %s', cicolour.aes[1])),
      arg = c(addarg$ci,
              'data = ~ dplyr::filter(.x, cioverleft == TRUE)',
              sprintf('colour = %s', cicolour[1]),
              sprintf('size = %s', base_line_size),
              sprintf('arrow = arrow(type = "closed", length = unit(%s, "pt"))', 8 * base_line_size),
              'na.rm = TRUE')
    )
  )



  # Write code for scales and coordinates
  codetext$scales.coords <- c(
    '# Use identity for aesthetic scales',
    'scale_shape_identity() +',
    'scale_fill_identity() +',
    'scale_colour_identity() +',
    '',
    make_layer(
      '# Set coordinate system',
      f = 'coord_cartesian',
      arg = c('clip = "off"',
              sprintf('xlim = c(%s, %s)', xfrom, xto))
    )
  )


  # Write code for columns to right of plots
  if (!is.null(col.right) | estcolumn) {
    col.right.all <- c(if (estcolumn){"auto_estcolumn"}, col.right)

    codetext$col.right.line <- unlist(purrr::pmap(
      list(col.right.all,
           as.numeric(col.right.pos),
           rep(makeunit(col.right.pos), length=length(col.right.pos)),
           col.right.heading,
           col.right.hjust,
           col.bold,
           col.right.parse,
           col.right.space,
           if(is.null(addaes$col.right)){""} else{addaes$col.right},
           if(is.null(addarg$col.right)){""} else{addarg$col.right}),
      ~ c(
        make_layer(
          sprintf('## column %s', ..1),
          f = 'ckbplotr::geom_text_move',
          aes = c(..9[..9!=""],
                  'y = -row',
                  sprintf('x = %s', round(tf(inv_tf(xto) + (inv_tf(xto) - inv_tf(xfrom)) * ..8), 6)),
                  if(is.character(..6)){
                    if(..7){
                      sprintf('label = dplyr::if_else(%s & !is.na(%s), paste0("bold(", %s,")"), %s)',
                              ..6, ..6, fixsp(..1), fixsp(..1))
                    } else {
                      c(sprintf('label = %s', fixsp(..1)),
                        sprintf('fontface = dplyr::if_else(%s & !is.na(%s),"bold", "plain")', ..6, ..6))
                    }
                  } else {
                    sprintf('label = %s', fixsp(..1))
                  }),
          arg = c(..10[..10!=""],
                  sprintf('move_x = unit(%s, "%s")', ..2, ..3),
                  sprintf('hjust = %s', ..5),
                  sprintf('size  = %s', base_size/(11/3)),
                  'na.rm = TRUE',
                  sprintf('parse = %s', ..7)),
          br = FALSE
        ),
        make_layer(
          f = 'ckbplotr::geom_text_move',
          aes = c(sprintf('y = %s', col.heading.space),
                  sprintf('x = %s', round(tf(inv_tf(xto) + (inv_tf(xto) - inv_tf(xfrom)) * ..8), 6)),
                  'label = title'),
          arg = c(sprintf('move_x = unit(%s, "%s")', ..2, ..3),
                  sprintf('hjust    = %s', ..5),
                  sprintf('size     = %s', base_size/(11/3)),
                  'fontface = "bold"',
                  sprintf('data = dplyr::tibble(panel = factor(%s', paste(deparse(panel.names), collapse = '')),
                  indent(36,
                         sprintf('levels = %s', paste(deparse(panel.names), collapse = ''))),
                  indent(36,
                         'ordered = TRUE)'),
                  indent(21,
                         sprintf('title = %s)', ds(unlist(..4)))))
        )
      )
    )
    )
    codetext$col.right.line <- c('# Add columns to right side of plots',
                                 codetext$col.right.line)
  }


  # Write code for addtext
  if (!is.null(addtext)){
    codetext$addtext <- make_layer(
      '## addtext',
      f = 'ckbplotr::geom_text_move',
      aes = c('y = -row',
              sprintf('x = %s',
                      round(tf(inv_tf(xto) + (inv_tf(xto) - inv_tf(xfrom)) * col.right.space[[1]]),
                            6)),
              if(is.character(col.bold[[1]])){
                if(col.parse[[1]]){
                  sprintf('label = dplyr::if_else(%s & !is.na(%s), paste0("bold(addtext)"), addtext)',
                          col.bold[[1]], col.bold[[1]])
                } else {
                  c('label = addtext',
                    sprintf('fontface = dplyr::if_else(%s & !is.na(%s),"bold", "plain")',
                            col.bold[[1]], col.bold[[1]]))
                }
              } else {
                'label = addtext'
              }),
      arg = c(sprintf('move_x = unit(%s, "%s")',
                      as.numeric(col.right.pos[[1]]),
                      makeunit(col.right.pos[[1]])),
              sprintf('hjust = %s', col.right.hjust[[1]]),
              sprintf('size  = %s', base_size/(11/3)),
              'na.rm = TRUE',
              'parse = TRUE')
    )
  }

  # Write code for columns to left of plots
  if (!is.null(col.left)) {
    codetext$col.left.line <- unlist(purrr::pmap(
      list(col.left,
           as.numeric(col.left.pos),
           rep(makeunit(col.left.pos), length=length(col.left.pos)),
           col.left.heading,
           col.left.hjust,
           col.bold,
           col.left.space,
           if(is.null(addaes$col.left)){""} else{addaes$col.left},
           if(is.null(addarg$col.left)){""} else{addarg$col.left}),
      ~ c(
        make_layer(
          sprintf('## column %s', ..1),
          f = 'ckbplotr::geom_text_move',
          aes = c(..8[..8!=""],
                  'y = -row',
                  sprintf('x = %s', round(tf(inv_tf(xfrom) - (inv_tf(xto) - inv_tf(xfrom)) * ..7), 6)),
                  sprintf('label = %s', fixsp(..1)),
                  if(is.character(..6)){
                    sprintf('fontface = dplyr::if_else(%s & !is.na(%s),"bold", "plain")', ..6, ..6)
                  } else {
                    'fontface = "plain"'
                  }),
          arg = c(..9[..9!=""],
                  sprintf('move_x = unit(-%s, "%s")', ..2, ..3),
                  sprintf('hjust = %s', ..5),
                  sprintf('size  = %s', base_size/(11/3)),
                  'na.rm = TRUE'),
          br = FALSE
        ),
        make_layer(
          f = 'ckbplotr::geom_text_move',
          aes = c(sprintf('y = %s', col.heading.space),
                  sprintf('x = %s', round(tf(inv_tf(xfrom) - (inv_tf(xto) - inv_tf(xfrom)) * ..7), 6)),
                  'label = title'),
          arg = c(sprintf('move_x = unit(-%s, "%s")', ..2, ..3),
                  sprintf('hjust    = %s', ..5),
                  sprintf('size     = %s', base_size/(11/3)),
                  'fontface = "bold"',
                  sprintf('data = dplyr::tibble(panel = factor(%s', paste(deparse(panel.names), collapse = '')),
                  indent(36,
                         sprintf('levels = %s', paste(deparse(panel.names), collapse = ''))),
                  indent(36,
                         'ordered = TRUE)'),
                  indent(21,
                         sprintf('title = %s)', ds(unlist(..4)))))
        )
      )
    )
    )
    codetext$col.left.line <- c('# Add columns to left side of plots',
                                codetext$col.left.line)
  }



  # Write code for x-axis labels and panel headings
  codetext$xlab.panel.headings <- c(
    make_layer(
      '# Add xlab below each axis',
      f = 'geom_text',
      aes = c(addaes$xlab,
              sprintf('y = -Inf, x = %s, label = xlab', xmid)),
      arg = c(addarg$xlab,
              'hjust = 0.5',
              sprintf('size  = %s', base_size/(11/3)),
              'vjust = 4.4',
              'fontface = "bold"',
              sprintf('data = dplyr::tibble(panel = factor(%s', paste(deparse(panel.names), collapse = '')),
              indent(36, sprintf('levels = %s', paste(deparse(panel.names), collapse = ''))),
              indent(36, 'ordered = TRUE)'),
              indent(21, sprintf('xlab = %s)', ds(xlab))))
    ),
    if (!all(panel.headings == "")){
      make_layer(
        '# Add panel name above each panel',
        f = 'geom_text',
        aes = c(addaes$panel.name,
                sprintf('y = %s, x = %s, label = title', col.heading.space, xmid)),
        arg = c(addarg$panel.name,
                'hjust = 0.5',
                'nudge_y = 2',
                sprintf('size  = %s', base_size/(11/3)),
                'fontface = "bold"',
                sprintf('data = dplyr::tibble(panel = factor(%s', paste(deparse(panel.names), collapse = '')),
                indent(36, sprintf('levels = %s', paste(deparse(panel.names), collapse = ''))),
                indent(36, 'ordered = TRUE)'),
                indent(21, sprintf('title = %s)', ds(panel.headings))))
      )
    }
  )



  # Write code for the plot title
  if (title != ""){
    codetext$title <- make_layer(
      '# Add the title',
      f = 'labs',
      arg = sprintf('title = "%s"', title)
    )
  }


  # Write code for the theme
  codetext$theme <- make_layer(
    '# Control the overall look of the plots',
    f = 'theme',
    arg = c(sprintf('text             = element_text(size = %s)', base_size),
            sprintf('line             = element_line(size = %s)', base_line_size),
            'panel.background = element_blank()',
            'panel.grid.major = element_blank()',
            'panel.grid.minor = element_blank()',
            if (title == ""){
              'plot.title       = element_blank()'
            } else {
              'plot.title.position = "plot"'
            },
            sprintf('axis.line.x      = element_line(size = %s, lineend = "round")',
                    base_line_size),
            'axis.title       = element_blank()',
            'axis.ticks.x     = element_line(colour = "black")',
            'axis.text.x      = element_text(colour = "black"',
            indent(32,
                   sprintf('margin = margin(t = %s)',base_size/(11/4.4)),
                   'vjust  = 1)'),
            'axis.ticks.y     = element_blank()',
            'axis.line.y      = element_blank()',
            'axis.text.y      = ggtext::element_markdown(hjust  = 0',
            indent(44,
                   'colour = "black"',
                   sprintf('margin = margin(r = %s, unit = "%s"))',
                           as.numeric(left.space), makeunit(left.space))),
            'panel.border     = element_blank()',
            sprintf('panel.spacing    = unit(%s, "%s") + %s + unit(%s, "%s")',
                    as.numeric(right.space),
                    makeunit(right.space),
                    paste(deparse(substitute(mid.space)), collapse = ''),
                    as.numeric(left.space),
                    makeunit(left.space)),
            'strip.background = element_blank()',
            'strip.placement  = "outside"',
            'strip.text       = element_blank()',
            'legend.position  = "none"',
            'plot.background  = element_blank()',
            sprintf('plot.margin      = %s + unit(c(0, %s, 0, 0), "%s")',
                    paste(deparse(substitute(plot.margin)), collapse = ''),
                    as.numeric(right.space),
                    makeunit(right.space))),
    plus = FALSE,
    duplicates = TRUE
  )

  # Create the plot code
  plotcode <- c(
    codetext$spacing,
    '',
    'library(ggplot2)',
    '',
    codetext$prep.data,
    codetext$row.labels.vec,
    codetext$check.cis,
    codetext$diamondscode,
    codetext$cicolourcode,
    codetext$ciundercode,
    codetext$start.ggplot,
    indent(2,
           codetext$facet,
           codetext$nullline,
           codetext$plot.cis.before,
           codetext$plot.points,
           codetext$plot.cis.after,
           codetext$arrows,
           codetext$plotdiamondscode,
           codetext$scales.coords,
           codetext$col.right.line,
           codetext$addtext,
           codetext$col.left.line,
           codetext$xlab.panel.headings,
           codetext$axes,
           codetext$title,
           codetext$theme)
  )


  # add additional code
  if (!is.null(addcode)){
    plotcode <- append(plotcode, addcode[2:length(addcode)], grep(addcode[1], trimws(plotcode))[1]-1)
  }

  # Show code in RStudio viewer.
  if (showcode){ displaycode(plotcode) }


  # Create plot and print
  plot <- eval(parse(text = plotcode), envir = envir)
  if (printplot){
    print(plot)
  }

  return(list(plot = plot,
              code = plotcode) )
}










#' Fix panel width and height of a forest plot
#'
#' \code{fix_panel} fixes the panel width and height of a forest plot
#'
#' @param plot A plot (created by make_forest_plot()).
#' @param width Width of panels. (e.g unit(50, "mm"))
#' @param height Height of panels. (e.g unit(150, "mm"))
#'
#' @return A gtable object
#'
#' @import ggplot2
#' @export


fix_panel <- function(plot, width = NULL, height = NULL){
  gtable <- ggplot2::ggplotGrob(plot)
  if(!is.null(width)){gtable$widths[gtable$layout$l[grepl("panel", gtable$layout$name)]] <- width}
  if(!is.null(height)){gtable$heights[gtable$layout$t[grepl("panel", gtable$layout$name)]] <- height}
  gtable
}
