
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
#'   rows of the plot. The data frame must contain columns 'heading1', 'heading2'
#'   and 'heading3'. Use NA if a lower level heading is not required.
#' @param rows A character vector. The top level labels (heading1) of rows
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
#'
#'
#' @return A dataset from which a forest plot can be generated.
#'
#'
#' @keywords internal
#'
#' @export

make_forest_data <- function(
  panels,
  cols          = panels,
  col.key       = "key",
  row.labels    = NULL,
  headings      = NULL,
  rows          = NULL,
  panel.names   = NULL,
  colnames      = NULL,
  col.estimate  = "estimate",
  col.stderr    = "stderr",
  col.lci       = NULL,
  col.uci       = NULL,
  col.left      = NULL,
  col.right     = NULL,
  col.keep      = NULL,
  ci.delim      = ", ",
  exponentiate  = TRUE,
  blankrows     = c(1, 1, 0, 0),
  scalepoints   = FALSE,
  minse         = NULL,
  addtext       = NULL
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
        dplyr::mutate(extratext = dplyr::case_when(
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
                      extratext) %>%
        dplyr::mutate(key = as.character(key))

      extrarowkeys <- c(extrarowkeys, addtext[[i]][["key"]])
    }
  }
  extrarowkeys <- unique(extrarowkeys)

  if (is.null(row.labels)) {
    out <- panels[[1]] %>%
      dplyr::mutate(row.label = !!rlang::sym(col.key),
                    key = !!rlang::sym(col.key),
                    extrarowkey = "") %>%
      dplyr::select(row.label, key, extrarowkey) %>%
      dplyr::add_row(row.label = "") %>%
      dplyr::mutate(row = 1:dplyr::n())

    # Add extra rows for addtext
    if (!is.null(addtext)) {
      for (k in 1:length(extrarowkeys)) {
        out <- out %>%
          dplyr::add_row(row.label = "", extrarowkey = paste0(extrarowkeys[[k]]),
                         .after = which(out$key == extrarowkeys[[k]]))
      }
    }
    out <- out %>%
      dplyr::mutate(row = 1:dplyr::n())


  } else {

    if (is.null(rows)) stop("argument rows must be given if row.labels is used")
    if (!col.key %in% names(row.labels)) stop(paste0(col.key, " must be a column in ",  deparse(substitute(row.labels))))

    for (panel in panels) {
      if (!col.key %in% names(panel)) stop(paste0(col.key, " must be a column in every data frame given in panels"))
    }

    for (head1 in rows) {
      if (!(head1 %in% row.labels$heading1)) {
        stop(paste(head1,"is not in heading1 column of", deparse(substitute(row.labels))))
      }
    }

    row.labels <- row.labels %>%
      dplyr::mutate(heading1 = heading1,
                    heading2 = heading2,
                    heading3 = heading3,
                    key = !!rlang::sym(col.key))

    out <- tibble::tibble(row.label = "", key = "", extrarowkey = "", removelater = TRUE)

    for (head1 in rows) {
      l2headings <- row.labels %>%
        dplyr::filter(heading1 == head1) %>%
        dplyr::select(heading2, key) %>%
        dplyr::distinct(heading2, .keep_all = TRUE)

      if (is.na(l2headings[[1, "heading2"]])) {
        if (head1 != "") {out <- tibble::add_row(out,
                                                 row.label = head1,
                                                 key = row.labels %>%
                                                   dplyr::filter(heading1 == head1) %>%
                                                   dplyr::pull(key))}

        # Add extra row for addtext
        if (row.labels[[which(row.labels$heading1 == head1),"key"]] %in% extrarowkeys) {
          out <- tibble::add_row(out,
                                 row.label = "",
                                 extrarowkey = row.labels[[which(row.labels$heading1 == head1), "key"]])
        }
      }
      else{

        if (head1 != "") {out <- tibble::add_row(out, row.label = head1)}
        if (blankrows[[1]] > 0) {for (i in 1:blankrows[[1]]) { out <- tibble::add_row(out, row.label = "") }}

        for (head2 in 1:nrow(l2headings)) {

          l3headings <- row.labels %>%
            dplyr::filter(heading1 == head1 & heading2 == l2headings[[head2, "heading2"]]) %>%
            dplyr::select(heading3, key)

          if (is.na(l3headings[[1, "heading3"]])) {
            if (head2 != "") {out <- tibble::add_row(out,
                                                     row.label = l2headings[[head2, "heading2"]],
                                                     key = l2headings[[head2, "key"]])}

            # Add extra row for addtext
            if (l2headings[[head2, "key"]] %in% extrarowkeys) {
              out <- tibble::add_row(out,
                                     row.label = "",
                                     extrarowkey = l2headings[[head2, "key"]])
            }
          }
          else{
            if (head2 != "") {out <- tibble::add_row(out,
                                                     row.label = l2headings[[head2, "heading2"]])}
            if (blankrows[[3]] > 0) {for (i in 1:blankrows[[3]]) { out <- tibble::add_row(out, row.label = "") }}
            for (head3 in 1:nrow(l3headings)) {
              out <- tibble::add_row(out,
                                     row.label = l3headings[[head3, "heading3"]],
                                     key = l3headings[[head3, "key"]])

              # Add extra row for addtext
              if (l3headings[[head3, "key"]] %in% extrarowkeys) {
                out <- tibble::add_row(out,
                                       row.label = "",
                                       extrarowkey = l3headings[[head3, "key"]])
              }
            }
          }
          if (blankrows[[4]] > 0) {for (i in 1:blankrows[[4]]) { out <- tibble::add_row(out, row.label = "") }}
        }
      }
      if (blankrows[[2]] > 0) {for (i in 1:blankrows[[2]]) { out <- tibble::add_row(out, row.label = "") }}
    }


    # add a blank heading at bottom if needed
    if (utils::tail(out$row.label, 1) != "") {
      out <- out %>%
        tibble::add_row(row.label = "")
    }

    out <- out %>%
      dplyr::filter(is.na(removelater) | !removelater) %>%
      dplyr::select(-removelater) %>%
      dplyr::mutate(row = 1:dplyr::n())
  }


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
      out1 <- dplyr::mutate(out1, extratext = as.character(NA))
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
      dplyr::mutate(estimate_transformed = tf(estimate),
                    lci_transformed = tf(lci),
                    uci_transformed = tf(uci)
      )
    if (is.null(minse)){
      minse <- min((datatoplot$estimate - datatoplot$lci)/1.96, na.rm = TRUE)
    } else {
      if (minse > min((datatoplot$estimate - datatoplot$lci)/1.96, na.rm = TRUE)) stop("minse is larger than the minimum standard error in the data")
    }
    datatoplot$size <- 1.96*minse/(datatoplot$estimate - datatoplot$lci)
  } else {
    datatoplot <- datatoplot %>%
      dplyr::mutate(estimate_transformed = tf(estimate),
                    lci_transformed = tf(estimate - 1.96*stderr),
                    uci_transformed = tf(estimate + 1.96*stderr)
      )
    if (is.null(minse)){
      minse <- min(datatoplot$stderr, na.rm = TRUE)
    } else {
      if (minse > min(datatoplot$stderr, na.rm = TRUE)) stop("minse is larger than the minimum standard error in the data")
    }
    datatoplot$size <- minse/datatoplot$stderr
  }

  datatoplot <- datatoplot %>%
    dplyr::mutate(textresult = dplyr::case_when(
      !is.na(estimate) ~ paste0("'",format(round(estimate_transformed, 2), nsmall = 2),
                                " (",
                                format(round(lci_transformed, 2), nsmall = 2),
                                ci.delim,
                                format(round(uci_transformed, 2), nsmall = 2),
                                ")'"),
      !is.na(extratext) ~ extratext,
      TRUE              ~ "''")) %>%
    dplyr::select(-extrarowkey, -extratext) %>%
    dplyr::arrange(panel, row)


  if (!scalepoints) {
    datatoplot$size <- 1
  }

  return(datatoplot)
}









































#' Make forest plot with ggplot2
#'
#' \code{make_forest_plot} creates a forest plot with ggplot
#'
#' The function returns the plot, data and ggplot2 code to create the plot.
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
#' @param col.left.space A numeric vector. Sizes of the gaps between the plot
#' and col.left columns. As a multiple of the length of the x-axis. (Default: 0)
#' @param col.right.space Size of the gap between the plot and column to the
#' right of the plot. As a multiple of the length of the x-axis. (Default: 0)
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
#' @param label.space Size of the gap between row labels and the first panel. (Default: 4)
#' @param panel.space Size of the gap between forest plot panels. (Default: 8)
#' @param panel.width Panel width to assume and apply different formatting to narrow CIs. Unit is "mm".
#' @param stroke Size of outline of shapes. (Default: 0)
#' @param margin Plot margin (top, right, bottom, left). (Default: c(2, 6, 2, 1))
#' @param units Units to use for label.space, panel.space and margin (Default: "Lines")
#' @param printplot Print the plot. (Default: TRUE)
#' @param showcode Show the ggplot2 code to generate the plot in RStudio 'Viewer' pane. (Default: TRUE)
#' @param envir Environment in which to evaluate the plot code. May be useful when calling this function inside another function.
#'
#' @return A list:
#' \describe{
#'   \item{plot}{the plot}
#'   \item{data}{a data frame from which the plot is made}
#'   \item{code}{ggplot2 code to generate the plot}
#'}
#'
#' @import ggplot2
#' @export




make_forest_plot <- function(
  panels,
  row.labels    = NULL,
  headings      = NULL,
  rows          = NULL,
  cols          = panels,
  exponentiate  = TRUE,
  logscale      = exponentiate,
  panel.names   = NULL,
  colnames      = NULL,
  panel.headings = panel.names,
  colheadings   = colnames,
  col.key       = "key",
  col.estimate  = "estimate",
  col.stderr    = "stderr",
  col.lci       = NULL,
  col.uci       = NULL,
  col.left      = NULL,
  col.right     = NULL,
  col.left.heading  = "",
  col.right.heading = "HR (95% CI)",
  col.left.space    = 0.02,
  col.right.space   = 0.02,
  col.left.hjust    = 1,
  col.right.hjust   = 0,
  col.heading.space = 0,
  estcolumn     = TRUE,
  col.keep      = NULL,
  ci.delim      = ", ",
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
  boldheadings  = NULL,
  scalepoints   = FALSE,
  minse         = NULL,
  pointsize     = 3,
  shape     = NULL,
  colour    = NULL,
  cicolour  = colour,
  fill      = NULL,
  ciunder   = NULL,
  addtext       = NULL,
  label.space   = 4,
  heading.space = NULL,
  panel.space   = 8,
  plot.space    = NULL,
  panel.width   = NULL,
  base_size     = 11,
  base_line_size = base_size/22,
  stroke        = 0,
  margin        = c(2, 6, 2, 1),
  units         = "lines",
  printplot     = TRUE,
  showcode      = TRUE,
  envir         = NULL
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
  if (!missing(heading.space)) {
    label.space <- heading.space
    message("Note: heading.space argument is now called label.space")
  }
  if (!missing(plot.space)) {
    panel.space <- plot.space
    message("Note: plot.space argument is now called panel.space")
  }


  # check arguments
  if (!missing(col.diamond) &&  !missing(diamond)) stop("Use either col.diamond or diamond, not both.")

  # take first element if diamond is a list
  if (is.list(diamond)){ diamond <- diamond[[1]] }

  # transpose column headings if a list
  if (purrr::is_list(col.right.heading)){ col.right.heading <- purrr::transpose(col.right.heading)}
  if (purrr::is_list(col.left.heading)){ col.left.heading <- purrr::transpose(col.left.heading)}

  if (logscale == TRUE) {
    tf       <- exp
    inv_tf   <- log
    scale    <- "log"
    if (is.null(nullval)) {nullval <- 1}
  } else {
    tf       <- identity
    inv_tf   <- identity
    scale    <- "identity"
  }

  col.keep <- c(col.keep, col.diamond, col.bold)
  for (x in c(shape, cicolour, colour, fill, ciunder)){
    if (x %in% names(panels[[1]])){ col.keep <- append(col.keep, x) }
  }

  # make deault panel.names
  if (is.null(panel.names)) { panel.names <- as.character(1:length(panels)) }

  # line at null
  nullline <- sprintf('  annotate(geom = "segment", y=-1, yend=-Inf, x=%s, xend=%s, size = %s) +',
                      nullval, nullval, base_line_size)

  # aesthetics: default value, match column name, or use argument itself
  if (is.null(shape)) {
    shape <- 15
  } else if (shape %in% names(panels[[1]])){
    shape <- paste0("`", shape, "`")
  }

  cicolouraes <- FALSE
  if (is.null(cicolour)) {
    cicolour <- '\"black\"'
  }
  else if (all(cicolour %in% names(panels[[1]]))){
    cicolour <- paste0("`", cicolour, "`")
    cicolouraes <- TRUE
  } else {
    cicolour <- paste0('\"', cicolour, '\"')
  }

  if (is.null(colour)) {
    colour <- '\"black\"'
  } else if (colour %in% names(panels[[1]])){
    colour <- paste0("`", colour, "`")
  } else {
    colour <- paste0('\"', colour, '\"')
  }

  if (is.null(fill)) {
    fill <- '\"black\"'
  } else if (fill %in% names(panels[[1]])){
    fill <- paste0("`", fill, "`")
  } else {
    fill <- paste0('\"', fill, '\"')
  }

  if (is.null(col.bold)) { col.bold <- FALSE } else {col.bold <- paste0("`", col.bold, "`")}

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
  xticksline <- paste0("                     breaks = ",paste(deparse(xticks), collapse = ""),",")


  # columns to right of plots
  if (is.null(col.right) & !estcolumn) {
    col.right.line <- ""
  } else {

    col.right.all <- col.right
    if (estcolumn){
      col.right.all <- c("textresult", col.right)
    }

    col.right.line <- unlist(purrr::pmap(list(col.right.all, col.right.space, col.right.heading, col.right.hjust, col.bold),
                                         ~ c(sprintf('  ## column %s', ..1),
                                             sprintf('  geom_text(aes(y = -row, x = %s,',
                                                     round(tf(inv_tf(xto) + (inv_tf(xto) - inv_tf(xfrom)) * ..2), 6)),
                                             if(is.character(..5)){
                                               sprintf('                label = dplyr::if_else(%s & !is.na(%s), paste0("bold(",`%s`,")"), %s)),',
                                                       ..5, ..5, ..1, ..1)
                                               } else {
                                                sprintf('                label = %s),', ..1)
                                                       },
                                             sprintf(
                                             '            hjust = %s,', ..4),
                                             sprintf(
                                             '            size = %s,', base_size/(11/3)),
                                             '            na.rm = TRUE,',
                                             '            parse = TRUE) +',
                                             sprintf(
                                             '  geom_text(aes(y = %s, x = %s,',
                                             col.heading.space,
                                             round(tf(inv_tf(xto) + (inv_tf(xto) - inv_tf(xfrom)) * ..2), 6)),
                                             '                label = title),',
                                             sprintf(
                                             '            hjust = %s,', ..4),
                                             sprintf(
                                             '            size = %s,', base_size/(11/3)),
                                             '            fontface = "bold",',
                                             sprintf(
                                             '            data = dplyr::tibble(panel = factor(%s,',
                                             paste(deparse(panel.names), collapse = '')),
                                             sprintf(
                                             '                                                levels = %s,',
                                             paste(deparse(panel.names), collapse = '')),
                                             '                                                ordered = TRUE),',
                                             sprintf(
                                             '                                 title = %s)) +',
                                             paste(deparse(unlist(..3)), collapse = '')))))
  }




  # columns to left of plots
  if (is.null(col.left)) {
    col.left.line <- ""
  } else {
    col.left.line <- unlist(purrr::pmap(list(col.left, col.left.space, col.left.heading, col.left.hjust, col.bold),
                                        ~ c(sprintf('  ## column %s', ..1),
                                            sprintf(
                                            '  geom_text(aes(y = -row, x = %s,',
                                            round(tf(inv_tf(xfrom) - (inv_tf(xto) - inv_tf(xfrom)) * ..2), 6)),
                                            sprintf(
                                            '                label = `%s`,',
                                            ..1
                                            ),
                                            if(is.character(..5)){
                                            sprintf(
                                            '            fontface = dplyr::if_else(!is.na(%s) & %s, "bold", "plain")),',
                                            ..5, ..5)} else {
                                            '            fontface = "plain"),'
                                            },
                                            sprintf(
                                            '            hjust = %s,', ..4),
                                            sprintf(
                                            '            size = %s,', base_size/(11/3)),
                                            '            na.rm = TRUE) +',
                                            sprintf(
                                            '  geom_text(aes(y = %s, x = %s,',
                                            col.heading.space,
                                            round(tf(inv_tf(xfrom) - (inv_tf(xto) - inv_tf(xfrom)) * ..2), 6)),
                                            '                label = title),',
                                            sprintf(
                                            '            hjust = %s,', ..4),
                                            sprintf(
                                            '            size = %s,', base_size/(11/3)),
                                            '            fontface = "bold",',
                                            sprintf(
                                            '            data = dplyr::tibble(panel = factor(%s,',
                                            paste(deparse(panel.names), collapse = '')),
                                            sprintf(
                                            '                                                levels = %s,',
                                            paste(deparse(panel.names), collapse = '')),
                                            '                                                ordered = TRUE),',
                                            sprintf(
                                            '                                 title = %s)) +',
                                            paste(deparse(unlist(..3)), collapse = '')))))
  }

  diamondscode <- NULL
  plotdiamondscode <- NULL
  if(!is.null(col.diamond) || !is.null(diamond)){
    if (!is.null(diamond)){
      diamondscode <- c(
        '# Create data frame for diamonds to be plotted',
        'diamonds <- datatoplot %>%',
        sprintf(
        '  dplyr::filter(key %%in%% %s) %%>%%', deparse(diamond)),
        '  dplyr::mutate(x1 = lci_transformed,',
        '                x2 = estimate_transformed,',
        '                x3 = uci_transformed,',
        '                x4 = estimate_transformed) %>%',
        '  tidyr::gather(part, x, x1:x4) %>%',
        '  dplyr::arrange(panel, row, part) %>%',
        '  dplyr::mutate(y = - row + c(0, -0.25, 0, 0.25))',
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
      diamondscode <- c(
        '# Create data frame for diamonds to be plotted',
        'diamonds <- datatoplot %>%',
        sprintf(
          '  dplyr::filter(`%s` == TRUE) %%>%%', col.diamond),
        '  dplyr::mutate(x1 = lci_transformed,',
        '                x2 = estimate_transformed,',
        '                x3 = uci_transformed,',
        '                x4 = estimate_transformed) %>%',
        '  tidyr::gather(part, x, x1:x4) %>%',
        '  dplyr::arrange(panel, row, part) %>%',
        '  dplyr::mutate(y = - row + c(0, -0.25, 0, 0.25))',
        '',
        '# Remove plotting of points if a diamond is to be used',
        sprintf('if (any(datatoplot[["%s"]])) {', col.diamond),
        sprintf('  datatoplot[!is.na(datatoplot[["%s"]]) & datatoplot[["%s"]],]$estimate_transformed <- NA', col.diamond, col.diamond),
        sprintf('  datatoplot[!is.na(datatoplot[["%s"]]) & datatoplot[["%s"]],]$lci_transformed <- NA', col.diamond, col.diamond),
        sprintf('  datatoplot[!is.na(datatoplot[["%s"]]) & datatoplot[["%s"]],]$uci_transformed <- NA', col.diamond, col.diamond),
        '}',
        ''
      )
    }

    plotdiamondscode <- c(
      '  # Add diamonds',
      '  geom_polygon(data = diamonds,',
      '               aes(x = x, y = y, group = row,',
      sprintf(
      '                   colour = %s, fill = %s),', cicolour[1], fill),
      sprintf(
      '               size = %s) +', stroke),
      ''
    )
  }

  # CI colour code
  if (is.numeric(panel.width) && length(cicolour) > 1) {
    cicolourcode <- c(
      '# Create column for CI colour',
      'datatoplot <- datatoplot %>%',
      sprintf(
      '  dplyr::mutate(cicolour =  dplyr::if_else((%s(uci_transformed) - %s(lci_transformed)) <= ',
      scale, scale),
      sprintf(
      '                           size * %s * dplyr::recode(%s, `22` = 0.6694, .default = 0.7553),',
      (inv_tf(xto) - inv_tf(xfrom)) * pointsize / panel.width, shape),
      sprintf(
        '                                    %s,', cicolour[length(cicolour)]),
      sprintf(
        '                                    %s))', cicolour[1])
    )
    cicolour <- "cicolour"
    cicolouraes <- TRUE
  } else {
    cicolourcode <- ''
  }


  # CI under code
  if (is.numeric(panel.width) && length(ciunder) > 1) {
    ciundercode <- c(
      '# Create column for CI under',
      'datatoplot <- datatoplot %>%',
      sprintf(
      '  dplyr::mutate(ciunder =  dplyr::if_else((%s(uci_transformed) - %s(lci_transformed)) <= ',
      scale, scale),
      sprintf(
      '                           size * %s * dplyr::recode(%s, `22` = 0.6694, .default = 0.7553),',
      (inv_tf(xto) - inv_tf(xfrom)) * pointsize / panel.width, shape),
      sprintf(
        '                                    %s,', ciunder[length(ciunder)]),
      sprintf(
        '                                    %s))', ciunder[1])
    )
    ciunder <- "ciunder"
  } else {
    ciundercode <- ""
  }

  argset <- function(x){
    sprintf('                %s = %s,',
            paste(deparse(substitute(x)), collapse = ''),
            paste(deparse(x), collapse = ''))
  }

  plotcode <- c(
                '# Prepare data to be plotted using ckbplotr::make_forest_data()',
                'datatoplot <- ckbplotr::make_forest_data(',
                sprintf(
                '                row.labels = %s,',
                if (!missing(headings)) {
                  paste(deparse(substitute(headings)), collapse = '')
                } else {
                  paste(deparse(substitute(row.labels)), collapse = '')
                }
                ),
                argset(rows),
                sprintf(
                '                panels = %s,',
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
                argset(exponentiate),
                argset(blankrows),
                argset(scalepoints),
                argset(minse),
                sprintf(
                '                addtext = %s)',
                paste(deparse(substitute(addtext)), collapse = '')
                ),
                '',
                '# Get a character vector of the row labels, so these can be used in the plot',
                'rowlabels <- datatoplot %>%',
                '              dplyr::group_by(row) %>%',
                '              dplyr::summarise(row.label = dplyr::first(row.label)) %>%',
                '              dplyr::arrange(row) %>%',
                '              dplyr::pull(row.label)',
                '',
                '# Get a character vector of the style for row labels',
                'boldlabels <- datatoplot %>%',
                '                  dplyr::group_by(row) %>%',
                sprintf(
                '                  dplyr::summarise(bold = dplyr::if_else(all(is.na(estimate_transformed) | all(key %%in%% %s)), "bold", "plain")) %%>%%',
                paste(deparse(bold.labels))),
                '                  dplyr::arrange(row) %>%',
                '                  dplyr::pull(bold)',
                '',

                '# Identify any CIs that extend outside axis limits',
                'datatoplot <- datatoplot %>%',
                sprintf(
                '                dplyr::mutate(cioverright  = (uci_transformed > %s),', xto),
                sprintf(
                '                              uci_transformed = pmin(uci_transformed, %s),', xto),
                sprintf(
                '                              lci_transformed = pmin(lci_transformed, %s),', xto),
                sprintf(
                '                              cioverleft  = (lci_transformed < %s),', xfrom),
                sprintf(
                '                              lci_transformed = pmax(lci_transformed, %s),', xfrom),
                sprintf(
                '                              uci_transformed = pmax(uci_transformed, %s))', xfrom),
                '',
                diamondscode,
                '',
                cicolourcode,
                '',
                ciundercode,
                '',
                '# Create the ggplot',
                'ggplot(datatoplot, aes(y=-row, x=estimate_transformed)) +',
                '',
                '  # Put the different panels in side-by-side plots using facets',
                '  facet_wrap(~panel, nrow = 1) +',
                '',
                '  # Add a line at null effect',
                nullline,
                '',
                if (isTRUE(ciunder)){
                  c(
                    '  # Plot the CIs',
                    '  geom_linerange(data = ~ dplyr::filter(.x, !is.na(estimate_transformed)),',
                    '                 aes(xmin = lci_transformed,',
                    '                     xmax = uci_transformed,',
                    sprintf(
                    '                     colour = %s),', cicolour[1]),
                    sprintf(
                    '                 size = %s,', base_line_size),
                    '                 na.rm = TRUE) +',
                    '')
                } else if (is.character(ciunder)){
                  c(
                    '  # Plot CIs - before plotting points',
                    sprintf(
                      '  geom_linerange(data = ~ dplyr::filter(.x, !is.na(estimate_transformed) & %s),',
                      ciunder),
                    '                 aes(xmin = lci_transformed,',
                    '                     xmax = uci_transformed,',
                    sprintf(
                    '                     colour = %s),', cicolour[1]),
                    sprintf(
                    '                 size = %s,', base_line_size),
                    '                 na.rm = TRUE) +',
                    '')
                },
                '  # Plot points at the transformed estimates',
                '  ## Scale by inverse of the SE',
                sprintf(
                '  geom_point(data = ~ dplyr::filter(.x, estimate_transformed > %s, estimate_transformed < %s),',
                xfrom, xto),
                sprintf(
                '             aes(size = size, shape = %s, colour = %s, fill = %s),',
                shape, colour, fill),
                sprintf(
                '             stroke = %s,',
                stroke),
                '             na.rm = TRUE) +',
                '',
                '  # Scale the size of points by their side length',
                '  # and make the scale range from zero upwards',
                '  scale_radius(limits = c(0, 1),',
                sprintf(
                '               range = c(0, %s)) +', pointsize),
                '',
                if (isFALSE(ciunder) || is.null(ciunder)){
                  c(
                    '  # Plot the CIs',
                    '  geom_linerange(data = ~ dplyr::filter(.x, !is.na(estimate_transformed)),',
                    '                 aes(xmin = lci_transformed,',
                    '                     xmax = uci_transformed,',
                    sprintf(
                    '                     colour = %s),', cicolour[1]),
                    sprintf(
                    '                 size = %s,', base_line_size),
                    '                 na.rm = TRUE) +',
                    '')
                } else if (is.character(ciunder)){
                  c(
                    '  # Plot CIs - after plotting points',
                    sprintf(
                    '  geom_linerange(data = ~ dplyr::filter(.x, !is.na(estimate_transformed) & !%s),',
                    ciunder),
                    '                 aes(xmin = lci_transformed,',
                    '                     xmax = uci_transformed,',
                    sprintf(
                    '                     colour = %s),', cicolour[1]),
                    sprintf(
                    '                 size = %s,', base_line_size),
                    '                 na.rm = TRUE) +',
                    '')
                },
                '  # Add tiny segments with arrows when the CIs go outside axis limits',
                '  geom_segment(data = ~ dplyr::filter(.x, cioverright == TRUE),',
                '               aes(y=-row,',
                '                   x=uci_transformed-0.000001,',
                '                   yend=-row,',
                if (cicolouraes) {c(
                '                   xend=uci_transformed,',
                sprintf(
                '                   colour = %s),', cicolour[1]))
                } else {c(
                '                   xend=uci_transformed),',
                sprintf(
                '               colour = %s,', cicolour[1]))
                },
                sprintf(
                '               size = %s,', base_line_size),
                sprintf(
                '               arrow = arrow(type = "closed", length = unit(%s, "pt")),', 8 * base_line_size),
                '               na.rm = TRUE) +',
                '  geom_segment(data = ~ dplyr::filter(.x, cioverleft == TRUE),',
                '               aes(y=-row,',
                '                   x=lci_transformed+0.000001,',
                '                   yend=-row,',
                if (cicolouraes) {c(
                '                   xend=lci_transformed,',
                sprintf(
                '                   colour = %s),', cicolour[1]))
                } else {c(
                '                   xend=lci_transformed),',
                sprintf(
                '               colour = %s,', cicolour[1]))
                },
                sprintf(
                '               size = %s,', base_line_size),
                sprintf(
                '               arrow = arrow(type = "closed", length = unit(%s, "pt")),', 8 * base_line_size),
                '               na.rm = TRUE) +',
                '',
                plotdiamondscode,
                '  # Use identity for aesthetic scales',
                '  scale_shape_identity() +',
                '  scale_fill_identity() +',
                '  scale_colour_identity() +',
                '',
                '  # Flip x and y coordinates',
                '  coord_cartesian(clip = "off",',
                sprintf(
                '             xlim = c(%s, %s)) +', xfrom, xto),
                '',
                '  # Add columns to right side of plots',
                col.right.line,
                '',
                '  # Add columns to left side of plots',
                col.left.line,
                '',
                '  # Add xlab below each axis',
                sprintf(
                '  geom_text(aes(y = -Inf, x = %s, label = xlab),', xmid),
                '            hjust = 0.5,',
                sprintf(
                '            size  = %s,', base_size/(11/3)),
                '            vjust = 4.4,',
                '            fontface = "bold",',
                sprintf(
                '            data = dplyr::tibble(panel = factor(%s,',
                paste(deparse(panel.names), collapse = '')),
                sprintf(
                '                                                 levels = %s,',
                paste(deparse(panel.names), collapse = '')),
                '                                                 ordered = TRUE),',
                sprintf(
                '                                 xlab = %s)) +',
                paste(deparse(xlab), collapse = '')),
                '',
                '  # Add panel name above each panel',
                sprintf(
                '  geom_text(aes(y = %s, x = %s, label = title),',
                col.heading.space , xmid),
                '            hjust = 0.5,',
                '            nudge_y = 2,',
                sprintf(
                '            size  = %s,', base_size/(11/3)),
                '            fontface = "bold",',
                sprintf(
                '            data = dplyr::tibble(panel = factor(%s,',
                paste(deparse(panel.names), collapse = '')),
                sprintf(
                '                                                 levels = %s,',
                paste(deparse(panel.names), collapse = '')),
                '                                                 ordered = TRUE),',
                sprintf(
                '                                 title = %s)) +',
                paste(deparse(panel.headings), collapse = '')),
                '',
                '  # Set the scale for the y axis (the estimates and CIs)',
                sprintf('  scale_x_continuous(trans  = "%s",', scale),
                xticksline,
                '                     expand = c(0,0)) +',
                '',
                '  # Set the scale for the x axis (the rows)',
                '  scale_y_continuous(breaks = -1:-max(datatoplot$row),',
                '                     labels = rowlabels,',
                '                     expand = c(0,0)) +',
                if (title != ""){c(
                '',
                '  # Add the title',
                sprintf('  labs(title = "%s") +', title))
                },
                '',
                '  # Control the overall look of the plots',
                sprintf('  theme(text             = element_text(size = %s),', base_size),
                sprintf('        line             = element_line(size = %s),', base_line_size),
                '        panel.background = element_blank(),',
                '        panel.grid.major = element_blank(),',
                '        panel.grid.minor = element_blank(),',
                if (title == ""){
                '        plot.title       = element_blank(),'
                } else {
                '        plot.title.position = "plot",'
                },
                sprintf(
                '        axis.line.x      = element_line(size = %s, lineend = "round"),', base_line_size),
                '        axis.title       = element_blank(),',
                '        axis.ticks.x     = element_line(colour = "black"),',
                '        axis.text.x      = element_text(colour = "black",',
                sprintf(
                '                                        margin = margin(t = %s),', base_size/(11/4.4)),
                '                                        vjust  = 1),',
                '        axis.ticks.y     = element_blank(),',
                '        axis.line.y      = element_blank(),',
                '        axis.text.y      = element_text(hjust  = 0,',
                '                                        colour = "black",',
                '                                        face   = boldlabels,',
                sprintf('                                        margin = margin(r = %s, unit = "%s")),', label.space, units),
                '        panel.border     = element_blank(),',
                sprintf('        panel.spacing    = unit(%s, "%s"),', panel.space, units),
                '        strip.background = element_blank(),',
                '        strip.placement  = "outside",',
                '        strip.text       = element_blank(),',
                '        legend.position  = "none",',
                '        plot.background  = element_blank(),',
                sprintf(
                '        plot.margin      = unit(%s, "%s"))',
                paste(deparse(margin), collapse = ''), units))


  # Write the ggplot2 code to a file in temp directory, and show in RStudio viewer.
  if (showcode){
    writeLines(paste(c('# ggplot2 code ------------------',
                       '',
                       plotcode),
                     collapse = "\n"),
               file.path(tempdir(), "plotcode.txt"))
    viewer <- getOption("viewer", default = function(url){})
    viewer(file.path(tempdir(), "plotcode.txt"))
  }

  # Create plot and print
  plot <- eval(parse(text = plotcode), envir = envir)
  if (printplot){
    print(plot)
  }

  return(list(plot = plot,
              data = plot$data,
              code = plotcode) )
}










#' Fix panel width of a forest plot
#'
#' \code{fix_panel_width} fixes the panel width of a forest plot
#'
#' @param plot A plot (created by make_forest_plot()).
#' @param width Width of panels.
#' @param units Units (Default: mm).
#'
#' @return A gtable object
#'
#' @import ggplot2
#' @export


fix_panel_width <- function(plot, width, units = "mm"){
  gtable <- ggplot2::ggplotGrob(plot)
  gtable$widths[gtable$layout$l[grepl("panel", gtable$layout$name)]] <- unit(x = width, units = units)
  gtable
}
