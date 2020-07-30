
#' Prepares data set for a forest plot
#'
#' \code{make_forest_data}
#'
#' @param headings A data frame that contains the headings to be used for the
#'   rows of the plot. The data frame must contain columns 'heading1', 'heading2'
#'   and 'heading3'. Use NA if a lower level heading is not required.
#' @param rows A character vector. The top level headings (heading1) of rows
#'   to be included in the plot.
#' @param cols A list of data frames. These should include columns or point
#'   estimates, and standard errors or confidence interval limits. If you
#'   specify a headings data frame, then they must also all contain a key column
#'   with the same name (which can be specified by col.key).
#' @param colnames A character vector. The names to be used for each forest plot.
#'   If none provided, then they will be numbered 1, 2, 3 ...
#' @param col.key Name of column that links the headings provided in headings
#'   and the results given in each data frame provided in cols.
#'
#'   If headings data frame is not given, then this column will be used as headings
#'   for each row of the plot.
#'
#'   (Default: "key")
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
#' @param addtext A list of data frames. List must be the same length as cols.
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
#' @return A dataset from which the plot is generated.
#'
#'
#' @keywords internal
#'

make_forest_data <- function(
  headings     = NULL,
  rows         = NULL,
  cols,
  colnames      = NULL,
  col.key       = "key",
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

  if (!is.null(col.lci) &&  is.null(col.uci)) stop("col.lci and col.uci must both be specified")
  if ( is.null(col.lci) && !is.null(col.uci)) stop("col.lci and col.uci must both be specified")
  if (is.null(colnames)) { colnames <- as.character(1:length(cols)) }
  if (!is.character(colnames)) stop("colnames must be a character vector")
  if (!all(!duplicated(colnames))) stop("colnames must be unique")
  if (length(cols) != length(colnames)) stop("cols and colnames must be the same length")
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
                                    " (p=",
                                    het_p,
                                    ")', sep='')"),
          !is.na(trend_stat) ~ paste0("paste('Trend: ', chi[1]^2,'=",
                                      trend_stat,
                                      " (p=",
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

  if (is.null(headings)) {
    out <- cols[[1]] %>%
      dplyr::mutate(Heading = !!rlang::sym(col.key),
                    key = !!rlang::sym(col.key),
                    extrarowkey = "") %>%
      dplyr::select(Heading, key, extrarowkey) %>%
      dplyr::add_row(Heading = "") %>%
      dplyr::mutate(row = 1:dplyr::n())

    # Add extra rows for addtext
    if (!is.null(addtext)) {
      for (k in 1:length(extrarowkeys)) {
        out <- out %>%
          dplyr::add_row(Heading = "", extrarowkey = paste0(extrarowkeys[[k]]),
                         .after = which(out$key == extrarowkeys[[k]]))
      }
    }
    out <- out %>%
      dplyr::mutate(row = 1:dplyr::n())


  } else {

    if (is.null(rows)) stop("argument rows must be given if headings is used")
    if (!col.key %in% names(headings)) stop(paste0(col.key, " must be a column in ",  deparse(substitute(headings))))

    for (col in cols) {
      if (!col.key %in% names(col)) stop(paste0(col.key, " must be a column in every data frame given in cols"))
    }

    for (head1 in rows) {
      if (!(head1 %in% headings$heading1)) {
        stop(paste(head1,"is not in heading1 column of", deparse(substitute(headings))))
      }
    }

    headings <- headings %>%
      dplyr::mutate(heading1 = heading1,
                    heading2 = heading2,
                    heading3 = heading3,
                    key = !!rlang::sym(col.key))

    out <- tibble::tibble(Heading = "", key = "", extrarowkey = "", removelater = TRUE)

    for (head1 in rows) {
      #    print(paste0("now working on: ", head1))
      l2headings <- headings %>%
        dplyr::filter(heading1 == head1) %>%
        dplyr::select(heading2, key) %>%
        dplyr::distinct(heading2, .keep_all = TRUE)

      if (is.na(l2headings[[1, "heading2"]])) {
        if (head1 != "") {out <- tibble::add_row(out, Heading = head1, key = headings %>% dplyr::filter(heading1 == head1) %>% dplyr::pull(key))}

        # Add extra row for addtext
        if (headings[[which(headings$heading1 == head1),"key"]] %in% extrarowkeys) {
          out <- tibble::add_row(out,
                                 Heading = "",
                                 extrarowkey = headings[[which(headings$heading1 == head1), "key"]])
        }
      }
      else{



        if (head1 != "") {out <- tibble::add_row(out, Heading = head1)}

        if (blankrows[[1]] > 0) {for (i in 1:blankrows[[1]]) { out <- tibble::add_row(out, Heading = "") }}



        for (head2 in 1:nrow(l2headings)) {
          #      print(paste0("now working on: ", l2headings[[head2, "heading2"]]))

          l3headings <- headings %>%
            dplyr::filter(heading1 == head1 & heading2 == l2headings[[head2, "heading2"]]) %>%
            dplyr::select(heading3, key)

          #     print(l3headings)

          if (is.na(l3headings[[1, "heading3"]])) {
            if (head2 != "") {out <- tibble::add_row(out, Heading = l2headings[[head2, "heading2"]], key = l2headings[[head2, "key"]])}

            # Add extra row for addtext
            if (l2headings[[head2, "key"]] %in% extrarowkeys) {
              out <- tibble::add_row(out,
                                     Heading = "",
                                     extrarowkey = l2headings[[head2, "key"]])
            }
          }
          else{
            if (head2 != "") {out <- tibble::add_row(out, Heading = l2headings[[head2, "heading2"]])}
            if (blankrows[[3]] > 0) {for (i in 1:blankrows[[3]]) { out <- tibble::add_row(out, Heading = "") }}
            for (head3 in 1:nrow(l3headings)) {
              out <- tibble::add_row(out,
                                     Heading = l3headings[[head3, "heading3"]],
                                     key = l3headings[[head3, "key"]])

              # Add extra row for addtext
              if (l3headings[[head3, "key"]] %in% extrarowkeys) {
                out <- tibble::add_row(out,
                                       Heading = "",
                                       extrarowkey = l3headings[[head3, "key"]])
              }
            }
          }
          if (blankrows[[4]] > 0) {for (i in 1:blankrows[[4]]) { out <- tibble::add_row(out, Heading = "") }}
        }
      }
      if (blankrows[[2]] > 0) {for (i in 1:blankrows[[2]]) { out <- tibble::add_row(out, Heading = "") }}
    }


    # add a blank heading at bottom if needed
    if (utils::tail(out$Heading, 1) != "") {
      out <- out %>%
        tibble::add_row(Heading = "")
    }

    out <- out %>%
      dplyr::filter(is.na(removelater) | !removelater) %>%
      dplyr::select(-removelater) %>%
      dplyr::mutate(row = 1:dplyr::n())
  }


  # make datatoplot
  datatoplot <- tibble::tibble()

  for (i in 1:length(cols)) {

    if (!is.null(col.lci)) {
      cols[[i]] <- cols[[i]] %>%
        dplyr::select(key = !!rlang::sym(col.key),
                      !!!rlang::syms(col.left),
                      estimate = !!rlang::sym(col.estimate),
                      lci      = !!rlang::sym(col.lci),
                      uci      = !!rlang::sym(col.uci),
                      !!!rlang::syms(col.right),
                      !!!rlang::syms(col.keep))
    } else {
      cols[[i]] <- cols[[i]] %>%
        dplyr::select(key = !!rlang::sym(col.key),
                      !!!rlang::syms(col.left),
                      estimate = !!rlang::sym(col.estimate),
                      stderr   = !!rlang::sym(col.stderr),
                      !!!rlang::syms(col.right),
                      !!!rlang::syms(col.keep))

    }

    out1 <- merge(out, cols[[i]], by = "key", all.x = TRUE) %>%
      dplyr::mutate(column = colnames[[i]])

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

  # Make 'column' a factor, so that facets will be in the correct order
  datatoplot <- datatoplot %>%
    dplyr::mutate(column = factor(column,
                                  levels = colnames,
                                  labels = colnames,
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
    dplyr::arrange(column, row)


  if (!scalepoints) {
    datatoplot$size <- 1
  }

  return(datatoplot)
}









































#' Make forest plot with ggplot2
#'
#' \code{make_forest_plot} creates a forest plot with ggplot
#'
#' The function returns the plot, a dataset which is used to create the plot,
#' and the ggplot2 code that creates the plot. In RStudio, the ggplot2 code
#' will be shown in the viewer.
#'
#'
#'
#' @inheritParams make_forest_data
#' @inheritParams theme_ckb
#' @param logscale Use log scale on the axis, and add a line at null effect. (Default: exponentiate)
#' @param colheadings Titles to be placed above each forest plot. (Default: colnames)
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
#' @param boldheadings A character vector identifying headings (using key values) which should additionally be bold. (Default: NULL)
#' @param heading.space Size of the gap between headings and the first plot.
#' Unit is "lines". (Default: 4)
#' @param plot.space Size of the gap between forest plots.
#' Unit is "lines". (Default: 8)
#' @param stroke Size of outline of shapes. (Default: base_size/22)
#' @param printplot Print the plot. (Default: TRUE)
#' @param showcode Show the ggplot2 code to generate the plot in RStudio 'Viewer' pane. (Default: TRUE)
#'
#' @return A list:
#' \describe{
#'   \item{plot}{the plot}
#'   \item{data}{a data frame from which the plot is generated}
#'   \item{code}{ggplot2 code to generate the plot}
#'}
#'
#' @import ggplot2
#' @export




make_forest_plot <- function(
  headings      = NULL,
  rows          = NULL,
  cols,
  exponentiate  = TRUE,
  logscale      = exponentiate,
  colnames      = NULL,
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
  heading.space = 4,
  plot.space    = 8,
  base_size     = 11,
  base_line_size = base_size/22,
  stroke        = base_size/22,
  printplot     = TRUE,
  showcode      = TRUE
){

  # check arguments
  if (!missing(col.diamond) &&  !missing(diamond)) stop("Use either col.diamond or diamond, not both.")

  # take first element if diamond is a list
  if (is.list(diamond)){ diamond <- diamond[[1]] }

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
    if (x %in% names(cols[[1]])){ col.keep <- append(col.keep, x) }
  }

  # make deault colnames
  if (is.null(colnames)) { colnames <- as.character(1:length(cols)) }

  datatoplot <- make_forest_data(
    headings      = headings,
    rows          = rows,
    cols          = cols,
    colnames      = colnames,
    col.key  = col.key,
    col.estimate  = col.estimate,
    col.stderr    = col.stderr,
    col.lci       = col.lci,
    col.uci       = col.uci,
    col.left      = col.left,
    col.right     = col.right,
    col.keep      = col.keep,
    ci.delim      = ci.delim,
    exponentiate  = exponentiate,
    blankrows     = blankrows,
    scalepoints   = scalepoints,
    minse         = minse,
    addtext       = addtext
  )

  # line at null
  nullline <- sprintf('  annotate(geom = "segment", x=-1, xend=-Inf, y=%s, yend=%s, size = %s) +',
                      nullval, nullval, base_line_size)

  # aesthetics: default value, match column name, or use argument itself
  if (is.null(shape)) {
    shape <- 15
  } else if (shape %in% names(cols[[1]])){
    shape <- paste0("`", shape, "`")
  }

  if (is.null(cicolour)) {
    cicolour <- '\"black\"'
  }
  else if (cicolour %in% names(cols[[1]])){
    cicolour <- paste0("`", cicolour, "`")
  } else {
    cicolour <- paste0('\"', cicolour, '\"')
  }

  if (is.null(colour)) {
    colour <- '\"black\"'
  } else if (colour %in% names(cols[[1]])){
    colour <- paste0("`", colour, "`")
  } else {
    colour <- paste0('\"', colour, '\"')
  }

  if (is.null(fill)) {
    fill <- '\"black\"'
  } else if (fill %in% names(cols[[1]])){
    fill <- paste0("`", fill, "`")
  } else {
    fill <- paste0('\"', fill, '\"')
  }

  if (is.null(col.bold)) { col.bold <- FALSE } else {col.bold <- paste0("`", col.bold, "`")}

  if (is.null(xlim)) {
    xlim <- range(pretty(c(datatoplot$lci_transformed, datatoplot$uci_transformed)))
    ## check for zero as axis limit when using exponential
    if (exponentiate & isTRUE(all.equal(0, xlim[[1]]))){
      xlim[[1]] <- min(c(datatoplot$lci_transformed, datatoplot$uci_transformed), na.rm = TRUE)
    }
  }

  xfrom <- min(xlim)
  xto   <- max(xlim)
  xmid  <- tf((inv_tf(xfrom) + inv_tf(xto)) / 2)

  ## check if any cis are outside limits of x-axis
  datatoplot <- datatoplot %>%
    dplyr::mutate(cioverright  = (uci_transformed > xto),
                  uci_transformed = pmin(uci_transformed, xto),
                  cioverleft  = (lci_transformed < xfrom),
                  lci_transformed = pmax(lci_transformed, xfrom))


  if (is.null(xticks)) {
    xticks <- pretty(c(xfrom, xto))
  }
  xticksline <- paste0("                     breaks = ",paste(deparse(xticks), collapse = ""),",")


  # columns to right of plots
  if (is.null(col.right) & !estcolumn) {
    col.right.line <- ""
  } else {

    if (estcolumn){
      col.right <- c("textresult", col.right)
    }

    col.right.line <- unlist(purrr::pmap(list(col.right, col.right.space, col.right.heading, col.right.hjust, col.bold),
                                         ~ c(sprintf('  ## column %s', ..1),
                                             sprintf('  geom_text(aes(x = -row, y = %s,',
                                                     tf(inv_tf(xto) + (inv_tf(xto) - inv_tf(xfrom)) * ..2)),
                                             if(is.character(..5)){
                                               sprintf('            label = dplyr::if_else(%s & !is.na(%s), paste0("bold(",`%s`,")"), %s)),',
                                                       ..5, ..5, ..1, ..1)
                                               } else {
                                                sprintf('            label = %s),', ..1)
                                                       },
                                             sprintf('            hjust = %s,', ..4),
                                             sprintf('            size = %s,', base_size/(11/3)),
                                             '            na.rm = TRUE,',
                                             '            parse = TRUE) +',
                                             '  annotate(geom = "text",',
                                             sprintf('           x = %s, y = %s,',
                                                     col.heading.space,
                                                     tf(inv_tf(xto) + (inv_tf(xto) - inv_tf(xfrom)) * ..2)),
                                             sprintf('           label = "%s",', ..3),
                                             sprintf('           hjust = %s,', ..4),
                                             sprintf('           size = %s,', base_size/(11/3)),
                                             '           fontface = "bold") +')))
  }




  # columns to left of plots
  if (is.null(col.left)) {
    col.left.line <- ""
  } else {
    col.left.line <- unlist(purrr::pmap(list(col.left, col.left.space, col.left.heading, col.left.hjust, col.bold),
                                        ~ c(sprintf('  ## column %s', ..1),
                                            sprintf(
                                            '  geom_text(aes(x = -row, y = %s,',
                                            tf(inv_tf(xfrom) - (inv_tf(xto) - inv_tf(xfrom)) * ..2)),
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
                                            sprintf('           hjust = %s,', ..4),
                                            sprintf('           size = %s,', base_size/(11/3)),
                                            '            na.rm = TRUE) +',
                                            '  annotate(geom = "text",',
                                            sprintf('           x = %s, y = %s,',
                                                    col.heading.space,
                                                    tf(inv_tf(xfrom) - (inv_tf(xto) - inv_tf(xfrom)) * ..2)),
                                            sprintf('           label = "%s",', ..3),
                                            sprintf('           hjust = %s,', ..4),
                                            sprintf('           size = %s,', base_size/(11/3)),
                                            '           fontface = "bold") +')))
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
        '  dplyr::mutate(y1 = lci_transformed,',
        '                y2 = estimate_transformed,',
        '                y3 = uci_transformed,',
        '                y4 = estimate_transformed) %>%',
        '  tidyr::gather(part, y, y1:y4) %>%',
        '  dplyr::arrange(column, row, part) %>%',
        '  dplyr::mutate(x = - row + c(0, -0.25, 0, 0.25))',
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
        '  dplyr::mutate(y1 = lci_transformed,',
        '                y2 = estimate_transformed,',
        '                y3 = uci_transformed,',
        '                y4 = estimate_transformed) %>%',
        '  tidyr::gather(part, y, y1:y4) %>%',
        '  dplyr::arrange(column, row, part) %>%',
        '  dplyr::mutate(x = - row + c(0, -0.25, 0, 0.25))',
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
      '                   colour = %s, fill = %s),', cicolour, fill),
      sprintf(
      '               size = %s) +', base_line_size),
      ''
    )
  }

  plotcode <- c('# Get a character vector of the headings, so these can be used in the plot',
                'headings <- datatoplot %>%',
                '              dplyr::group_by(row) %>%',
                '              dplyr::summarise(Heading = dplyr::first(Heading)) %>%',
                '              dplyr::arrange(row) %>%',
                '              dplyr::pull(Heading)',
                '',
                '# Get a character vector of the style for headings',
                'boldheadings <- datatoplot %>%',
                '                  dplyr::group_by(row) %>%',
                sprintf(
                '                  dplyr::summarise(bold = dplyr::if_else(all(is.na(estimate_transformed) | all(key %%in%% %s)), "bold", "plain")) %%>%%',
                paste(deparse(boldheadings))),
                '                  dplyr::arrange(row) %>%',
                '                  dplyr::pull(bold)',
                '',
                diamondscode,
                '# Create the ggplot',
                'ggplot(datatoplot, aes(x=-row, y=estimate_transformed)) +',
                '',
                '  # Put the different columns in side-by-side plots using facets',
                '  facet_wrap(~column, nrow = 1) +',
                '',
                '  # Add a line at null effect',
                nullline,
                '',
                if (isTRUE(ciunder)){
                  c(
                    '  # Plot the CIs',
                    '  geom_linerange(data = ~ dplyr::filter(.x, !is.na(estimate_transformed)),',
                    '                 aes(ymin = lci_transformed,',
                    '                     ymax = uci_transformed,',
                    sprintf(
                    '                     colour = %s), ', cicolour),
                    sprintf(
                    '                 size = %s,', base_line_size),
                    '                 na.rm = TRUE) +',
                    '')
                } else if (is.character(ciunder) && any(datatoplot[[ciunder]], na.rm = TRUE)){
                  c(
                    '  # Plot CIs - before plotting points',
                    sprintf(
                      '  geom_linerange(data = ~ dplyr::filter(.x, !is.na(estimate_transformed) & %s),',
                      ciunder),
                    '                 aes(ymin = lci_transformed,',
                    '                     ymax = uci_transformed,',
                    sprintf(
                    '                     colour = %s),', cicolour),
                    sprintf(
                    '                 size = %s,', base_line_size),
                    '                 na.rm = TRUE) +',
                    '')
                },
                '  # Plot points at the transformed estimates',
                '  ## Scale by inverse of the SE',
                sprintf(
                '  geom_point(aes(size = size, shape = %s, colour = %s, fill = %s),',
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
                    '                 aes(ymin = lci_transformed,',
                    '                     ymax = uci_transformed,',
                    sprintf(
                    '                     colour = %s), ', cicolour),
                    sprintf(
                    '                 size = %s,', base_line_size),
                    '                 na.rm = TRUE) +',
                    '')
                } else if (is.character(ciunder) && !all(datatoplot[[ciunder]], na.rm = TRUE)){
                  c(
                    '  # Plot CIs - after plotting points',
                    sprintf(
                    '  geom_linerange(data = ~ dplyr::filter(.x, !is.na(estimate_transformed) & !%s),',
                    ciunder),
                    '                 aes(ymin = lci_transformed,',
                    '                     ymax = uci_transformed,',
                    sprintf(
                    '                     colour = %s),', cicolour),
                    sprintf(
                    '                 size = %s,', base_line_size),
                    '                 na.rm = TRUE) +',
                    '')
                },
                '  # Add tiny segments with arrows when the CIs go outside axis limits',
                if(any(datatoplot$cioverright, na.rm = TRUE)){c(
                '  geom_segment(data = ~ dplyr::filter(.x, cioverright == TRUE),',
                '               aes(x=-row, y=uci_transformed-0.000001, xend=-row, yend=uci_transformed,',
                sprintf(
                '                   colour = %s),', cicolour),
                sprintf(
                '               size = %s,', base_line_size),
                sprintf(
                '               arrow = arrow(type = "closed", length = unit(%s, "pt")),', 8 * base_line_size),
                '               na.rm = TRUE) +')},
                if(any(datatoplot$cioverleft, na.rm = TRUE)){c(
                '  geom_segment(data = ~ dplyr::filter(.x, cioverleft == TRUE),',
                '               aes(x=-row, y=lci_transformed+0.000001, xend=-row, yend=lci_transformed,',
                sprintf(
                '                   colour = %s),', cicolour),
                sprintf(
                '               size = %s,', base_line_size),
                sprintf(
                '               arrow = arrow(type = "closed", length = unit(%s, "pt")),', 8 * base_line_size),
                '               na.rm = TRUE) +')},
                '',
                plotdiamondscode,
                '  # Use identity for aesthetic scales',
                '  scale_shape_identity() +',
                '  scale_fill_identity() +',
                '  scale_colour_identity() +',
                '',
                '  # Flip x and y coordinates',
                '  coord_flip(clip = "off",',
                sprintf(
                '             ylim = c(%s, %s)) +', xfrom, xto),
                '',
                '  # Add columns to right side of plots',
                col.right.line,
                '',
                '  # Add columns to left side of plots',
                col.left.line,
                '',
                '  # Add xlab below each axis',
                sprintf(
                '  geom_text(aes(x = -Inf, y = %s, label = xlab),', xmid),
                '            hjust = 0.5,',
                sprintf(
                '            size  = %s,', base_size/(11/3)),
                '            vjust = 4.4,',
                '            fontface = "bold",',
                sprintf(
                '            data = dplyr::tibble(column = factor(%s,',
                paste(deparse(colnames), collapse = '')),
                sprintf(
                '                                                 levels = %s,',
                paste(deparse(colnames), collapse = '')),
                '                                                 ordered = TRUE),',
                sprintf(
                '                                 xlab = %s)) +',
                paste(deparse(xlab), collapse = '')),
                '',
                '  # Add column name above each column',
                sprintf(
                '  geom_text(aes(x = %s, y = %s, label = title),',
                col.heading.space , xmid),
                '            hjust = 0.5,',
                '            nudge_x = 2,',
                sprintf(
                '            size  = %s,', base_size/(11/3)),
                '            fontface = "bold",',
                sprintf(
                '            data = dplyr::tibble(column = factor(%s,',
                paste(deparse(colnames), collapse = '')),
                sprintf(
                '                                                 levels = %s,',
                paste(deparse(colnames), collapse = '')),
                '                                                 ordered = TRUE),',
                sprintf(
                '                                 title = %s)) +',
                paste(deparse(colheadings), collapse = '')),
                '',
                '  # Set the scale for the y axis (the estimates and CIs)',
                sprintf('  scale_y_continuous(trans  = "%s",', scale),
                xticksline,
                '                     expand = c(0,0)) +',
                '',
                '  # Set the scale for the x axis (the rows)',
                '  scale_x_continuous(breaks = -1:-max(datatoplot$row),',
                '                     labels = headings,',
                '                     name   = "",',
                '                     expand = c(0,0)) +',
                '  # Add the title',
                sprintf('  labs(title = "%s") +', title),
                '',
                '  # Control the overall looks of the plots',
                sprintf('  theme(text             = element_text(size = %s),', base_size),
                sprintf('        line             = element_line(size = %s),', base_line_size),
                '        panel.background = element_rect(fill = "white", colour = NA),',
                '        panel.grid.major = element_blank(),',
                '        panel.grid.minor = element_blank(),',
                sprintf(
                '        axis.line.x      = element_line(size = %s, lineend = "round"),', base_line_size),
                '        axis.title.x     = element_blank(),',
                '        axis.ticks.x     = element_line(colour = "black"),',
                '        axis.text.x      = element_text(colour = "black",',
                sprintf(
                '                                        margin = margin(t = %s),', base_size/(11/4.4)),
                '                                        vjust  = 1),',
                '        axis.ticks.y     = element_blank(),',
                '        axis.line.y      = element_blank(),',
                '        axis.text.y      = element_text(hjust  = 0,',
                '                                        colour = "black",',
                '                                        face   = boldheadings,',
                sprintf('                                        margin = margin(r = %s, unit = "lines")),', heading.space),
                '        panel.border     = element_blank(),',
                sprintf('        panel.spacing    = unit(%s, "lines"),', plot.space),
                '        strip.background = element_blank(),',
                '        strip.placement  = "outside",',
                '        strip.text       = element_blank(),',
                '        legend.position  = "none",',
                '        plot.margin      = unit(c(2,6,2,0), "lines"))')


  # Write the ggplot2 code to a file in temp directory, and show in RStudio viewer.
  if (showcode){
    writeLines(paste(c('# ggplot2 code ------------------',
                       '# Assign the data returned by the function to datatoplot',
                       'datatoplot <- plot$data',
                       '',
                       plotcode),
                     collapse = "\n"),
               file.path(tempdir(), "plotcode.txt"))
    viewer <- getOption("viewer", default = function(url){})
    viewer(file.path(tempdir(), "plotcode.txt"))
  }


  # Make copy of datatoplot before running plot code
  datatoplot_clean <- datatoplot

  # Create plot and print
  plot <- eval(parse(text = plotcode))
  if (printplot){
    print(plot)
  }

  return(list(plot = plot,
              data = datatoplot_clean,
              code = plotcode) )

}
