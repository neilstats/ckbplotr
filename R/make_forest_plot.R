
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
#' @param colnames A character vector. The titles to be used for each forest plot.
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
#' @param col.pval Currently does nothing.
#' @param col.left Name of additional column to be printed to the left of the plot.
#' @param col.right Name of additional column to be printed to the right of the plot.
#'   Currently this is not added to the plot, but will be in the data frame.
#' @param ci.delim Character string to separate lower and upper limits of
#'   confidence interval. (Default: ", ")
#' @param whiteci A list of character vectors. List must be the same length as cols.
#'   Identify the rows (using the key values) for which the CI should be plotted in white. (Default: NULL)
#' @param diamond A list of character vectors. List must be the same length as cols.
#'   Identify the rows (using the key values) for which the estimate and CI should be plotted using a diamond. (Default: NULL)
#' @param exponentiate Exponentiate estimates (and CIs) before plotting,
#'   use log scale on the axis, and add a line at null effect. (Default: TRUE)
#' @param blankrows A numeric vector of length 4 specifying the number of blank rows
#'   after a heading1, at the end of a heading1 'section', after
#'   a heading2, and at the end of a heading2 'section. (Default: c(1, 1, 0, 0))
#' @param scalepoints Should the points be scaled by inverse of the standard
#'   error? (Default: FALSE)
#' @param addtext A list of data frames. List must be the same length as cols.
#'   Data frames should contain a col.key column, and one or more of:
#'
#'   1. a column named 'text' containing character strings
#'
#'   2. columns named 'het_dof', 'het_stat', and 'het_p' containing character strings
#'
#'   3. columns names 'trend_stat' and 'trend_p' containing character strings
#'
#'   The character strings (for 'text'), heterogeneity or trend test results will
#'   be plotted to the right of each forest plot below the key specified in the
#'   col.key column.
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
  col.pval      = NULL,
  col.left      = NULL,
  col.right     = NULL,
  ci.delim      = ", ",
  whiteci       = NULL,
  diamond       = NULL,
  exponentiate  = TRUE,
  blankrows     = c(1, 1, 0, 0),
  scalepoints   = FALSE,
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
          !is.na(het_stat) ~ paste0("paste('\u2003Heterogeneity: ', chi[",
                                   het_dof,
                                   "]^2,'=",
                                   het_stat,
                                   " (p=",
                                   het_p,
                                   ")', sep='')"),
          !is.na(trend_stat) ~ paste0("paste('\u2003Trend: ', chi[1]^2,'=",
                                   trend_stat,
                                   " (p=",
                                   trend_p,
                                   ")', sep='')")
        )) %>%
        dplyr::select(key = !!rlang::sym(col.key),
                      extratext)

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
      dplyr::mutate(row = 1:n())

    # Add extra rows for addtext
    if (!is.null(addtext)) {
      for (k in 1:length(extrarowkeys)) {
        out <- out %>%
          dplyr::add_row(Heading = "", extrarowkey = paste0(extrarowkeys[[k]]),
                         .after = which(out$key == extrarowkeys[[k]]))
      }
    }
    out <- out %>%
      dplyr::mutate(row = 1:n())


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

    out <- tibble::tibble(Heading = "", key = "", extrarowkey = "")

    for (head1 in rows) {
      #    print(paste0("now working on: ", head1))
      l2headings <- headings %>%
        dplyr::filter(heading1 == head1) %>%
        dplyr::select(heading2, key) %>%
        dplyr::distinct(heading2, .keep_all = TRUE)

      if (is.na(l2headings[[1, "heading2"]])) {
        out <- tibble::add_row(out, Heading = head1, key = headings %>% dplyr::filter(heading1 == head1) %>% dplyr::pull(key))

        # Add extra row for addtext
        if (headings[[heading == head1]]$key %in% extrarowkeys) {
          out <- tibble::add_row(out,
                                 Heading = "",
                                 extrarowkey = paste0(headings[[headeading == head1]]$key))
        }
      }
      else{



        out <- tibble::add_row(out, Heading = head1)

        if (blankrows[[1]] > 0) {for (i in 1:blankrows[[1]]) { out <- tibble::add_row(out, Heading = "") }}



        for (head2 in 1:nrow(l2headings)) {
          #      print(paste0("now working on: ", l2headings[[head2, "heading2"]]))

          l3headings <- headings %>%
            dplyr::filter(heading1 == head1 & heading2 == l2headings[[head2, "heading2"]]) %>%
            dplyr::select(heading3, key)

          #     print(l3headings)

          if (is.na(l3headings[[1, "heading3"]])) {
            out <- tibble::add_row(out, Heading = l2headings[[head2, "heading2"]], key = l2headings[[head2, "key"]])

            # Add extra row for addtext
            if (l2headings[[head2, "key"]] %in% extrarowkeys) {
              out <- tibble::add_row(out,
                                     Heading = "",
                                     extrarowkey = l2headings[[head2, "key"]])
            }
          }
          else{
            out <- tibble::add_row(out, Heading = l2headings[[head2, "heading2"]])
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
    if (tail(out$Heading, 1) != "") {
      out <- out %>%
        tibble::add_row(Heading = "")
    }

    out <- out %>%
      dplyr::mutate(row = 1:n())
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
                      !!!rlang::syms(col.right))
    } else {
      cols[[i]] <- cols[[i]] %>%
        dplyr::select(key = !!rlang::sym(col.key),
                      !!!rlang::syms(col.left),
                      estimate = !!rlang::sym(col.estimate),
                      stderr   = !!rlang::sym(col.stderr),
                      !!!rlang::syms(col.right))

    }

    out1 <- merge(out, cols[[i]], by = "key", all.x = TRUE) %>%
      dplyr::mutate(column = colnames[[i]],
                    linecolour = dplyr::if_else(key %in% whiteci[[i]], "white", "black"),
                    diamond = key %in% diamond[[i]])

    if (!is.null(addtext)){
      out1 <- merge(out1, addtext[[i]], by.x = "extrarowkey", by.y = "key", all.x = TRUE)
    } else {
      out1 <- dplyr::mutate(out1, extratext = as.character(NA))
    }



    datatoplot <- dplyr::bind_rows(datatoplot, out1) %>%
      dplyr::mutate(bold = dplyr::if_else(is.na(estimate), "bold", "plain"))
  }



  if (exponentiate == TRUE) {
    tf       <- exp
    inv_tf   <- log
    scale    <- "log"
  } else {
    tf       <- identity
    inv_tf   <- identity
    scale    <- "identity"
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
    minse <- min((datatoplot$estimate - datatoplot$lci)/1.96, na.rm = TRUE)
    datatoplot$size <- 1.96*minse/(datatoplot$estimate - datatoplot$lci)
  } else {
    datatoplot <- datatoplot %>%
      dplyr::mutate(estimate_transformed = tf(estimate),
                    lci_transformed = tf(estimate - 1.96*stderr),
                    uci_transformed = tf(estimate + 1.96*stderr)
      )
    minse <- min(datatoplot$stderr, na.rm = TRUE)
    datatoplot$size <- minse/datatoplot$stderr
  }

  datatoplot <- datatoplot %>%
    dplyr::mutate(textresult = dplyr::case_when(
      !is.na(estimate) ~ paste0("'\u2003",format(round(estimate_transformed, 2), nsmall = 2),
                                " (",
                                format(round(lci_transformed, 2), nsmall = 2),
                                ci.delim,
                                format(round(uci_transformed, 2), nsmall = 2),
                                ")'"),
      !is.na(extratext) ~ extratext,
      TRUE              ~ "''"))


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
#' @param title Title to appear at the top of the plot.
#' @param xlab Label to appear below the x-axis and above the text showing
#'             estimates and CIs. (Default: "HR (95\% CI)")
#' @param xlim A numeric vector. The limits of the x axis.
#' @param xticks A numeric vector. The tick points of the x axis.
#' @param pointsize The (largest) size of box to use for plotting point
#'                  estimates. (Default: 3)
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
  colnames      = NULL,
  col.key       = "key",
  col.estimate  = "estimate",
  col.stderr    = "stderr",
  col.lci       = NULL,
  col.uci       = NULL,
  col.left      = NULL,
  col.right     = NULL,
  col.pval      = NULL,
  ci.delim      = ", ",
  title         = "",
  xlab          = "HR (95% CI)",
  xlim          = NULL,
  xticks        = NULL,
  blankrows     = c(1, 1, 0, 0),
  whiteci       = NULL,
  diamond       = NULL,
  scalepoints   = FALSE,
  pointsize     = 3,
  addtext       = NULL
){


  if (exponentiate == TRUE) {
    tf       <- exp
    inv_tf   <- log
    scale    <- "log"
    nullline <- "geom_segment(aes(x=-1, xend=-Inf, y=1, yend=1)) +"
  } else {
    tf       <- identity
    inv_tf   <- identity
    scale    <- "identity"
    nullline <- ""
  }


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
    col.pval      = col.pval,
    col.left      = col.left,
    col.right     = col.right,
    ci.delim      = ci.delim,
    whiteci       = whiteci,
    diamond       = diamond,
    exponentiate  = exponentiate,
    blankrows     = blankrows,
    scalepoints   = scalepoints,
    addtext       = addtext
  )


  if (is.null(xlim)) {
    xfrom <- floor(min(tf(datatoplot$estimate - 1.96*datatoplot$stderr), na.rm = TRUE)*10)/10
    xto   <- ceiling(max(tf(datatoplot$estimate + 1.96*datatoplot$stderr), na.rm = TRUE)*10)/10
  } else {
    xfrom <- min(xlim)
    xto   <- max(xlim)
  }

  xmid  <- tf((inv_tf(xfrom) + inv_tf(xto)) / 2)

  ## check if any cis are outside limits of x-axis
  datatoplot <- datatoplot %>%
    dplyr::mutate(cioverright  = (uci_transformed > xto),
                  uci_transformed = pmin(uci_transformed, xto),
                  cioverleft  = (lci_transformed < xfrom),
                  lci_transformed = pmax(lci_transformed, xfrom))


  if (is.null(xticks)) {
    xticksline <- ""
  } else {
    xticksline <- paste0("breaks = ",deparse(xticks),",")
  }


  if (is.null(col.left)) {
    col.left.line <- ""
  } else {
    col.left.line <- paste0('geom_text(aes(x = -row, y = ', xfrom,', label = `',col.left,'`),
            hjust = 1,
            size = 3,
            na.rm = TRUE) +
  annotate(geom = "text",
           x = 0, y = ', xfrom,',
           label = "',col.left,'",
           hjust = 1,
           size  = 3) +')
  }


  plotcode <- paste0('

# Get a character vector of the headings, so these can be used in the plot
headings <- datatoplot %>%
              dplyr::group_by(row) %>%
              dplyr::summarise(Heading = dplyr::first(Heading)) %>%
              dplyr::arrange(row) %>%
              dplyr::pull(Heading)

# Get a character vector of the style for headings
boldheadings <- datatoplot %>%
                  dplyr::group_by(row) %>%
                  dplyr::summarise(bold = dplyr::first(bold)) %>%
                  dplyr::arrange(row) %>%
                  dplyr::pull(bold)

# Create data frame for diamonds to be plotted
diamonds <- datatoplot %>%
  dplyr::filter(diamond == TRUE) %>%
  dplyr::mutate(y1 = lci_transformed,
                y2 = estimate_transformed,
                y3 = uci_transformed,
                y4 = estimate_transformed) %>%
  tidyr::gather(part, y, y1:y4) %>%
  dplyr::arrange(column, part) %>%
  dplyr::mutate(x = - row + rep(c(0, -0.25, 0, 0.25), times = sum(datatoplot$diamond)))

# Remove plotting of points if a diamond is to be used
if (any(datatoplot$diamond)) {
  datatoplot[datatoplot$diamond,]$estimate_transformed <- NA
  datatoplot[datatoplot$diamond,]$lci_transformed <- NA
  datatoplot[datatoplot$diamond,]$uci_transformed <- NA
}


# Create the ggplot
ggplot(datatoplot, aes(x=-row, y=estimate_transformed)) +

  # Put the different columns in side-by-side plots using facets
  facet_wrap(~column) +

  # Add a line at null effect (only if exponentiate=TRUE)
  ', nullline ,'

  # Plot points at the transformed estimates as squares
  ## Scale squares by inverse of the SE
  geom_point(aes(size = size),
             shape = 15,
             na.rm = TRUE) +

  # Scale the size of squares by their side length
  # and make the scale range from zero upwards
  scale_radius(limits = c(0, NA),
               range = c(0, ', pointsize,')) +

  # Plot CIs
  geom_linerange(aes(ymin = lci_transformed, ymax = uci_transformed, colour = linecolour),
                 na.rm = TRUE) +
  scale_colour_identity() +

  # Add tiny segments with arrows when the CIs go outside axis limits
  geom_segment(data = datatoplot %>% dplyr::filter(cioverright == TRUE),
               aes(x=-row, y=uci_transformed-0.000001, xend=-row, yend=uci_transformed),
               arrow = arrow(type = "closed", length = unit(6, "pt"))) +
  geom_segment(data = datatoplot %>% dplyr::filter(cioverleft == TRUE),
               aes(x=-row, y=lci_transformed+0.000001, xend=-row, yend=lci_transformed),
               arrow = arrow(type = "closed", length = unit(6, "pt"))) +

  # Add diamonds
  geom_polygon(data = diamonds, aes(x = x, y = y), colour="black", fill = "white") +

  # Flip x and y coordinates
  coord_flip(clip = "off") +

  # Plot estimates and confidence intervals (textresult) on the right edge of each forest plot
  ## An Em Space ("\u2003") has been put at the start of these strings, adding a small gap between the plot and this text
  ## You could also set hjust as negative to move the text right
  geom_text(aes(x = -row, y = Inf, label = textresult),
            hjust = 0,
            vjust = 0.5,
            size = 3,
            parse = TRUE) +

  # Add column of text to left side of plots
', col.left.line, '

  # Add xlab above the estimates and CIs on the right of each plot
  annotate(geom = "text",
           x = 0, y = Inf,
           label = paste0("\u2003", "',xlab,'"),
           hjust = 0,
           size  = 3) +

  # Add xlab below each axis
  annotate(geom = "text",
           x = -Inf, y = ',xmid,',
           label = "',xlab,'",
           hjust = 0.5,
           size  = 3,
           vjust = 4) +

  # Set the scale for the y axis (the estimates and CIs)
  scale_y_continuous(trans  = "',scale,'",
                     limits = c(',xfrom,',',xto,'),
                     ',xticksline,'
                     expand = c(0,0),
                     name   = "',xlab,'") +

  # Set the scale for the x axis (the rows)
  scale_x_continuous(breaks = -1:-max(datatoplot$row),
                     labels = headings,
                     name   = "",
                     expand = c(0,0)) +

  # Add the title
  labs(title = "', title,'") +

  # Control the overall looks of the plots
  theme(panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x      = element_line(size = 0.5),
        axis.title.x     = element_blank(),
        axis.ticks.y     = element_blank(),
        axis.text.y      = element_text(hjust  = 0,
                                        size   = 8,
                                        colour = "black",
                                        face   = boldheadings,
                                        margin = margin(r = 25)),
        panel.border     = element_blank(),
        panel.spacing    = unit(8, "lines"),
        strip.background = element_blank(),
        strip.placement  = "outside",
        strip.text       = element_text(face = "bold"),
        legend.position  = "none",
        plot.margin      = unit(c(2,6,2,0), "lines"))
')

  # Write the ggplot2 code to a file in temp directory, and show in RStudio viewer.
  writeLines(paste("# ggplot2 code ------------------",
                   "# Assign the data returned by the function to 'datatoplot' \ndatatoplot <- plot$data",
                   plotcode,
                   sep = "\n\n"),
             file.path(tempdir(), "plotcode.txt"))
  viewer <- getOption("viewer", default = function(url){})
  viewer(file.path(tempdir(), "plotcode.txt"))

  plot <- eval(parse(text = plotcode))
  print(plot)

  return(list(plot = plot,
              data = datatoplot,
              code = parse(text = plotcode)) )

}
