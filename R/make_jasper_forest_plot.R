#' Make forest plot with Jasper
#'
#' \code{make_jasper_forest_plot} creates a forest plot with Jasper
#'
#' This function requires the in-house Jasper package.
#'
#' The function creates a suitable csv, then runs the Jasper::ForestFromCSV
#' to create a forest plot.
#'
#' @inheritParams make_forest_data
#' @param filestem String to use for file names. (Default: "_test")
#' @param xlab Label to appear below the x-axis and above the text showing
#'             estimates and CIs. (Default: "HR (95\% CI)")
#' @param forest.xlim A numeric vector. The limits of the x axis.
#' @param forest.xticks A numeric vector. The tick points of the x axis.
#' @param col.right Name of additional column to be printed to the right of the plot.
#' @param col.pval Name of additional column to be printed to the right of the plot,
#'   and formatted as P value.
#' @param ... Other parameters will be passed to the Jasper::ForestFromCSV function.
#'
#' @export

make_jasper_forest_plot <- function(
  filestem      = "_test",
  headings      = NULL,
  rows          = NULL,
  cols,
  exponentiate  = TRUE,
  col.key       = "key",
  col.estimate  = "estimate",
  col.stderr    = "stderr",
  col.lci       = NULL,
  col.uci       = NULL,
  col.left      = NULL,
  col.right     = NULL,
  col.pval      = NULL,
  xlab          = "HR (95% CI)",
  forest.xlim   = NULL,
  forest.xticks = NULL,
  blankrows     = c(1, 1, 0, 0),
  ...
){

  if (!requireNamespace("Jasper", quietly = TRUE)) {
    stop("Package \"Jasper\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  out <- make_forest_data(
    headings      = headings,
    rows          = rows,
    cols          = cols,
    col.key  = col.key,
    col.estimate  = col.estimate,
    col.stderr    = col.stderr,
    col.lci       = col.lci,
    col.uci       = col.uci,
    col.pval      = col.pval,
    col.left      = col.left,
    col.right     = col.right,
    exponentiate  = exponentiate,
    blankrows     = blankrows) %>%
    dplyr::group_by(row) %>%
    dplyr::summarise(Heading = dplyr::first(Heading),
                     key = dplyr::first(key))

  col.right <- c(col.pval, col.right)

  tres <- list()
  for (x in 1:length(cols)) {
    tres[[x]] <- cols[[x]]

    if (!is.null(col.lci)) {
      tres[[x]] <- tres[[x]] %>%
        dplyr::select(key = !!rlang::sym(col.key),
                      !!!rlang::syms(col.left),
                      Estimate = !!rlang::sym(col.estimate),
                      LCI      = !!rlang::sym(col.lci),
                      UCI      = !!rlang::sym(col.uci),
                      !!!rlang::syms(col.right))
    } else {
      tres[[x]] <- tres[[x]] %>%
        dplyr::select(key = !!rlang::sym(col.key),
                      !!!rlang::syms(col.left),
                      Estimate = !!rlang::sym(col.estimate),
                      StdErr   = !!rlang::sym(col.stderr),
                      !!!rlang::syms(col.right))
    }

    names(tres[[x]])[-1] <- paste0(names(tres[[x]])[-1], x)

    tres[[x]] <- tres[[x]] %>%
      dplyr::filter(!is.na(key))
  }


  res <- purrr::reduce(tres, function(x, y) merge(x, y, by = "key", all = TRUE))

  out2 <- merge(out, res, by = "key", all.x = TRUE) %>%
    dplyr::arrange(row) %>%
    dplyr::select(-row, -key)

  readr::write_csv(out2, file.path(getwd(), paste0(filestem, "_csv_for_jasper_forest.csv")))

  Jasper::ForestFromCSV(
    file                   = file.path(getwd(), paste0(filestem, "_csv_for_jasper_forest.csv")),
    filestem               = filestem,
    ExponentiateDataOnPlot = exponentiate,
    LogScale               = exponentiate,
    StopIfCodeExists       = FALSE,
    suppress.date          = TRUE,
    forest.xlim            = forest.xlim,
    forest.xticks          = forest.xticks,
    xlab                   = xlab,
    ValueLabelsHeader      = xlab,
    FormatPValue           = col.pval,
    ...
  )

}
