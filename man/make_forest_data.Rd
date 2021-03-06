% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_forest_plot.R
\name{make_forest_data}
\alias{make_forest_data}
\title{Prepares data set for a forest plot}
\usage{
make_forest_data(
  panels,
  col.key = "key",
  row.labels = NULL,
  row.labels.levels = c("heading1", "heading2", "heading3"),
  rows = NULL,
  panel.names = NULL,
  col.estimate = "estimate",
  col.stderr = "stderr",
  col.lci = NULL,
  col.uci = NULL,
  col.left = NULL,
  col.right = NULL,
  col.keep = NULL,
  ci.delim = ", ",
  exponentiate = TRUE,
  blankrows = c(1, 1, 0, 0),
  scalepoints = FALSE,
  minse = NULL,
  addtext = NULL,
  cols = panels,
  headings = NULL,
  colnames = NULL
)
}
\arguments{
\item{panels}{A list of data frames. These should include columns or point
estimates, and standard errors or confidence interval limits. If you
specify a row.labels data frame, then they must also all contain a key column
with the same name (which can be specified by col.key).}

\item{col.key}{Name of column that links the results given in each data frame
  provided in panels and the labels given in row.labels.

  If row.labels data frame is not given, then this column will be used as row labels.

  (Default: "key")}

\item{row.labels}{A data frame that contains the labels to be used for the
rows of the plot. Use NA if a lower level heading is not required for a given row.}

\item{row.labels.levels}{A character vector up to length 3. The names of columns in row.labels
to use as headings/subheadings/labels for labelling rows. (Default: c("heading1", "heading2", "heading3"))}

\item{rows}{A character vector. The top level labels of rows
to be included in the plot.}

\item{panel.names}{A character vector. The names to be used for each forest plot panel.
If none provided, then they will be numbered 1, 2, 3 ...}

\item{col.estimate}{Name of column that provides point estimates.
(Default: "estimate")}

\item{col.stderr}{Name of column that provides standard errors. (Default: "stderr")}

\item{col.lci}{Name of column that provides lower limit of confidence intervals.}

\item{col.uci}{Name of column that provides upper limit of confidence intervals.}

\item{col.left}{Names of columns to be printed to the left of the plot.}

\item{col.right}{Names of columns to be printed to the right of the plot.}

\item{col.keep}{Names of additional columns to be kept in returned data frame.}

\item{ci.delim}{Character string to separate lower and upper limits of
confidence interval. (Default: ", ")}

\item{exponentiate}{Exponentiate estimates (and CIs) before plotting. (Default: TRUE)}

\item{blankrows}{A numeric vector of length 4 specifying the number of blank rows
after a heading1, at the end of a heading1 'section', after
a heading2, and at the end of a heading2 'section. (Default: c(1, 1, 0, 0))}

\item{scalepoints}{Should the points be scaled by inverse of the standard
error? (Default: FALSE)}

\item{minse}{Minimum standard error to use when scaling point size. (Default will use minimum in the data.)}

\item{addtext}{A list of data frames. List must be the same length as panels.
  Data frames should contain a column with the name specified in col.key,
  and one or more of:

  1. a column named 'text' containing character strings

  2. columns named 'het_dof', 'het_stat', and 'het_p' containing character strings

  3. columns names 'trend_stat' and 'trend_p' containing character strings

  The character strings, heterogeneity test, and trend test results will
  be plotted in the column of estimates and CIs, below the row with the key
  given in the col.key column.}

\item{cols}{DEPRECATED.}

\item{headings}{DEPRECATED.}

\item{colnames}{DEPRECATED.}
}
\value{
A dataset from which a forest plot can be generated.
}
\description{
\code{make_forest_data}
}
\keyword{internal}
