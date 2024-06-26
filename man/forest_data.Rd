% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forest-data.R
\name{forest_data}
\alias{forest_data}
\title{Prepares data set for a forest plot}
\usage{
forest_data(
  panels,
  panel.names = NULL,
  col.key = "key",
  col.estimate = "estimate",
  col.stderr = "stderr",
  col.lci = NULL,
  col.uci = NULL,
  col.left = NULL,
  col.right = NULL,
  col.keep = NULL,
  row.labels = NULL,
  row.labels.levels = NULL,
  rows = NULL,
  row.labels.space = c(0, 1, 0, 0),
  ci.delim = ", ",
  digits = 2,
  exponentiate = TRUE,
  scalepoints = FALSE,
  minse = NULL,
  addtext = NULL,
  diamond = NULL,
  col.diamond = NULL,
  bold.labels = NULL
)
}
\arguments{
\item{panels}{A list of data frames. These should include columns or point
estimates, and standard errors or confidence interval limits. If you
specify a row.labels data frame, then they must also all contain a key column
with the same name (which can be specified by col.key).}

\item{panel.names}{A character vector. The names to be used for each forest plot panel.
If none provided, then they will be numbered 1, 2, 3 ...}

\item{col.key}{Name of column that links the results given in each data frame
provided in panels and the labels given in row.labels.
If row.labels data frame is not given, then this column will be used as row labels.
(Default: "key")}

\item{col.estimate, col.stderr, col.lci, col.uci}{Names of columns for: point estimates, standard errors, lower and upper limits of confidence intervals.}

\item{col.left, col.right}{Names of columns to be printed to the left/right of the plot.}

\item{col.keep}{Names of additional columns to be kept in returned data frame.}

\item{row.labels}{A data frame that contains the labels to be used for the
rows of the plot. Use NA if a lower level heading is not required for a given row.}

\item{row.labels.levels}{A character vector. The names of columns in row.labels
to use as headings/subheadings/labels for labelling rows.}

\item{rows}{If set, then only rows matching these labels (at the first level) will be included.}

\item{row.labels.space}{A numeric vector specifying the space
after a row label heading, at the end of a row label heading 'section'. (Default: c(0, 1, 0, 0))}

\item{ci.delim}{Character string to separate lower and upper limits of confidence interval. (Default: ", ")}

\item{digits}{Number of digits after decimal point to show for estimates and confidence intervals. (Default: 2)}

\item{exponentiate}{Exponentiate estimates (and CIs) before plotting. (Default: TRUE)}

\item{scalepoints}{Should the points be scaled by inverse of the standard error? (Default: FALSE)}

\item{minse}{Minimum standard error to use when scaling point size. (Default will use minimum in the data.)}

\item{addtext}{A list of data frames. List must be the same length as panels.
Data frames should contain a column with the name specified in col.key,
and one or more of:
\enumerate{
\item a column named 'text' containing character strings
\item columns named 'het_dof', 'het_stat', and 'het_p' containing character strings
\item columns names 'trend_stat' and 'trend_p' containing character strings
}

The character strings, heterogeneity test, and trend test results will
be plotted in the column of estimates and CIs, below the row with the key
given in the col.key column.}

\item{diamond}{Alternative to col.diamond. A character vectors identify the rows
(using the key values) for which the estimate and CI should be plotted using a diamond.}

\item{col.diamond}{Plot estimates and CIs as diamonds. Name of a column of logical values.}

\item{bold.labels}{A character vector identifying row labels (using key values) which should additionally be bold. (Default: NULL)}
}
\value{
A dataset from which a forest plot can be generated.
}
\description{
Prepares data set for a forest plot
}
\keyword{internal}
