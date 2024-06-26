% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save-figure.R
\name{save_figure}
\alias{save_figure}
\title{Output plots as files}
\usage{
save_figure(
  figure,
  filename,
  cropped = NULL,
  args = NULL,
  args_cropped = NULL,
  preview = FALSE,
  ...
)
}
\arguments{
\item{figure}{Plot (or graphical object).}

\item{filename}{Name of file to create.}

\item{cropped}{Name of second output file of the figure without margins or title.}

\item{args}{List of arguments passed to \code{ggplot2::ggsave()} for the main figure.}

\item{args_cropped}{List of arguments passed to \code{ggplot2::ggsave()} for the cropped figure.}

\item{preview}{Preview the output in the RStudio Viewer pane. (Default: False)}

\item{...}{Other arguments passed to \link{prepare_figure}.}
}
\description{
Output plots as files
}
