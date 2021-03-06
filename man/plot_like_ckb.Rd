% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_like_ckb.R
\name{plot_like_ckb}
\alias{plot_like_ckb}
\title{Make a ggplot into CKB style}
\usage{
plot_like_ckb(
  plot,
  xlims = NULL,
  ylims = NULL,
  gap = c(0.025, 0.025),
  ext = c(0.025, 0.025),
  ratio = 1.5,
  base_size = 11,
  base_line_size = base_size/22
)
}
\arguments{
\item{plot}{A ggplot2 plot object.}

\item{xlims}{A numeric vector of length two. The limits of the x-axis.}

\item{ylims}{A numeric vector of length two. The limits of the y-axis.}

\item{gap}{A numeric vector of length two. The gap between plotting area and axis to the left and bottom of the plot, as a proportion of the x-axis length. (Default: c(0.025, 0.025))}

\item{ext}{A numeric vector of length two. The extensions to add to the right and top of the plot, as a proportion of the x-axis length. (Default: c(0.025, 0.025))}

\item{ratio}{The ratio (y-axis:x-axis) to use for the plot. (Default: 1.5)}

\item{base_size}{base font size, given in pts.}

\item{base_line_size}{base size for line elements}
}
\value{
A ggplot2 plot.
}
\description{
Make a ggplot into CKB style
}
