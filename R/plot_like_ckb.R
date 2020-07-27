#' CKB ggplot theme
#'
#' Based on theme_bw
#'
#' @param base_size base font size, given in pts.
#' @param base_line_size base size for line elements
#'
#' @export

theme_ckb <- function(base_size = 11,
                      base_line_size = base_size/22){
  theme_bw(base_size = base_size,
           base_line_size = base_line_size) %+replace%
    theme(panel.grid        = element_blank(),
          panel.border      = element_blank(),
          axis.ticks        = element_line(colour = "black"),
          axis.text         = element_text(colour = "black"),
          axis.text.x       = element_text(margin = margin(t = base_size/(11/4.4)), vjust = 1),
          axis.text.x.top   = element_text(margin = margin(b = base_size/(11/4.4)), vjust = 0),
          axis.text.y       = element_text(margin = margin(r = base_size/(11/4.4)), hjust = 1),
          axis.text.y.right = element_text(margin = margin(l = base_size/(11/4.4)), hjust = 0),
          axis.title        = element_text(face = "bold"),
          axis.title.x      = element_text(margin = unit(c(1,0,0,0), "lines")),
          axis.title.y      = element_text(margin = unit(c(0,1,0,0), "lines"), angle = 90),
          plot.margin       = unit(c(0,0,0.5,0), "lines"),
          plot.background   = element_blank(),
          plot.title        = element_text(hjust = 0.5, face = "bold"),
          complete          = TRUE)
}




#' Make a ggplot into CKB style
#'
#' @inheritParams theme_ckb
#' @param plot A ggplot2 plot object.
#' @param xlims A numeric vector of length two. The limits of the x-axis.
#' @param ylims A numeric vector of length two. The limits of the y-axis.
#' @param gap A numeric vector of length two. The gap between plotting area and axis to the left and bottom of the plot, as a proportion of the x-axis length. (Default: c(0.025, 0.025))
#' @param ext A numeric vector of length two. The extensions to add to the right and top of the plot, as a proportion of the x-axis length. (Default: c(0.025, 0.025))
#' @param ratio The ratio (y-axis:x-axis) to use for the plot. (Default: 1.5)
#'
#' @return A ggplot2 plot.
#'
#' @import ggplot2
#' @export

plot_like_ckb <- function(
  plot,
  xlims=NULL,
  ylims=NULL,
  gap=c(0.025,0.025),
  ext=c(0.025,0.025),
  ratio=1.5,
  base_size = 11,
  base_line_size = base_size/22
){

  # get plot axis transformations
  tf_x    <- ggplot_build(plot)$layout$panel_scales_x[[1]]$trans$transform
  invtf_x <- ggplot_build(plot)$layout$panel_scales_x[[1]]$trans$inverse
  tf_y    <- ggplot_build(plot)$layout$panel_scales_y[[1]]$trans$transform
  invtf_y <- ggplot_build(plot)$layout$panel_scales_y[[1]]$trans$inverse
  tf_x    <- ifelse(is.null(tf_x), identity, tf_x)
  invtf_x <- ifelse(is.null(invtf_x), identity, invtf_x)
  tf_y    <- ifelse(is.null(tf_y), identity, tf_y)
  invtf_y <- ifelse(is.null(invtf_y), identity, invtf_y)

  # if xlims or ylims not given as argument, get axis limits from range in ggplot plot
  if (is.null(xlims)){
    if (!is.null(ggplot_build(plot)$layout$panel_scales_x[[1]]$limits)){
      xlims <- ggplot_build(plot)$layout$panel_scales_x[[1]]$limits
    } else {
      xlims <- ggplot_build(plot)$layout$panel_scales_x[[1]]$range$range
    }
    xlims <- range(pretty(xlims))
  }

  if (is.null(ylims)){
    if (!is.null(ggplot_build(plot)$layout$panel_scales_y[[1]]$limits)){
      ylims <- ggplot_build(plot)$layout$panel_scales_y[[1]]$limits
    } else {
      ylims <- ggplot_build(plot)$layout$panel_scales_y[[1]]$range$range
    }
    ylims <- range(pretty(ylims))
  }

  # calculate plot limits
  limits <- list(xaxis = xlims, yaxis = ylims)
  addtox <- c(gap[[1]]*diff(range(tf_x(limits[["xaxis"]]))),
              ext[[1]]*diff(range(tf_x(limits[["xaxis"]]))))
  addtoy <- c((1/ratio)*gap[[2]]*diff(range(tf_y(limits[["yaxis"]]))),
              (1/ratio)*ext[[2]]*diff(range(tf_y(limits[["yaxis"]]))))
  limits[["x"]] <- invtf_x(tf_x(limits[["xaxis"]]) + c(-1, 1)*addtox)
  limits[["y"]] <- invtf_y(tf_y(limits[["yaxis"]]) + c(-1, 1)*addtoy)
  limits[["ratio"]] <- ratio*diff(range(tf_x(limits[["xaxis"]])))/diff(range(tf_y(limits[["yaxis"]])))

  plot +
    coord_fixed(ratio  = limits[["ratio"]],
                xlim   = limits[["x"]],
                ylim   = limits[["y"]],
                expand = FALSE,
                clip = "off") +
    annotate(geom = "segment",
             x    = limits[["x"]][[1]],
             xend = limits[["x"]][[1]],
             y    = limits[["yaxis"]][[1]],
             yend = limits[["yaxis"]][[2]],
             lwd  = base_line_size,
             lineend = "round",
             colour = "black") +
    annotate(geom = "segment",
             x    = limits[["xaxis"]][[1]],
             xend = limits[["xaxis"]][[2]],
             y    = limits[["y"]][[1]],
             yend = limits[["y"]][[1]],
             lwd  = base_line_size,
             lineend = "round",
             colour = "black") +
    theme_ckb(base_size = base_size, base_line_size = base_line_size)
}
