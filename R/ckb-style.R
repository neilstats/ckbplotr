#' CKB ggplot theme
#'
#' A ggplot2 theme, based on theme_bw.
#'
#' @param base_size Base font size, given in pts. (Default: 11)
#' @param base_line_size Base size for line elements. (Deault: base_size/22)
#' @param ink,paper Colour for foreground and background elements. (Defaults: "black" and "white")
#' @param colour Deprecated. Use `ink` instead.
#' @param axis.title.margin Margin between axis titles and plot. (Default: 1)
#' @param plot.margin Margin around entire plot (Default: margin(0.5, 0, 0.5, 0, "lines"))
#' @param ... Arguments passed to `ggplot2::theme_bw()` if using ggplot2 4.0.0 or later.
#'
#' @export

theme_ckb <- function(base_size         = 11,
                      base_line_size    = base_size/22,
                      ink               = "black",
                      paper             = "white",
                      colour            = NULL,
                      axis.title.margin = 1,
                      plot.margin       = margin(0.5, 1.5, 0.5, 0.5, "lines"),
                      ...){

  if (!missing(colour)) {
    ink <- colour
  }

  if (compareVersion(as.character(packageVersion("ggplot2")), "4.0.0") >= 0) {
    theme_bw(...,
             ink = ink,
             paper = paper,
             base_size = base_size,
             base_line_size = base_line_size) %+replace%
      theme(axis.line = element_line(lineend = "round"),
            axis.text = element_text(colour = ink),
            axis.ticks = element_line(colour = ink),
            axis.text.x       = element_text(margin = margin(t = base_size/(11/4.4)), vjust = 1),
            axis.text.x.top   = element_text(margin = margin(b = base_size/(11/4.4)), vjust = 0),
            axis.text.y       = element_text(margin = margin(r = base_size/(11/4.4)), hjust = 1),
            axis.text.y.right = element_text(margin = margin(l = base_size/(11/4.4)), hjust = 0),
            axis.title        = element_text(face = "bold"),
            axis.title.x      = element_text(margin = margin(axis.title.margin, 0, 0, 0, "lines")),
            axis.title.y      = element_text(margin = margin(0, axis.title.margin, 0, 0, "lines"), angle = 90),

            panel.grid = element_blank(),
            panel.border = element_blank(),
            legend.background = element_blank(),
            strip.background  = element_blank(),
            strip.text        = element_text(face = "bold", colour = ink),
            plot.margin       = plot.margin,
            plot.title        = element_text(hjust = 0.5, face = "bold"),
            complete = TRUE)
  } else {
    theme_bw(base_size = base_size,
             base_line_size = base_line_size) %+replace%
      theme(panel.grid        = element_blank(),
            panel.border      = element_blank(),
            panel.background  = element_blank(),
            axis.ticks        = element_line(colour = ink),
            axis.line         = element_line(colour = ink, lineend = "round"),
            axis.text         = element_text(colour = ink),
            axis.text.x       = element_text(margin = margin(t = base_size/(11/4.4)), vjust = 1),
            axis.text.x.top   = element_text(margin = margin(b = base_size/(11/4.4)), vjust = 0),
            axis.text.y       = element_text(margin = margin(r = base_size/(11/4.4)), hjust = 1),
            axis.text.y.right = element_text(margin = margin(l = base_size/(11/4.4)), hjust = 0),
            axis.title        = element_text(face = "bold", colour = ink),
            axis.title.x      = element_text(margin = margin(axis.title.margin, 0, 0, 0, "lines")),
            axis.title.y      = element_text(margin = margin(0, axis.title.margin, 0, 0, "lines"), angle = 90),
            legend.background = element_blank(),
            strip.background  = element_blank(),
            strip.text        = element_text(face = "bold", colour = ink),
            plot.margin       = plot.margin,
            plot.background   = element_rect(fill = paper, colour = paper),
            plot.title        = element_text(hjust = 0.5, face = "bold", colour = ink),
            complete          = TRUE)
  }
}






#' Make a ggplot into CKB style
#'
#'
#' @inheritParams theme_ckb
#' @param xlims A numeric vector of length two. The limits of the x-axis.
#' @param ylims A numeric vector of length two. The limits of the y-axis.
#' @param gap A numeric vector of length two. The gap between plotting area and axis to the left and bottom of the plot, as a proportion of the x-axis length. (Default: c(0.025, 0.025))
#' @param ext A numeric vector of length two. The extensions to add to the right and top of the plot, as a proportion of the x-axis length. (Default: c(0, 0))
#' @param ratio The ratio (y-axis:x-axis) to use for the plot. Ignored if both width and height are set. (Default: 1.5)
#' @param width A `grid::unit` object to set the width of the plot (not including the gap or extension).
#' @param height A `grid::unit` object to set the height of the plot (not including the gap or extension).
#' @param clip Passed to clip argument of `ggplot2::coord_cartesian()`. (Default: "on")
#'
#'
#' @import ggplot2
#' @export

ckb_style <- function(
    xlims          = NULL,
    ylims          = NULL,
    gap            = c(0.025,0.025),
    ext            = c(0,0),
    ratio          = 1.5,
    width          = NULL,
    height         = NULL,
    base_size      = 11,
    base_line_size = base_size/22,
    ink            = "black",
    paper          = "white",
    colour         = NULL,
    axis.title.margin = 1,
    plot.margin    = margin(0.5, 1.5, 0.5, 0.5, "lines"),
    clip = "on"
){

  # check arguments
  if (length(gap) != 2){
    rlang::abort("gap must be a vector of length 2")
  }

  if (length(ext) != 2){
    rlang::abort("ext must be a vector of length 2")
  }

  if (!missing(colour)) {
    ink <- colour
  }

  # panel sizes
  if (missing(width) & missing(height)){
    full_width <- 1
    full_height <- (ratio + gap[[2]] + ext[[2]]) / (1 + gap[[1]] + ext[[1]])
  } else if (!missing(width) & missing(height)){
    full_width <- width * (1 + gap[[1]] + ext[[1]])
    full_height <-  width * (ratio + gap[[2]] + ext[[2]])
  } else if (missing(width) & !missing(height)){
    full_width <- height / ratio * (1 + gap[[1]] + ext[[1]])
    full_height <- height / ratio * (ratio + gap[[2]] + ext[[2]])
  } else if (!missing(width) & !missing(height)){
    full_width <- width * (1 + gap[[1]] + ext[[1]])
    full_height <- height + width * (gap[[2]] + ext[[2]])
    ratio <- as.numeric(grid::convertUnit(height, "mm")) / as.numeric(grid::convertUnit(width, "mm"))
  }


  return(structure(list(xlims = xlims,
                        ylims = ylims,
                        gap = gap,
                        ext = ext,
                        ratio = ratio,
                        full_height = full_height,
                        full_width = full_width,
                        base_line_size = base_line_size,
                        base_size = base_size,
                        ink = ink,
                        paper = paper,
                        axis.title.margin = axis.title.margin,
                        plot.margin = plot.margin,
                        clip = clip),
                   class = "ckbplot"))
}

#' @noRd
#' @export
#' @keywords internal
ggplot_add.ckbplot <- function(object, plot, ...) {
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
  if (is.null(object$xlims)){
    if (!is.null(ggplot_build(plot)$layout$panel_scales_x[[1]]$limits)){
      xlims <- ggplot_build(plot)$layout$panel_scales_x[[1]]$limits
    } else {
      xlims <- ggplot_build(plot)$layout$panel_scales_x[[1]]$range$range
    }
    xlims <- range(pretty(invtf_x(xlims)))
  } else {
    xlims <- object$xlims
  }

  if (is.null(object$ylims)){
    if (!is.null(ggplot_build(plot)$layout$panel_scales_y[[1]]$limits)){
      ylims <- ggplot_build(plot)$layout$panel_scales_y[[1]]$limits
    } else {
      ylims <- ggplot_build(plot)$layout$panel_scales_y[[1]]$range$range
    }
    ylims <- range(pretty(invtf_y(ylims)))
  } else {
    ylims <- object$ylims
  }

  # calculate plot limits
  limits <- list(xaxis = xlims, yaxis = ylims)

  ## check for infinite values in transformed axis limits
  if (any(!is.finite(tf_x(limits[["xaxis"]])))) {
    rlang::abort("Infinite or NaN values in x-axis. Provide axis limits and check transformation of x scale.")
  }
  if (any(!is.finite(tf_y(limits[["yaxis"]])))) {
    rlang::abort("Infinite or NaN values in y-axis. Provide axis limits and check transformation of y scale.")
  }

  addtox <- c(object$gap[[1]]*diff(range(tf_x(limits[["xaxis"]]))),
              object$ext[[1]]*diff(range(tf_x(limits[["xaxis"]]))))
  addtoy <- c((1/object$ratio)*object$gap[[2]]*diff(range(tf_y(limits[["yaxis"]]))),
              (1/object$ratio)*object$ext[[2]]*diff(range(tf_y(limits[["yaxis"]]))))
  limits[["x"]] <- invtf_x(tf_x(limits[["xaxis"]]) + c(-1, 1)*addtox)
  limits[["y"]] <- invtf_y(tf_y(limits[["yaxis"]]) + c(-1, 1)*addtoy)


  # update plot
  plot <- plot +
    coord_cartesian(xlim   = limits[["x"]],
                    ylim   = limits[["y"]],
                    expand = FALSE,
                    clip = object$clip) +
    ggh4x::force_panelsizes(rows = object$full_height,
                            cols = object$full_width,
                            respect = TRUE) +
    guides(x = legendry::guide_axis_base(cap = tf_x(limits[["xaxis"]])),
           y = legendry::guide_axis_base(cap = tf_y(limits[["yaxis"]]))) +
    theme_ckb(base_size = object$base_size,
              base_line_size = object$base_line_size,
              ink = object$ink,
              paper = object$paper,
              axis.title.margin = object$axis.title.margin,
              plot.margin = object$plot.margin)

  return(plot)
}






#' Make a ggplot into CKB style
#'
#' @param plot A ggplot2 plot
#' @param ... Arguments passed to ckb_style()
#'
#' @return A ggplot2 plot.
#'
#' @import ggplot2
#' @export

plot_like_ckb <- function(plot, ...){
  plot <- plot + ckb_style(...)
  return(plot)
}

