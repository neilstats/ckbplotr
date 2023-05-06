#' CKB ggplot theme
#'
#' Based on theme_bw
#'
#' @param base_size base font size, given in pts.
#' @param base_line_size base size for line elements
#' @param colour Colour for non-data aspects of the plot. (Default: "black")
#' @param plot.margin Margin around entire plot (Default: margin(0.5, 0, 0.5, 0, "lines"))
#'
#' @export

theme_ckb <- function(base_size      = 11,
                      base_line_size = base_size/22,
                      colour         = "black",
                      plot.margin    = margin(0.5, 0, 0.5, 0, "lines")){
  theme_bw(base_size = base_size,
           base_line_size = base_line_size) %+replace%
    theme(panel.grid        = element_blank(),
          panel.border      = element_blank(),
          panel.background  = element_blank(),
          axis.ticks        = element_line(colour = colour),
          axis.text         = element_text(colour = colour),
          axis.text.x       = element_text(margin = margin(t = base_size/(11/4.4)), vjust = 1),
          axis.text.x.top   = element_text(margin = margin(b = base_size/(11/4.4)), vjust = 0),
          axis.text.y       = element_text(margin = margin(r = base_size/(11/4.4)), hjust = 1),
          axis.text.y.right = element_text(margin = margin(l = base_size/(11/4.4)), hjust = 0),
          axis.title        = element_text(face = "bold", colour = colour),
          axis.title.x      = element_text(margin = unit(c(1,0,0,0), "lines")),
          axis.title.y      = element_text(margin = unit(c(0,1,0,0), "lines"), angle = 90),
          strip.background  = element_blank(),
          strip.text        = element_text(face = "bold", colour = colour),
          plot.margin       = plot.margin,
          plot.background   = element_blank(),
          plot.title        = element_text(hjust = 0.5, face = "bold", colour = colour),
          complete          = TRUE)
}




#' Make a ggplot into CKB style
#'
#' @inheritParams theme_ckb
#' @param plot A ggplot2 plot object.
#' @param xlims A numeric vector of length two. The limits of the x-axis.
#' @param ylims A numeric vector of length two. The limits of the y-axis.
#' @param gap A numeric vector of length two. The gap between plotting area and axis to the left and bottom of the plot, as a proportion of the x-axis length. (Default: c(0.025, 0.025))
#' @param ext A numeric vector of length two. The extensions to add to the right and top of the plot, as a proportion of the x-axis length. (Default: c(0, 0))
#' @param ratio The ratio (y-axis:x-axis) to use for the plot. Ignored if both width and height are set. (Default: 1.5)
#' @param width A `grid::unit` object to set the width of the plot (not including the gap or extension).
#' @param height A `grid::unit` object to set the height of the plot (not including the gap or extension).
#' @param colour Colour for non-data aspects of the plot. (Default: "black")
#' @param axes Choice of axis lines to add to the plot, one of "both", "x" or "y". (Default: "both")
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
    ext=c(0,0),
    ratio=1.5,
    width = NULL,
    height = NULL,
    base_size = 11,
    base_line_size = base_size/22,
    colour = "black",
    plot.margin = margin(0.5, 0, 0.5, 0, "lines"),
    axes = "both"
){

  # check arguments
  if (!axes %in% c("both", "x", "y", "none")){rlang::abort("axes should be one of 'both', 'x', 'y' or 'none'.")}

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

  # update plot
  plot <- plot +
    coord_fixed(xlim   = limits[["x"]],
                ylim   = limits[["y"]],
                expand = FALSE,
                clip = "off") +
    ggh4x::force_panelsizes(rows = full_height,
                            cols = full_width,
                            respect = TRUE) +
    theme_ckb(base_size = base_size,
              base_line_size = base_line_size,
              colour = colour,
              plot.margin = plot.margin)

  # add axis lines to plot
  if (axes %in% c("both", "y")){
    plot <- plot +
      annotate(geom = "segment",
               x    = limits[["x"]][[1]],
               xend = limits[["x"]][[1]],
               y    = limits[["yaxis"]][[1]],
               yend = limits[["yaxis"]][[2]],
               linewidth  = base_line_size,
               lineend = "round",
               colour = colour)
  }

  if (axes %in% c("both", "x")){
    plot <- plot +
      annotate(geom = "segment",
               x    = limits[["xaxis"]][[1]],
               xend = limits[["xaxis"]][[2]],
               y    = limits[["y"]][[1]],
               yend = limits[["y"]][[1]],
               linewidth  = base_line_size,
               lineend = "round",
               colour = colour)
  }

  return(plot)
}



#' Make a ggplot into CKB style
#'
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams theme_ckb
#' @param xlims A numeric vector of length two. The limits of the x-axis.
#' @param ylims A numeric vector of length two. The limits of the y-axis.
#' @param gap A numeric vector of length two. The gap between plotting area and axis to the left and bottom of the plot, as a proportion of the x-axis length. (Default: c(0.025, 0.025))
#' @param ext A numeric vector of length two. The extensions to add to the right and top of the plot, as a proportion of the x-axis length. (Default: c(0, 0))
#' @param ratio The ratio (y-axis:x-axis) to use for the plot. Ignored if both width and height are set. (Default: 1.5)
#' @param width A `grid::unit` object to set the width of the plot (not including the gap or extension).
#' @param height A `grid::unit` object to set the height of the plot (not including the gap or extension).
#' @param axes Choice of axis lines to add to the plot, one of "both", "x" or "y". (Default: "both")
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
    colour         = "black",
    plot.margin    = margin(0.5, 0, 0.5, 0, "lines"),
    axes           = "both"
){

  # check arguments
  if (!axes %in% c("both", "x", "y", "none")){
    rlang::abort("axes should be one of 'both', 'x', 'y' or 'none'.")
  }

  if (length(gap) != 2){
    rlang::abort("gap must be a vector of length 2")
  }

  if (length(ext) != 2){
    rlang::abort("ext must be a vector of length 2")
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
                        colour = colour,
                        plot.margin = plot.margin,
                        axes = axes),
                   class = "ckbplot"))
}

#' @noRd
#' @export
#' @keywords internal
ggplot_add.ckbplot <- function(object, plot, object_name) {
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
    xlims <- range(pretty(xlims))
  } else {
    xlims <- object$xlims
  }

  if (is.null(object$ylims)){
    if (!is.null(ggplot_build(plot)$layout$panel_scales_y[[1]]$limits)){
      ylims <- ggplot_build(plot)$layout$panel_scales_y[[1]]$limits
    } else {
      ylims <- ggplot_build(plot)$layout$panel_scales_y[[1]]$range$range
    }
    ylims <- range(pretty(ylims))
  } else {
    ylims <- object$ylims
  }

  # calculate plot limits
  limits <- list(xaxis = xlims, yaxis = ylims)
  addtox <- c(object$gap[[1]]*diff(range(tf_x(limits[["xaxis"]]))),
              object$ext[[1]]*diff(range(tf_x(limits[["xaxis"]]))))
  addtoy <- c((1/object$ratio)*object$gap[[2]]*diff(range(tf_y(limits[["yaxis"]]))),
              (1/object$ratio)*object$ext[[2]]*diff(range(tf_y(limits[["yaxis"]]))))
  limits[["x"]] <- invtf_x(tf_x(limits[["xaxis"]]) + c(-1, 1)*addtox)
  limits[["y"]] <- invtf_y(tf_y(limits[["yaxis"]]) + c(-1, 1)*addtoy)


  # update plot
  plot <- plot +
    coord_fixed(xlim   = limits[["x"]],
                ylim   = limits[["y"]],
                expand = FALSE,
                clip = "off") +
    ggh4x::force_panelsizes(rows = object$full_height,
                            cols = object$full_width,
                            respect = TRUE) +
    theme_ckb(base_size = object$base_size,
              base_line_size = object$base_line_size,
              colour = object$colour,
              plot.margin = object$plot.margin)

  # add axis lines to plot
  if (object$axes %in% c("both", "y")){
    plot <- plot +
      annotate(geom = "segment",
               x    = limits[["x"]][[1]],
               xend = limits[["x"]][[1]],
               y    = limits[["yaxis"]][[1]],
               yend = limits[["yaxis"]][[2]],
               linewidth  = object$base_line_size,
               lineend = "round",
               colour = object$colour)
  }

  if (object$axes %in% c("both", "x")){
    plot <- plot +
      annotate(geom = "segment",
               x    = limits[["xaxis"]][[1]],
               xend = limits[["xaxis"]][[2]],
               y    = limits[["y"]][[1]],
               yend = limits[["y"]][[1]],
               linewidth  = object$base_line_size,
               lineend = "round",
               colour = object$colour)
  }

  return(plot)
}
