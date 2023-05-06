#' Fix panel width and height of a forest plot
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' plot_like_ckb() and shape_plot() have width and height arguments and
#' forest_plot() has panel.width and panel.height arguments. These use
#' ggh4x::force_panelsizes() to fix panel sizes.
#'
#' @param plot A plot (created by forest_plot()).
#' @param width Width of panels. (e.g unit(50, "mm"))
#' @param height Height of panels. (e.g unit(150, "mm"))
#'
#' @return A gtable object
#'
#' @keywords internal
#' @export


fix_panel <- function(plot, width = NULL, height = NULL){

  lifecycle::deprecate_warn("0.8.1",
                            "fix_panel()",
                            "ggh4x::force_panelsizes()",
                            details = "forest_plot(), shape_plot(), and plot_like_ckb() alo have arguments for setting panel width and height.")

  # generate grob from ggplot2 plot
  gtable <- ggplot2::ggplotGrob(plot)

  # check arguments
  if (!missing(width) & !missing(height) & gtable$respect){rlang::abort("Can only specificy one of width and height to maintain aspect ratio.)")}

  ## calculate ratio from numeric part of panel height / width
  ## assumes they are the same unit (probably "null" because created by ggplot)
  ratio <- as.numeric(gtable$heights[gtable$layout$t[grepl("panel", gtable$layout$name)]]) / as.numeric(gtable$widths[gtable$layout$l[grepl("panel", gtable$layout$name)]])

  if(!is.null(width)){
    gtable$widths[gtable$layout$l[grepl("panel", gtable$layout$name)]] <- width
    if (gtable$respect){
      ## respect aspect ratio
      gtable$heights[gtable$layout$t[grepl("panel", gtable$layout$name)]] <- width * ratio
    }
  }
  if(!is.null(height)){
    gtable$heights[gtable$layout$t[grepl("panel", gtable$layout$name)]] <- height
    if (gtable$respect){
      ## respect aspect ratio
      gtable$widths[gtable$layout$l[grepl("panel", gtable$layout$name)]] <- height / ratio
    }
  }
  gtable
}
