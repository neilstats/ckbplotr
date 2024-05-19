#' forest plots when xlim is a list
#' @noRd
forest_plot_list_xlim <- function(
  ## original arguments passed to
  ## separate forest_plot() calls
  original_arguments,
  ## arguments that need to evaluated first
  ## because they are used by
  xlim,
  xticks,
  panels,
  xlab,
  col.left.heading,
  col.right.heading,
  panel.headings,
  ## the new environment in which to evaluate forest_plot()
  envir
){

  ## check arguments
  if (!is.list(xlim) | !is.list(xticks) | !is.list(panels)){
    rlang::abort("panels, xlim and xticks must be lists")
  }
  if (length(unique(c(length(xlim), length(xticks), length(panels)))) != 1){
    rlang::abort("panels, xlim and xticks must be lists of the same length")
  }
  if (!is.null(panel.headings) && length(panel.headings) != length(panels)){
    rlang::abort("panel.headings must be same length as panels")
  }

  ## make lists
  xlab <- as.list(xlab)
  if (length(xlab) < length(xlim)){
    xlab <- rep(xlab, length(xlim))
  }
  if (!is.list(col.left.heading)){
    col.left.heading <- rep(list(col.left.heading), length(xlim))
  }
  if (!is.list(col.right.heading)){
    col.right.heading <- rep(list(col.right.heading), length(xlim))
  }


  ## create arguments for plot.margin and mid.space
  plot.margin <- eval(original_arguments$plot.margin)
  if (is.null(plot.margin)){
    plot.margin <- eval(formals(forest_plot)$plot.margin)
  }
  mid.space <- eval(original_arguments$mid.space)
  if (is.null(mid.space)){
    mid.space <- eval(formals(forest_plot)$mid.space)
  }

  ## create list of plots (one for each panel)
  plots_list <- lapply(1:length(xlim), \(i) {

    ## adjust right plot.margin for all but last panel
    plot_margin <- plot.margin
    if (i != length(xlim)){
      plot_margin[[2]] <- mid.space
    }

    update_args <- list(panels = panels[i],
                        xlim = xlim[[i]],
                        xticks = xticks[[i]],
                        plot.margin = plot_margin,
                        quiet = TRUE)

    if (!is.null(xlab[i]) && length(xlab) > 0){
      update_args$xlab <- xlab[[i]]
    }
    if (!is.null(col.left.heading[i])){
      update_args$col.left.heading <- col.left.heading[[i]]
    }
    if (!is.null(col.right.heading[i])){
      update_args$col.right.heading <- col.right.heading[[i]]
    }
    if (!is.null(panel.headings)){
      update_args$panel.headings <- panel.headings[[i]]
    }

    forest <- do.call("forest_plot",
                      utils::modifyList(original_arguments, update_args),
                      envir = envir)

    ## adjust left plot.margin for all but first panel
    if (i != 1){
      forest$plot$theme$plot.margin[[4]] <- forest$plot$theme$axis.text.y$margin[[2]]
      forest$plot <- forest$plot + theme(axis.text.y = element_blank())
    }

    return(forest$plot)
  })

  ## create figure by combining plots
  figure <- do.call(gridExtra::gtable_cbind,
                    lapply(plots_list, ggplotGrob))

  return(invisible(list(figure = figure,
                        plots = plots_list)))
}
