#' forest plots when xlim is a list
#' @noRd
forest_plot_list_xlim <- function(
  ## original arguments passed to
  ## separate forest_plot() calls
  original_arguments,
  ## arguments that need to be evaluated first
  ## because they are used by forest_plot_list_xlim()
  xlim,
  xticks,
  panels,
  xlab,
  nullval,
  left.heading,
  right.heading,
  panel.headings,
  ## the new environment in which to evaluate forest_plot()
  envir
){

  ## check arguments
  if (!is.list(xlim) | !is.list(xticks) | !is.list(panels)){
    cli::cli_abort("{.arg panels}, {.arg xlim} and {.arg xticks} must be lists")
  }
  if (length(unique(c(length(xlim), length(xticks), length(panels)))) != 1){
    cli::cli_abort("{.arg panels}, {.arg xlim} and {.arg xticks} must be lists of the same length")
  }
  if (!is.null(panel.headings) && length(panel.headings) != length(panels)){
    cli::cli_abort("{.arg panel.headings} must be same length as {.arg panels}")
  }

  ## make lists
  xlab <- as.list(xlab)
  if (length(xlab) < length(xlim)){
    xlab <- rep(xlab, length(xlim))
  }
  if (!is.list(left.heading)){
    left.heading <- rep(list(left.heading), length(xlim))
  }
  if (!is.list(right.heading)){
    right.heading <- rep(list(right.heading), length(xlim))
  } else if (length(right.heading) < length(xlim)) {
    right.heading <- rep(right.heading, length(xlim))
  }
  nullval <- as.list(nullval)
  if (length(nullval) < length(xlim)){
    nullval <- rep(nullval, length(xlim))
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

    update_args <- list(panels = panels[i],
                        xlim = xlim[[i]],
                        xticks = xticks[[i]],
                        quiet = TRUE)

    if (!is.null(xlab[i]) && length(xlab) > 0){
      update_args$xlab <- xlab[[i]]
    }
    if (!is.null(nullval[i]) && length(nullval) > 0){
      update_args$nullval <- nullval[[i]]
    }
    if (!is.null(left.heading[i])){
      update_args$left.heading <- left.heading[[i]]
    }
    if (!is.null(right.heading[i])){
      update_args$right.heading <- right.heading[[i]]
    }
    if (!is.null(panel.headings)){
      update_args$panel.headings <- panel.headings[[i]]
    }
    if (is.list(eval(original_arguments$fill))){
      update_args$fill <- eval(original_arguments$fill)[[i]]
    }
    if (is.list(eval(original_arguments$cicolour))){
      update_args$cicolour <- eval(original_arguments$cicolour)[[i]]
    }
    if (is.list(eval(original_arguments$colour))){
      update_args$colour <- eval(original_arguments$colour)[[i]]
    }
    forest <- do.call("forest_plot",
                      utils::modifyList(original_arguments, update_args),
                      envir = envir)
    return(forest)
  })

  ## function to adjust margins of plot for each panel
  adjust_margins <- function(i, plots_list){

    panel <- plots_list[[i]]$plot

    ## adjust right plot.margin for all but last panel
    if (i != length(plots_list)){
      panel$theme$plot.margin[[2]] <- panel$theme$panel.spacing - panel$theme$axis.text.y$margin[[2]]
    }

    ## adjust left plot.margin for all but first panel
    if (i != 1){
      panel$theme$plot.margin[[4]] <- panel$theme$axis.text.y$margin[[2]]
      panel <- panel + theme(axis.text.y = element_blank())
    }

    return(panel)
  }

  ## create figure by combining plots
  figure <- do.call(gridExtra::gtable_cbind,
                    lapply(lapply(seq_along(plots_list),
                                  function(x) adjust_margins(x, plots_list)),
                           ggplotGrob))

  return(invisible(list(figure = figure,
                        plots = plots_list)))
}
