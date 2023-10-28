#' forest plots when xlim is a list
#' @noRd
forest_plot_list_xlim <- function(call){

  xlim <- eval(call$xlim)
  xticks <- eval(call$xticks)
  panels <- eval(call$panels)

  ## check arguments
  if (!is.list(xlim) | !is.list(xticks) | !is.list(panels)){
    rlang::abort("panels, xlim and xticks must be lists")
  }
  if (length(unique(c(length(xlim), length(xticks), length(panels)))) != 1){
    rlang::abort("panels, xlim and xticks must be lists of the same length")
  }

  ## create arguments for plot.margin and mid.space
  plot.margin <- eval(call$plot.margin)
  if (is.null(plot.margin)){
    plot.margin <- eval(formals(forest_plot)$plot.margin)
  }
  mid.space <- eval(call$mid.space)
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

    forest <- do.call("forest_plot",
                      utils::modifyList(call, update_args))

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
