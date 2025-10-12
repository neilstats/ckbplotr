#' Prepare figure for saving
#'
#' @param figure Plot (or graphical object).
#' @param title Title to be added to the page. (Default: "")
#' @param title.pos Position of the title text. Default is 1/4 inch from top left of page.
#'    (Default: unit.c(unit(1.27/2, "cm"), unit(1, "npc") - unit(1.27/2, "cm")))
#' @param title.just Justification of the title text. (Default: c(0, 1))
#' @param title.gpar Graphical parameters for title. (Default: list(fontsize = 12, fontface = "bold"))
#' @param footer Footer to be added to the page. (Default: "")
#' @param footer.pos Position of the footer text.
#'    Default is 1/6 inch from bottom and 1/4 inch from left of page.
#'    (Default: unit.c(unit(1.27/2, "cm"), unit(1.27/3, "cm")))
#' @param footer.just Justification of the footer text. (Default: c(0, 0))
#' @param footer.gpar Graphical parameters for footer. (Default: list(fontsize = 9))
#' @param margin Margin to be placed around the plot.
#'    Default is 2.27cm top, 1.27cm (1/2 inch) other sides.
#'    (Default: unit(c(2.27, 1.27, 1.27, 1.27), units = "cm"))
#' @param size A unit vector of length two (width, height).
#'    Size of plot (a width/height larger than page weight/height minus margins will be
#'    ignored), centred within margins.
#'    By default, plot will fill the space within margins.
#' @param valign If size is set, where to place figure within margins. 1 = top, 0.5 = middle, 0 = bottom. (Default: 0.5)
#' @param halign If size is set, where to place figure within margins. 1 = right, 0.5 = middle, 0 = left (Default: 0.5)
#' @param pagesize Page size of output: "A4" or "A5". (Default: "A4")
#' @param landscape Landscape page orientation? (Default: False)
#' @param pagedim Dimensions (width, height) of output. Overrides pagesize and landscape arguments if used.
#'
#' @export
#'

prepare_figure <- function(figure,
                           title       = "",
                           title.pos   = grid::unit.c(unit(1.27/2, "cm"),
                                                      unit(1, "npc") - unit(1.27/2, "cm")),
                           title.just  = c(0, 1),
                           title.gpar  = list(fontsize = 12,
                                              fontface = "bold"),
                           footer      = "",
                           footer.pos  = grid::unit.c(unit(1.27/2, "cm"),
                                                      unit(1.27/3, "cm")),
                           footer.just = c(0, 0),
                           footer.gpar = list(fontsize = 9),
                           margin      = unit(c(2.27, 1.27, 1.27, 1.27), units = "cm"),
                           size        = NULL,
                           valign      = 0.5,
                           halign      = 0.5,
                           pagesize    = c("A4", "A5"),
                           landscape   = FALSE,
                           pagedim     = NULL){

  ## Check it figure is a patchwork object, and convert to gtable
  if (inherits(figure, "patchwork")){
    figure <- patchwork::patchworkGrob(figure)
  }
  if (inherits(figure, "ggplot")) {
    figure <- ggplotGrob(figure)
  }

  ## Set page dimensions
  pagesize <- match.arg(pagesize)
  if (missing(pagedim)){
    pagedim <- switch(pagesize,
                      A4 = unit(c(210, 297), "mm"),
                      A5 = unit(c(148, 210), "mm"),
                      cli::cli_abort("Invalid pagesize value."))
  }

  if (landscape){
    pagedim <- rev(pagedim)
  }


  ## Increase margins so that figure will fit dimensions given by size argument
  ## (Do not decrease margins)
  if (!missing(size)){
    add_to_width_margins <- pagedim[[1]] - size[[1]] - margin[[2]] - margin[[4]]
    add_to_width_margins <- grid::unit.pmax(unit(0, "mm"),
                                            grid::convertUnit(add_to_width_margins, "mm"))
    margin[[2]] <- margin[[2]] + (1 - halign) * add_to_width_margins
    margin[[4]] <- margin[[4]] + halign * add_to_width_margins
    add_to_height_margins <- pagedim[[2]] - size[[2]] - margin[[1]] - margin[[3]]
    add_to_height_margins <- grid::unit.pmax(unit(0, "mm"),
                                             grid::convertUnit(add_to_height_margins, "mm"))
    margin[[1]] <- margin[[1]] + (1 - valign) * add_to_height_margins
    margin[[3]] <- margin[[3]] + valign * add_to_height_margins
  }


  ## Arrange figure with page margins
  figure_vp <- grid::viewport(
    x = margin[4],
    y = margin[3],
    width = pagedim[1] - margin[4] - margin[2],
    height = pagedim[2] - margin[1] - margin[3],
    just = c("left", "bottom"),
    name = "figure_area"
  )

  figure_with_margins <- grid::gTree(
    children = grid::gList(figure),
    vp = figure_vp
  )

  ## Create title grob
  titleGrob <- gridtext::textbox_grob(
    title,
    gp = do.call(grid::gpar, title.gpar),
    x = title.pos[1],
    y = title.pos[2],
    hjust = title.just[1],
    vjust = title.just[2],
    maxwidth = pagedim[1] - 2 * title.pos[1])

  ## Create footer grob
  footerGrob <- gridtext::textbox_grob(
    footer,
    gp = do.call(grid::gpar, footer.gpar),
    x = footer.pos[1],
    y = footer.pos[2],
    hjust = footer.just[1],
    vjust = footer.just[2],
    maxwidth = pagedim[1] - 2 * footer.pos[1])

  ## Arrange page with title and footer
  page <- grid::gList(figure_with_margins,
                      titleGrob,
                      footerGrob)

  ## Dimensions
  attr(page, "width")  <- grid::convertUnit(pagedim[[1]], "mm")
  attr(page, "height") <- grid::convertUnit(pagedim[[2]], "mm")

  attr(figure, "width")  <- grid::convertUnit(pagedim[[1]] - margin[[4]] - margin[[2]], "mm")
  attr(figure, "height") <-  grid::convertUnit(pagedim[[2]] - margin[[1]] - margin[[3]], "mm")

  return(list(page = page,
              figure = figure))
}




#' Output plots as files
#'
#' @inheritParams prepare_figure
#' @param filename Name of file to create.
#' @param cropped Name of second output file of the figure without margins or title.
#' @param args List of arguments passed to `ggplot2::ggsave()` for the main figure.
#' @param args_cropped List of arguments passed to `ggplot2::ggsave()` for the cropped figure.
#' @param preview Preview the output in the RStudio Viewer pane. (Default: False)
#' @param ... Other arguments passed to \link{prepare_figure}.
#'
#'
#' @export
#'
save_figure <- function(figure,
                        filename,
                        cropped = NULL,
                        args = NULL,
                        args_cropped = NULL,
                        preview = FALSE,
                        ...){

  # Prepare figure
  figure <- prepare_figure(figure, ...)

  # Save to file
  figargs <- list(filename = filename,
                  plot     = figure$page,
                  width    = attr(figure$page, "width"),
                  height   = attr(figure$page, "height"),
                  units    = "mm",
                  bg       = "transparent")
  if(!is.null(args)){figargs <- utils::modifyList(figargs, args)}

  if (preview) {
    do.call("ggpreview", figargs)
    return(invisible(filename))
  }

  do.call("ggsave", figargs)

  ## Save cropped figure to PNG file
  if (!is.null(cropped)){
    if (!is.character(cropped)) {cli::cli_abort("{.arg cropped} should be a file name.")}
    figargs <- list(filename = cropped,
                    plot   = figure$figure,
                    width  = attr(figure$figure, "width"),
                    height = attr(figure$figure, "height"),
                    units = "mm",
                    bg = "transparent")
    if(!is.null(args_cropped)){figargs <- utils::modifyList(figargs, args_cropped)}
    do.call("ggsave", figargs)
  }

  return(invisible(filename))
}






#' Create a plot preview and display it in the Viewer pane.
#'
#' This function saves a ggplot2 plot to a temporary PNG file and then embeds it in an HTML
#' page, which is opened in the Viewer pane.
#'
#' @details # Device
#' The plot is saved using `ggsave` with the `png` device, regardless of what is specified in
#' the call, so any arguments not used by `ggsave` or `png` are ignored.
#'
#' @param ... Arguments passed to `ggsave` and the PNG device function.
#'
#' @export
#'
ggpreview <- function(...) {
  ## create temporary files
  temp_img <- tempfile()
  temp_html <- tempfile(fileext = ".html")

  ## save using png device
  call <- as.list(match.call())[-1]
  call <- utils::modifyList(call,
                            list(filename = temp_img,
                                 device = ragg::agg_png))
  call <- call[names(call) %in% c(names(formals(ggsave)),
                                  names(formals(ragg::agg_png)))]
  do.call("ggsave", call)

  ## create html file
  html <- c("<!DOCTYPE html>",
            "<html>",
            "<title>Plot preview</title>",
            "</head>",
            "<body>",
            glue::glue("<img src = '{xfun::base64_uri(temp_img, type = 'image/png')}' ",
                       "style='margin: auto; border: 1px lightgrey solid; ",
                       "display: block; ",
                       "background-color: white;",
                       "box-shadow: rgba(100, 100, 111, 0.75) 0px 7px 29px 0px; ",
                       "max-width: 94%; max-height:94vh; height: auto;'"),
            "</body>",
            "</html>")
  con <- file(temp_html, open = "w", encoding = "UTF-8")
  writeLines(
    html,
    con = con,
    sep = "\n")
  close(con)

  ## show html file in Viewer pane
  viewer <- getOption("viewer", default = function(url){})
  viewer(temp_html)
}
