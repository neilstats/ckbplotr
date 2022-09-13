#' Output plots as files
#'
#' @param figure Plot (or graphical object).
#' @param name Name of figure. Used to name the output file(s) and embedded in the PDF document properties Title field.
#' @param title Title to be added to the page. (Default: "")
#' @param titlepos Position of the title text. Default is 1/4 inch from top left of page.
#'    (Default: unit.c(unit(1.27/2, "cm"), unit(1, "npc") - unit(1.27/2, "cm")))
#' @param titlejust Justification of the title text. (Default: c(0, 1))
#' @param margin Margin to be placed around the plot.
#'    Default is 2.27cm top, 1.27cm (1/2 inch) other sides.
#'    (Default: unit(c(2.27, 1.27, 1.27, 1.27), units = "cm"))
#' @param size A unit vector of length two (width, height).
#'    Size of plot (a width/height larger than page weight/height minus margins will be
#'    ignored), centred within margins.
#'    By default, plot will fill the space within margins.
#' @param pagesize Page size of PDF output: "A4" or "A5". (Default: "A4")
#' @param landscape Landscape page orientation? (Default: False)
#' @param pagedim Dimensions (width, height) of PDF output. Overrides pagesize and landscape arguments if used.
#' @param cropped Create a PNG output of the figure without margins or title. (Default: False)
#'
#' @export
#'
save_figure <- function(figure,
                        name,
                        title     = "",
                        titlepos  = grid::unit.c(unit(1.27/2, "cm"),
                                                 unit(1, "npc") - unit(1.27/2, "cm")),
                        titlejust = c(0, 1),
                        margin    = unit(c(2.27, 1.27, 1.27, 1.27), units = "cm"),
                        size      = NULL,
                        pagesize  = c("A4", "A5"),
                        landscape = FALSE,
                        pagedim   = NULL,
                        cropped   = FALSE){

  ## Set page dimensions
  pagesize <- match.arg(pagesize)
  if (missing(pagedim)){
    pagedim <- switch(pagesize,
                      A4 = unit(c(210, 297), "mm"),
                      A5 = unit(c(148, 210), "mm"),
                      stop("Invalid pagesize value."))
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
    margin[[2]] <- margin[[2]] + 0.5 * add_to_width_margins
    margin[[4]] <- margin[[4]] + 0.5 * add_to_width_margins
    add_to_height_margins <- pagedim[[2]] - size[[2]] - margin[[1]] - margin[[3]]
    add_to_height_margins <- grid::unit.pmax(unit(0, "mm"),
                                             grid::convertUnit(add_to_height_margins, "mm"))
    margin[[1]] <- margin[[1]] + 0.5 * add_to_height_margins
    margin[[3]] <- margin[[3]] + 0.5 * add_to_height_margins
  }


  ## Arrange figure with page margins
  ### Layout matrix
  layout <- rbind(c(NA, NA, NA),
                  c(NA, 1, NA),
                  c(NA, NA, NA))

  ## Figure with page margins
  figure_with_margins <- gridExtra::arrangeGrob(
    figure,
    layout_matrix = layout,
    widths = grid::unit.c(margin[4],
                          pagedim[1] - margin[4] - margin[2],
                          margin[2]),
    heights = grid::unit.c(margin[1],
                           pagedim[2] - margin[1] - margin[3],
                           margin[3]))

  ## Create title grob
  titleGrob <- gridtext::textbox_grob(
    title,
    gp = grid::gpar(cex = 1, fontface = "bold"),
    x = titlepos[1],
    y = titlepos[2],
    hjust = titlejust[1],
    vjust = titlejust[2],
    maxwidth = pagedim[1] - 2 * titlepos[1])

  ## Arrange page with title
  page <- gridExtra::arrangeGrob(titleGrob,
                                 figure_with_margins,
                                 nrow = 2,
                                 heights = c(0, 1))


  ## Save to PDF file
  ggsave(paste0(name, ".pdf"),
         plot   = page,
         width  = grid::convertUnit(pagedim[[1]], "mm"),
         height = grid::convertUnit(pagedim[[2]], "mm"),
         units = "mm",
         title  = name)

  ## Save cropped figure to PNG file
  if (cropped){
    ggsave(paste0(name, ".png"),
           plot   = figure,
           width  = grid::convertUnit(pagedim[[1]] - margin[[4]] - margin[[2]], "mm"),
           height = grid::convertUnit(pagedim[[2]] - margin[[1]] - margin[[3]], "mm"),
           units = "mm",
           bg = "transparent")
  }

  invisible(name)
}
