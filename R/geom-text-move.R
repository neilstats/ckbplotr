#' Text that can be moved
#'
#' This geom adds a fixed horizontal and/or vertical move to ggplot2::geom_text()
#'
#' @section Aesthetics:
#'
#' `geom_text_move()` understands the same aesthetics as `ggplot2::geom_text()`
#'
#' @inheritParams ggplot2::geom_text
#' @param move_x Unit value to move text horizontally (Default: unit(0, "pt"))
#' @param move_y Unit value to move text vertically (Default: unit(0, "pt"))
#' @export

geom_text_move <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           parse = FALSE,
                           nudge_x = 0,
                           nudge_y = 0,
                           move_x = unit(0, "pt"),
                           move_y = unit(0, "pt"),
                           check_overlap = FALSE,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextMove,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      move_x = move_x,
      move_y = move_y,
      ...
    )
  )
}

#' @rdname geom_text_move
#' @format NULL
#' @usage NULL
GeomTextMove <- ggproto("GeomTextMove", GeomText,
                        draw_panel = function(data, panel_params, coord, parse = FALSE,
                                              na.rm = FALSE, check_overlap = FALSE,
                                              move_x = unit(0, "pt"),
                                              move_y = unit(0, "pt")) {
                          text_grob <- ggplot2::GeomText$draw_panel(data = data,
                                                                    panel_params = panel_params,
                                                                    coord = coord,
                                                                    parse = parse,
                                                                    na.rm = na.rm,
                                                                    check_overlap = check_overlap)
                          text_grob$x <- text_grob$x + move_x
                          text_grob$y <- text_grob$y + move_y
                          return(text_grob)
                        }
)





# The geom_text_move() function and GeomTextMove ggproto were developed from the
# geom_text() function and GeomText ggproto in the ggplot2 package, found at
# https://github.com/tidyverse/ggplot2
# The ggplot2 package is released with the following license:
#
#
# # MIT License
#
# Copyright (c) 2020 ggplot2 authors
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
