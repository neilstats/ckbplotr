#' code for the coord
#' @noRd
forest.coord <- function(x) {
  if (!is.null(x$add$coord)) {
    return(c('# Set coordinate system [coord]', paste(x$add$coord, " +"), ''))
  }

  make_layer(
    '# Set coordinate system [coord]',
    f = 'coord_cartesian',
    arg = c('clip = "off"',
            'xlim = c({x$xfrom}, {x$xto})'))
}


#' code for the x axis scale
#' @noRd
forest.scale.x <- function(x) {
  if (!is.null(x$add$scale.x)) {
    return(c('# Set the scale for the x axis [scale.x]',
             paste(x$add$scale.x, " +"),
             ''))
  }

  make_layer(
    '# Set the scale for the x axis [scale.x]',
    f = "scale_x_continuous",
    arg = c('transform  = "{x$axis_scale}"',
            "limits     = c({x$xfrom}, {x$xto})",
            "breaks     = {deparse1(x$xticks, collapse = '')}",
            'expand     = c(0,0)')
  )
}

#' code for the y axis scale
#' @noRd
forest.scale.y <- function(x) {
  if (!is.null(x$add$scale.y)) {
    return(c('# Set the scale for the y axis [scale.y]',
             paste(x$add$scale.y, " +"),
             ''))
  }

  make_layer(
    '# Set the scale for the y axis [scale.y]',
    f = "scale_y_continuous",
    arg = c(
      'transform = "reverse"',
      if (!is.null(x$row.labels.heading)){
        c('breaks    = c({-x$heading.space}, attr(datatoplot, "rowlabels")$row)',
          'labels    = c({quote_string(glue::glue("**{x$row.labels.heading}**"))}, attr(datatoplot, "rowlabels")$row.label)')
      } else {
        c('breaks    = attr(datatoplot, "rowlabels")$row',
          'labels    = attr(datatoplot, "rowlabels")$row.label')
      },
      'limits    = c(max(attr(datatoplot, "rowlabels")$row) + {deparse1(x$bottom.space)}, {if (!is.null(x$row.labels.heading)) -{x$heading.space} else "NA"})',
      'expand    = c(0,0)')
  )
}

#' code to identify narrows CIs when using panel.width
#' @noRd
forest.narrowci <- function(x) {
  if (!x$fixed_panel_width){return(NULL)}

  panel.width.mm <- as.numeric(grid::convertUnit(x$panel.width, "mm"))
  convert_pointsize <- (x$axis_scale_fn(x$xto) - x$axis_scale_fn(x$xfrom)) * (x$pointsize + 2 * x$stroke) / panel.width.mm
  size <- if (x$scalepoints) "size" else "1"

  code <- c(
    '# Create column for narrow CIs',
    'datatoplot <- dplyr::mutate(datatoplot,',
    indent(28,
           'narrowci = ({x$axis_scale}(uci_transformed) - {x$axis_scale}(lci_transformed)) <= ',
           indent(12,
                  '{size} * ',
                  '{convert_pointsize} *',
                  if (!is.null(x$shape_list$aes)){
                    c('dplyr::case_match({column_name(x$shape_list$aes)},',
                      indent(18,
                             '"22" ~ sqrt(pi / 4) * 0.7528125,',
                             '"square filled" ~ sqrt(pi / 4) * 0.7528125,',
                             '.default = 0.7528125))'))
                  } else {
                    switch(as.character(x$shape_list$arg),
                           "22" = "sqrt(pi / 4) * 0.7528125)",
                           "square filled" = "sqrt(pi / 4) * 0.7528125)",
                           "0.7528125)")
                  }
           ),
           '')
  )
  purrr::map_chr(code, glue::glue, .envir = environment())
}

#' code for CI colours if using panel.width
#' @noRd
forest.cicolour <- function(vals,
                            panel.names) {
  x <- 'dplyr::case_when('
  if (is.list(vals)){
    for (i in 1:length(vals)){
      x<- c(x,
            glue::glue('panel == {quote_string(panel.names[[{i}]])} & narrowci ~ {vals[[{i}]][length(vals[[{i}]])]}, '),
            glue::glue('panel == {quote_string(panel.names[[{i}]])} & !narrowci ~ {vals[[{i}]][1]}, '))
    }
    x <- c(x, 'TRUE ~ "black")')
  } else {
    x <- c(x, 'narrowci ~ {vals[length(vals)]}, TRUE ~ {vals[1]})')
  }
  paste(purrr::map_chr(x, glue::glue, .envir = environment()), collapse = "")
}

#' code for plotting diamonds
#' @noRd
forest.plotdiamondscode <- function(x) {
  if (is.null(x$col.diamond) && is.null(x$diamond)){return(NULL)}

  if (!is.null(x$add$diamonds)) {
    return(c('# Add diamonds [diamonds]', paste(x$add$diamonds, " +"), ''))
  }

  make_layer(
    '# Add diamonds [diamonds]',
    f = 'geom_polygon',
    aes = c(x$addaes$diamonds,
            'x = x, y = row + y, group = row',
            'colour = {column_name(x$colour_list$aes)}',
            'fill = {column_name(x$fill_list$aes)}'),
    arg = c(x$addarg$diamonds,
            'colour = {quote_string(x$colour_list$arg)}',
            'fill = {quote_string(x$fill_list$arg)}',
            'linewidth = {x$diamonds.linewidth}',
            'data = \\(x) tidyr::unnest(x, diamond_polygon)')
  )
}

#' code to start ggplot
#' @noRd
forest.start.ggplot <- function() {
  c(
    '# Create the ggplot',
    'ggplot(datatoplot, aes(y = row, x = estimate_transformed)) +',
    ''
  )
}

#' code to put panels in facets
#' @noRd
forest.facet <- function(x) {
  if (!is.null(x$add$facet)) {
    return(c('# Put the different panels in side-by-side plots using facets [facet]',
             paste(x$add$facet, " +"),
             ''))
  }

  make_layer(
    '# Put the different panels in side-by-side plots using facets [facet]',
    f = 'facet_wrap',
    arg = c('vars(panel), nrow = 1')
  )
}

#' code for line at null
#' @noRd
forest.nullline <- function(x) {
  if (!is.null(x$add$nullline)) {
    return(c('# Add a line at null effect [nullline]',
             paste(x$add$nullline, " +"),
             ''))
  }

  if (is.null(x$nullval)){return(NULL)}
  if (length(x$nullval) == 1) {
    make_layer(
      '# Add a line at null effect [nullline]',
      f = "annotate",
      arg = c(x$addarg$nullline,
              'geom      = "segment"',
              'y         = 0.7',
              'yend      = Inf',
              'x         = {x$nullval}',
              'xend      = {x$nullval}',
              'linewidth = {x$base_line_size}',
              'colour    = {quote_string(x$plotcolour)}')
    )
  } else {
    make_layer(
      '# Add a line at null effect [nullline]',
      f = 'geom_segment',
      aes = c(x$addaes$nullline,
              'y = 0.7',
              'yend = Inf',
              'x = nullval',
              'y = nullval'),
      arg = c(x$addarg$nullline,
              'linewidth = {x$base_line_size}',
              'colour    = {quote_string(x$plotcolour)}',
              'data = \\(x) dplyr::tibble(panel = sort(unique(x[["panel"]]))',
              indent(23, 'nullval = {deparse1(x$nullval)})'))
    )
  }
}


#' code to plot points
#' @noRd
forest.points <- function(x) {
  if (!is.null(x$add$points)) {
    return(c('# Plot the point estimates [points]',
             paste(x$add$points, " +"),
             ''))
  }

  make_layer(
    '# Plot points at the transformed estimates [points]',
    f = 'geom_point',
    aes = c(x$addaes$points,
            if (x$scalepoints){
              'size = size'
            },
            'shape  = {column_name(x$shape_list$aes)}',
            'colour = {c(column_name(x$colour_list$aes), x$colour_list$string_aes)}',
            'fill   = {c(column_name(x$fill_list$aes), x$fill_list$string_aes)}'),
    arg = c(x$addarg$points,
            'data   = \\(x) dplyr::filter(x',
            indent(25, c('estimate_transformed > {x$xfrom}',
                         'estimate_transformed < {x$xto}',
                         '!as_diamond)')),
            'shape  = {if (is.numeric(x$shape_list$arg)) x$shape_list$arg else quote_string(x$shape_list$arg)}',
            if (!x$scalepoints){
              'size   = {x$pointsize}'
            },
            'colour = {quote_string(x$colour_list$arg)}',
            'fill   = {quote_string(x$fill_list$arg)}',
            'stroke = {x$stroke}',
            'na.rm  = TRUE')
  )
}

#' code for radius scale
#' @noRd
forest.scale.radius <- function(x) {
  if (!is.null(x$add$scale.radius)) {
    return(c('# Set the scale for the size of boxes [scale.radius]',
             paste(x$add$scale.radius, " +"),
             ''))
  }

  if (!x$scalepoints){return(NULL)}

  make_layer(
    c('# Set the scale for the size of boxes [scale.radius]',
      '# Scale by  side length and make the scale range from zero upwards'),
    f = 'scale_radius',
    arg = c('limits = c(0, 1)',
            'range  = c(0, {x$pointsize})')
  )
}

#' code for plotting confidence interval lines
#' @noRd
forest.ci <- function(x, type = c("all", "before", "after", "null")) {
  if (type == "null"){return(NULL)}
  make_layer(
    switch(type,
           "all"    = '# Plot the CIs [ci]',
           "before" = '# Plot the CIs - before plotting points [ci]',
           "after"  = '# Plot the CIs - after plotting points [ci]'),
    f = 'geom_errorbar',
    aes = c(x$addaes$ci,
            'xmin = pmin(pmax(lci_transformed, {x$xfrom}), {x$xto})',
            'xmax = pmin(pmax(uci_transformed, {x$xfrom}), {x$xto})',
            'colour = {c(column_name(x$cicolour_list$aes[1]), x$cicolour_list$string_aes)}'),
    arg = c(x$addarg$ci,
            switch(type,
                   "all"    = 'data = \\(x) dplyr::filter(x, !is.na(estimate_transformed), !as_diamond)',
                   "before" = 'data = \\(x) dplyr::filter(x, !is.na(estimate_transformed), {x$ciunder}, !as_diamond)',
                   "after"  = 'data = \\(x) dplyr::filter(x, !is.na(estimate_transformed), !{x$ciunder}, !as_diamond)'),
            'colour    = {quote_string(x$cicolour_list$arg[1])}',
            'width     = 0',
            'linewidth = {x$base_line_size}',
            'na.rm     = TRUE')
  )
}

forest.ci.before <- function(x){
  if (!is.null(x$add$ci.before)) {
    return(c('# Plot the CIs [ci.before]',
             paste(x$add$ci.before, " +"),
             ''))
  }
  forest.ci(x, type = x$ci_order[[1]])
}

forest.ci.after <- function(x){
  if (!is.null(x$add$ci.after)) {
    return(c('# Plot the CIs [ci.after]',
             paste(x$add$ci.after, " +"),
             ''))
  }
  forest.ci(x, type = x$ci_order[[2]])
}

#' code for shape scale
#' @noRd
forest.scale.shape <- function(x) {
  if (!is.null(x$add$scale.shape)) {
    return(c('# Set the scale for shape [scale.shape]',
             paste(x$add$scale.shape, " +"),
             ''))
  }

  if(is.null(x$shape_list$aes)){return(NULL)}

  c('# Set the scale for shape [scale.shape]',
    'scale_shape_identity() +',
    '')
}

#' code for fill scale
#' @noRd
forest.scale.fill <- function(x) {
  if (!is.null(x$add$scale.fill)) {
    return(c('# Set the scale for fill [scale.fill]',
             paste(x$add$scale.fill, " +"),
             ''))
  }

  fill_scale_needed <- !is.null(x$fill_list$aes) | !is.null(x$fill_list$string_aes)
  if(!fill_scale_needed){return(NULL)}

  c('# Set the scale for shape [scale.fill]',
    'scale_fill_identity() +',
    '')
}

#' code for colour scale
#' @noRd
forest.scale.colour <- function(x) {
  if (!is.null(x$add$scale.colour)) {
    return(c('# Set the scale for colour [scale.colour]',
             paste(x$add$scale.colour, " +"),
             ''))
  }

  colour_scale_needed <- any(!is.null(x$colour_list$aes),
                             !is.null(x$colour_list$string_aes),
                             !is.null(x$cicolour_list$aes),
                             !is.null(x$cicolour_list$string_aes))
  if(!colour_scale_needed){return(NULL)}

  c('# Set the scale for colour [scale.colour]',
    'scale_colour_identity() +',
    '')
}

#' code to add arrows to CIs
#' @noRd
forest.arrows <- function(x) {
  if (!x$values_outside_xlim){return(NULL)}
  if (!is.null(x$add$arrows)) {
    return(c('# Add tiny segments with arrows when the CIs go outside axis limits [arrows]',
             paste(x$add$arrows, " +"),
             ''))
  }

  make_layer(
    '# Add tiny segments with arrows when the CIs go outside axis limits [arrows]',
    f = 'geom_segment',
    aes = c(x$addaes$ci,
            'y      = row',
            'yend   = row',
            'x      = x',
            'xend   = xend',
            'colour = {c(column_name(x$cicolour_list$aes[1]), x$cicolour_list$string_aes)}'),
    arg = c(x$addarg$ci,
            'data      = \\(d) dplyr::bind_rows(dplyr::filter(d, uci_transformed > {x$xto}) |> dplyr::mutate(x = {x$xto} - 1e-6, xend = {x$xto})',
            indent(31, 'dplyr::filter(d, lci_transformed < {x$xfrom}) |> dplyr::mutate(x = {x$xfrom} + 1e-6, xend = {x$xfrom}))'),
            'colour    = {quote_string(x$cicolour_list$arg[1])}',
            'linewidth = {x$base_line_size}',
            'arrow     = arrow(type = "closed", length = unit({8 * x$base_line_size}, "pt"))',
            'na.rm     = TRUE')
  )
}

forest.columns.code <- function(column,
                                pos,
                                heading,
                                hjust,
                                bold,
                                parse,
                                xpos,
                                addaes,
                                addarg,
                                heading.space,
                                text_size,
                                plotcolour,
                                headingaddaes,
                                headingaddarg){
  c(
    make_layer(
      glue::glue('## column {column}'),
      f = 'ckbplotr::geom_text_move',
      aes = c(addaes[addaes!=""],
              'y = row',
              'x = {xpos}',
              if(is.character(bold)){
                if(parse %in% c("col", "both")){
                  'label = dplyr::if_else({column_name(bold)} & !is.na({column_name(bold)}), paste0("bold(", {column_name(column)},")"), {column_name(column)})'
                } else {
                  c('label = {column_name(column)}',
                    'fontface = dplyr::if_else({column_name(bold)} & !is.na({column_name(bold)}), "bold", "plain")')
                }
              } else {
                'label = {column_name(column)}'
              }),
      arg = c(addarg[addarg!=""],
              'move_x     = {printunit(pos)}',
              'hjust      = {hjust}',
              'size       = {text_size}',
              'lineheight = 1',
              'colour     = {quote_string(plotcolour)}',
              'na.rm      = TRUE',
              'parse      = {parse %in% c("col", "both")}'),
      br = FALSE),
    make_layer(
      f = 'ckbplotr::geom_text_move',
      aes = c(headingaddaes[headingaddaes!=""],
              'y     = - {heading.space}',
              'x     = {xpos}',
              'label = title'),
      arg = c(headingaddarg[headingaddarg!=""],
              'move_x     = {printunit(pos)}',
              'hjust      = {hjust}',
              'vjust      = 0',
              'size       = {text_size}',
              'colour     = {quote_string(plotcolour)}',
              'fontface   = "bold"',
              'lineheight = 1',
              'parse      = {parse %in% c("heading", "both")}',
              'data = \\(x) dplyr::tibble(panel = sort(unique(x[["panel"]]))',
              indent(23, 'title = {ds(unlist(heading))})'))
    )
  )
}

#' code for columns to right of panel
#' @noRd
forest.columns.right <- function(x) {
  if (is.null(x$col.right.all)){return(NULL)}
  code <- unlist(purrr::pmap(
    list(column     = x$col.right.all,
         pos        = x$right.pos,
         heading    = x$right.heading,
         hjust      = x$right.hjust,
         bold       = if (is.null(x$col.bold)) FALSE else x$col.bold,
         parse      = x$right.parse,
         xpos       = x$xto,
         addaes     = if(is.null(x$addaes$col.right)){""} else{x$addaes$col.right},
         addarg     = if(is.null(x$addarg$col.right)){""} else{x$addarg$col.right},
         heading.space = x$heading.space,
         text_size  = x$text_size,
         plotcolour = x$plotcolour,
         headingaddaes = if(is.null(x$addaes$heading.right)){""} else{x$addaes$heading.right},
         headingaddarg = if(is.null(x$addarg$heading.right)){""} else{x$addarg$heading.right}),
    forest.columns.code))
  c('# Add columns to right side of panels [col.right, heading.right]', code)
}

#' code for columns to left of panels
#' @noRd
forest.columns.left <- function(x) {
  if (is.null(x$col.left)){return(NULL)}
  code <- unlist(purrr::pmap(
    list(column     = x$col.left,
         pos        = - x$left.pos,
         heading    = x$left.heading,
         hjust      = x$left.hjust,
         bold       = if (is.null(x$col.bold)) FALSE else x$col.bold,
         parse      = x$left.parse,
         xpos       = x$xfrom,
         addaes     = if(is.null(x$addaes$col.left)){""} else{x$addaes$col.left},
         addarg     = if(is.null(x$addarg$col.left)){""} else{x$addarg$col.left},
         heading.space = x$heading.space,
         text_size  = x$text_size,
         plotcolour = x$plotcolour,
         headingaddaes = if(is.null(x$addaes$heading.left)){""} else{x$addaes$heading.left},
         headingaddarg = if(is.null(x$addarg$heading.left)){""} else{x$addarg$heading.left}),
    forest.columns.code))
  c('# Add columns to left side of panel [col.left, heading.left]', code)
}

#' code for addtext
#' @noRd
forest.addtext <- function(x) {
  if (is.null(x$addtext)){return(NULL)}
  make_layer(
    '## Add text [addtext]',
    f = 'ckbplotr::geom_text_move',
    aes = c(x$addaes$addtext,
            'y = row',
            'x = {x$xto}',
            'label = addtext'),
    arg = c(x$addarg$addtext,
            'move_x     = {printunit(x$right.pos[[1]])}',
            'hjust      = {x$right.hjust[[1]]}',
            'size       = {x$text_size}',
            'lineheight = 1',
            'colour     = {quote_string(x$plotcolour)}',
            'na.rm      = TRUE',
            'parse      = TRUE')
  )
}

#' code for horizontal rule under panel headings
#' @noRd
forest.column.headings.rule <- function(x){

  if (is.null(x$heading.rule) |
      (is.logical(x$heading.rule) && !x$heading.rule)) {
    return(NULL)
  }

  if (is.logical(x$heading.rule) && x$heading.rule) {
    x_coords <- 'x = unit(c(0, 1), "npc") + c(-1, 0)*{printunit(x$left.space.inner)} + c(0, 1)*{printunit(x$right.space.inner)}'
  } else if (inherits(x$heading.rule, "unit")) {
    if (length(x$heading.rule) == 1) {
      x_coords <- 'x = unit(c(0, 1), "npc") + c(-1, 1)*{printunit(x$heading.rule)}'
    } else {
      x_coords <- 'x = unit(c(0, 1), "npc") + c(-1, 0)*{printunit(x$heading.rule[1])} + c(0, 1)*{printunit(x$heading.rule[2])}'
    }
  } else {
    cli::cli_abort("{.arg heading.rule} must be logical or a grid::unit() object")
  }

  make_layer(
    '# Add horizontal rule under column headings',
    f = 'annotation_custom',
    arg = c(
      make_layer(f = 'grob = grid::linesGrob',
                 arg = c(x_coords,
                         'y = unit(c(-{x$base_size}/8, -{x$base_size}/8), "mm")',
                         'gp = grid::gpar(lwd = {round(x$base_line_size * .stroke / 2, 6)})'),
                 plus = FALSE,
                 br = FALSE),
      'xmin = I(0)',
      'xmax = I(1)',
      'ymin = {x$heading.space}',
      'ymax = {x$heading.space}'
    )
  )
}


#' code for x-axis labels
#' @noRd
forest.xlab <- function(x) {
  if (!is.null(x$add$xlab)) {
    return(c('# Add xlab below each axis [xlab]',
             paste(x$add$xlab, " +"),
             ''))
  }

  make_layer(
    '# Add xlab below each axis [xlab]',
    f = 'ckbplotr::geom_text_move',
    aes = c(x$addaes$xlab,
            'y = Inf',
            'x = {x$xmid}',
            'label = xlab'),
    arg = c(x$addarg$xlab,
            'hjust      = 0.5',
            'size       = {x$text_size}',
            'lineheight = 1',
            'colour     = {quote_string(x$plotcolour)}',
            'vjust      = 1',
            'move_y     = unit(-{x$text_size*2.4}, "mm")',
            'fontface   = "bold"',
            'data = \\(x) dplyr::tibble(panel = sort(unique(x[["panel"]]))',
            indent(23, 'xlab = {ds(x$xlab)})'))
  )
}

#' code for panel headings
#' @noRd
forest.panel.headings <- function(x) {
  if (!is.null(x$add$panel.headings)) {
    return(c('# Add panel name above each panel [panel.headings]',
             paste(x$add$panel.headings, " +"),
             ''))
  }

  if (all(x$panel.headings == "")){return(NULL)}

  make_layer(
    '# Add panel name above each panel [panel.headings]',
    f = 'ckbplotr::geom_text_move',
    aes = c(x$addaes$panel.headings,
            'y     = {- x$heading.space}',
            'x     = {x$xmid}',
            'label = title'),
    arg = c(x$addarg$panel.headings,
            'hjust      = 0.5',
            'vjust      = 0',
            if (x$panel.headings.align == "plot"){
              'move_x     = ({printunit(x$right.space.inner)} - {printunit(x$left.space.inner)})/2'
            },
            'move_y     = unit({x$text_size*3}, "mm")',
            'size       = {x$text_size}',
            'lineheight = 1',
            'colour     = {quote_string(x$plotcolour)}',
            'fontface   = "bold"',
            'data = \\(x) dplyr::tibble(panel = sort(unique(x[["panel"]]))',
            indent(23, 'title = {ds(x$panel.headings)})'))
  )
}


#' code to set panel width and/or height
#' @noRd
forest.panel.size <- function(x) {
  if (!is.null(x$add$panel.size)) {
    return(c('# Fix panel size [panel.size]',
             paste(x$add$panel.size, " +"),
             ''))
  }

  if (!x$fixed_panel_width & !x$fixed_panel_height){return(NULL)}
  make_layer(
    '# Fix panel size [panel.size]',
    f = 'ggh4x::force_panelsizes',
    arg = c('cols = {printunit(x$panel.width)}',
            'rows = {printunit(x$panel.height)}'),
    plus = TRUE
  )
}



#' code for the theme
#' @noRd
forest.theme <- function(x) {
  if (!is.null(x$add$theme)) {
    if (!is.null(x$add$end)) {
      x$add$theme <- paste(x$add$theme, " +")
    }
    return(c('# Control the overall look of the plot [theme]',
             x$add$theme,
             ''))
  }

  make_layer(
    '# Control the overall look of the plot [theme]',
    f = 'theme',
    arg = c(x$addarg$theme,
            'text             = element_text(size = {x$base_size}, colour = {quote_string(x$plotcolour)})',
            'line             = element_line(linewidth = {x$base_line_size})',
            'panel.background = element_blank()',
            'panel.grid.major = element_blank()',
            'panel.grid.minor = element_blank()',
            if (x$title == ""){
              'plot.title       = element_blank()'
            } else {
              'plot.title.position = "plot"'
            },
            'axis.line.x      = element_line(colour = {quote_string(x$plotcolour)}, linewidth = {x$base_line_size}, lineend = "round")',
            'axis.title       = element_blank()',
            'axis.ticks.x     = element_line(colour = {quote_string(x$plotcolour)})',
            'axis.ticks.length.x = unit({x$base_size/4},  "pt")',
            'axis.text.x      = element_text(colour = {quote_string(x$plotcolour)}',
            indent(32,
                   'margin = margin(t = {x$base_size/(11/4.4)})',
                   'vjust  = 1)'),
            'axis.ticks.y     = element_blank()',
            'axis.ticks.length.y = unit(0, "pt")',
            'axis.line.y      = element_blank()',
            'axis.text.y      = {x$row.labels.element}(hjust  = 0',
            indent(44,
                   'colour = {quote_string(x$plotcolour)}',
                   'margin = margin(r = {as.numeric(x$left.space)}, unit = {makeunit(x$left.space)}))'),
            'panel.border     = element_blank()',
            'panel.spacing    = {printunit(x$right.space)} + {deparse1(x$mid.space, collapse = "")} + {printunit(x$left.space)}',
            'strip.background = element_blank()',
            'strip.placement  = "outside"',
            'strip.text       = element_blank()',
            'legend.position  = "none"',
            'plot.background  = element_blank()',
            'plot.margin      = {deparse1(x$plot.margin, collapse = "")} + unit(c({x$space_for_panel_headings}, 0, {2*x$text_size}, 0), "mm") + unit(c(0, {as.numeric(x$right.space)}, 0, 0), {makeunit(x$right.space)})'
    ),
    plus = !is.null(x$add$end),
    duplicates = TRUE,
  )
}

#' code for title
#' @noRd
forest.title <- function(x) {
  if (x$title == ""){return(NULL)}
  make_layer(
    '# Add the title',
    f = 'labs',
    arg = 'title = "{x$title}"'
  )
}


#' code to add object at start of ggplot
#' @noRd
forest.add.start <- function(x) {
  if (is.null(x$add$start)){return(NULL)}
  c("# Add to plot [start]",
    paste(c(x$add$start, " +"), collapse = ""),
    "")
}

#' code to add object at end of ggplot
#' @noRd
forest.add.end <- function(x) {
  if (is.null(x$add$end)){return(NULL)}
  c("# Add to plot [end] ",
    x$add$end,
    "")
}

#' code for user function on datatoplot
#' @noRd
function.data.function <- function(x){
  if (is.null(x$data.function)){return(NULL)}
  c(glue::glue('datatoplot <- {x$data.function}(datatoplot)'),
    '')
}

