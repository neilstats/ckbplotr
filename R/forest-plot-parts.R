#' code for the axes
#' @noRd
forest.axes <- function(axis_scale,
                        xticks,
                        row.labels.heading,
                        bottom.space,
                        col.heading.space) {
  c(
    make_layer(
      '# Set the scale for the x axis (the estimates and CIs)',
      f = "scale_x_continuous",
      arg = c('trans  = "{axis_scale}"',
              "breaks = {paste(deparse(xticks), collapse = '')}",
              'expand = c(0,0)')
    ),
    make_layer(
      '# Set the scale for the y axis (the rows)',
      f = "scale_y_continuous",
      arg = c(
        'trans = "reverse"',
        if (!is.null(row.labels.heading)){
          c('breaks = c(-{col.heading.space}, attr(datatoplot, "rowlabels")$row)',
            'labels = c({quote_string(glue::glue("**{row.labels.heading}**"))}, attr(datatoplot, "rowlabels")$row.label)')
        } else {
          c('breaks = attr(datatoplot, "rowlabels")$row',
            'labels = attr(datatoplot, "rowlabels")$row.label')
        },
        'limits = c(max(attr(datatoplot, "rowlabels")$row) + {deparse(bottom.space)}, NA)',
        'expand = c(0,0)')
    )
  )
}


#' code for CI colours if using panel.width
#' @noRd
forest.cicolourcode <- function(axis_scale,
                                axis_scale_fn,
                                xto,
                                xfrom,
                                pointsize,
                                stroke,
                                panel.width,
                                shape_list,
                                cicolour_list,
                                panel.names) {

  panel.width.mm <- as.numeric(grid::convertUnit(panel.width, "mm"))
  convert_pointsize <- (axis_scale_fn(xto) - axis_scale_fn(xfrom)) * (pointsize + 2 * stroke) / panel.width.mm

  x <- c(
    '# Create column for CI colour',
    'datatoplot <- datatoplot %>%',
    indent(2,
           'dplyr::mutate(narrowci =  ({axis_scale}(uci_transformed) - {axis_scale}(lci_transformed)) <= ',
           indent(26,
                  'size * {convert_pointsize} * dplyr::recode({c(shape_list$arg, column_name(shape_list$aes))}, `22` = 0.6694, .default = 0.7553)) %>%'),
           'dplyr::mutate(cicolour = dplyr::case_when('))

  if(is.list(cicolour_list$values)){
    for (i in 1:length(cicolour_list$values)){
      x<- c(x,
            indent(27,
                   glue::glue('panel == {quote_string(panel.names[[{i}]])} & narrowci ~ {cicolour_list$values[[{i}]][length(cicolour_list$values[[{i}]])]},'),
                   glue::glue('panel == {quote_string(panel.names[[{i}]])} & !narrowci ~ {cicolour_list$values[[{i}]][1]},')))
    }
    x <- c(x,
           indent(27, 'TRUE ~ "black"))'),
           '')
  } else {
    x <- c(x,
           indent(27,
                  'narrowci ~ {cicolour_list$values[length(cicolour_list$values)]},',
                  'TRUE     ~ {cicolour_list$values[1]}))'),
           '')
  }
  purrr::map_chr(x, glue::glue, .envir = environment())
}

#' code for preparing data when fill is a list
#' @noRd
forest.fillcode <- function(fill_values,
                            panel.names) {
  x <- c(
    '# Create column for fill colour',
    'datatoplot <- datatoplot %>%',
    indent(2,'dplyr::mutate(fill = dplyr::case_when('))
  for (i in 1:length(fill_values)){
    x <- c(x, indent(25, glue::glue('panel == {quote_string(panel.names[[{i}]])} ~ {quote_string(fill_values[[{i}]][1])},')))
  }
  x <- c(x,
         indent(25, 'TRUE ~ "black"))'),
         '')
  purrr::map_chr(x, glue::glue, .envir = environment())
}

#' code for preparing data for ciunder when using panel.width
#' @noRd
forest.ciundercode <- function(ciunder) {
  x <- c('# Create column for CI under',
         'datatoplot <- datatoplot %>%',
         indent(2,
                paste0('dplyr::mutate(ciunder = ',
                       'dplyr::if_else(narrowci, ',
                       '{ciunder[length(ciunder)]}, ',
                       '{ciunder[1]}))')),
         '')
  purrr::map_chr(x, glue::glue, .envir = environment())
}


#' code for plotting diamonds
#' @noRd
forest.plotdiamondscode <- function(colour_list,
                                    fill_list,
                                    diamonds.linewidth) {
  make_layer(
    '# Add diamonds',
    f = 'geom_polygon',
    aes = c('x = x, y = row + y, group = row',
            'colour = {column_name(colour_list$aes)}',
            'fill = {column_name(fill_list$aes)}'),
    arg = c('colour = {quote_string(colour_list$arg)}',
            'fill = {quote_string(fill_list$arg)}',
            'linewidth = {diamonds.linewidth}',
            'data = ~ tidyr::unnest(., diamond_polygon)')
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
forest.facet <- function() {
  make_layer(
    '# Put the different panels in side-by-side plots using facets',
    f = 'facet_wrap',
    arg = c('vars(panel), nrow = 1')
  )
}

#' code for line at null
#' @noRd
forest.nullline <- function(nullval,
                            base_line_size,
                            plotcolour) {
  make_layer(
    '# Add a line at null effect',
    f = "annotate",
    arg = c('geom      = "segment"',
            'y         = 0.7',
            'yend      = Inf',
            'x         = {nullval}',
            'xend      = {nullval}',
            'linewidth = {base_line_size}',
            'colour    = {quote_string(plotcolour)}')
  )
}


#' code to plot points
#' @noRd
forest.plot.points <- function(addaes,
                               shape_list,
                               colour_list,
                               fill_list,
                               addarg,
                               xfrom,
                               xto,
                               stroke,
                               pointsize) {
  c(
    make_layer(
      c('# Plot points at the transformed estimates',
        '## Scale by inverse of the SE'),
      f = 'geom_point',
      aes = c(addaes$point,
              'size   = size',
              'shape  = {column_name(shape_list$aes)}',
              'colour = {column_name(colour_list$aes)}',
              'fill   = {column_name(fill_list$aes)}'),
      arg = c(addarg$point,
              'data   = ~ dplyr::filter(.x, estimate_transformed > {xfrom}, estimate_transformed < {xto}, !as_diamond)',
              'shape  = {shape_list$arg}',
              'colour = {quote_string(colour_list$arg)}',
              'fill   = {quote_string(fill_list$arg)}',
              'stroke = {stroke}',
              'na.rm  = TRUE')
    ),
    make_layer(
      c('# Scale the size of points by their side length',
        '# and make the scale range from zero upwards'),
      f = 'scale_radius',
      arg = c('limits = c(0, 1)',
              'range  = c(0, {pointsize})')
    )
  )
}

#' code for plotting confidence interval lines
#' @noRd
forest.cis <- function(addaes,
                       cicolour_list,
                       addarg,
                       ciunder,
                       base_line_size,
                       xfrom,
                       xto,
                       type = c("all", "before", "after", "null")) {
  if (type == "null"){return(NULL)}
  make_layer(
    switch(type,
           "all"    = '# Plot the CIs',
           "before" = '# Plot the CIs - before plotting points',
           "after"  = '# Plot the CIs - after plotting points'),
    f = 'geom_errorbar',
    aes = c(addaes$ci,
            'xmin = pmin(pmax(lci_transformed, {xfrom}), {xto})',
            'xmax = pmin(pmax(uci_transformed, {xfrom}), {xto})',
            'colour = {column_name(cicolour_list$aes[1])}'),
    arg = c(addarg$ci,
            switch(type,
                   "all"    = 'data = ~ dplyr::filter(.x, !is.na(estimate_transformed), !as_diamond)',
                   "before" = 'data = ~ dplyr::filter(.x, !is.na(estimate_transformed), {ciunder}, !as_diamond)',
                   "after"  = 'data = ~ dplyr::filter(.x, !is.na(estimate_transformed), !{ciunder}, !as_diamond)'),
            'colour    = {quote_string(cicolour_list$arg[1])}',
            'width     = 0',
            'linewidth = {base_line_size}',
            'na.rm     = TRUE')
  )
}


#' code for scales and coordinates
#' @noRd
forest.scales.coords <- function(xfrom,
                                 xto) {
  x <- c(
    '# Use identity for aesthetic scales',
    'scale_shape_identity() +',
    'scale_fill_identity() +',
    'scale_colour_identity() +',
    '',
    make_layer(
      '# Set coordinate system',
      f = 'coord_cartesian',
      arg = c('clip = "off"',
              'xlim = c({xfrom}, {xto})')
    )
  )
}


#' code to add arrows to CIs
#' @noRd
forest.arrows <- function(addaes,
                          cicolour_list,
                          addarg,
                          base_line_size,
                          xfrom,
                          xto) {
  c(make_layer(
    '# Add tiny segments with arrows when the CIs go outside axis limits',
    f = 'geom_segment',
    aes = c(addaes$ci,
            'y      = row',
            'yend   = row',
            'x      = {xto} - 1e-6',
            'xend   = {xto}',
            'colour = {column_name(cicolour_list$aes[1])}'),
    arg = c(addarg$ci,
            'data      = ~ dplyr::filter(.x, uci_transformed > {xto})',
            'colour    = {quote_string(cicolour_list$arg[1])}',
            'linewidth = {base_line_size}',
            'arrow     = arrow(type = "closed", length = unit({8 * base_line_size}, "pt"))',
            'na.rm     = TRUE'),
    br = FALSE
  ),
  make_layer(
    f = 'geom_segment',
    aes = c(addaes$ci,
            'y      = row',
            'yend   = row',
            'x      = {xfrom} + 1e-6',
            'xend   = {xfrom}',
            'colour = {column_name(cicolour_list$aes[1])}'),
    arg = c(addarg$ci,
            'data      = ~ dplyr::filter(.x, lci_transformed < {xfrom})',
            'colour    = {quote_string(cicolour_list$arg[1])}',
            'linewidth = {base_line_size}',
            'arrow     = arrow(type = "closed", length = unit({8 * base_line_size}, "pt"))',
            'na.rm     = TRUE'))
  )
}

forest.col.code <- function(column,
                            pos,
                            heading,
                            hjust,
                            bold,
                            parse,
                            xpos,
                            addaes,
                            addarg,
                            col.heading.space,
                            text_size,
                            plotcolour){
  c(
    make_layer(
      glue::glue('## column {column}'),
      f = 'ckbplotr::geom_text_move',
      aes = c(addaes[addaes!=""],
              'y = row',
              'x = {xpos}',
              if(is.character(bold)){
                if(parse){
                  'label = dplyr::if_else({column_name(bold)} & !is.na({column_name(bold)}), paste0("bold(", {column_name(column)},")"), {column_name(column)})'
                } else {
                  c('label = {column_name(column)}',
                    'fontface = dplyr::if_else({column_name(bold)} & !is.na({column_name(bold)}), "bold", "plain")')
                }
              } else {
                'label = {column_name(column)}'
              }),
      arg = c(addarg[addarg!=""],
              'move_x  = {printunit(pos)}',
              'hjust   = {hjust}',
              'size    = {text_size}',
              'colour  = {quote_string(plotcolour)}',
              'na.rm   = TRUE',
              'parse   = {parse}'),
      br = FALSE),
    make_layer(
      f = 'ckbplotr::geom_text_move',
      aes = c('y     = - {col.heading.space}',
              'x     = {xpos}',
              'label = title'),
      arg = c('move_x  = {printunit(pos)}',
              'hjust    = {hjust}',
              'size     = {text_size}',
              'colour   = {quote_string(plotcolour)}',
              'fontface = "bold"',
              'data = dplyr::tibble(panel = sort(unique(datatoplot$panel))',
              indent(21, 'title = {ds(unlist(heading))})'))
    )
  )
}

#' code for columns to right of panel
#' @noRd
forest.columns.right <- function(col.right.all,
                                 col.right.pos,
                                 col.right.heading,
                                 col.right.hjust,
                                 col.bold,
                                 col.right.parse,
                                 col.right.space,
                                 addaes,
                                 addarg,
                                 xto,
                                 xfrom,
                                 text_size,
                                 plotcolour,
                                 col.heading.space,
                                 axis_scale_fn,
                                 axis_scale_inverse_fn) {
  x <- unlist(purrr::pmap(
    list(column     = col.right.all,
         pos        = col.right.pos ,
         heading    = col.right.heading,
         hjust      = col.right.hjust,
         bold       = if (is.null(col.bold)) FALSE else col.bold,
         parse      = col.right.parse,
         xpos       = round(axis_scale_inverse_fn(axis_scale_fn(xto) + (axis_scale_fn(xto) - axis_scale_fn(xfrom)) * col.right.space), 6),
         addaes     = if(is.null(addaes$col.right)){""} else{addaes$col.right},
         addarg     = if(is.null(addarg$col.right)){""} else{addarg$col.right},
         col.heading.space = col.heading.space,
         text_size  = text_size,
         plotcolour = plotcolour),
    forest.col.code))
  c('# Add columns to right side of plots', x)
}

#' code for columns to left of panels
#' @noRd
forest.columns.left <- function(col.left,
                                col.left.pos,
                                col.left.heading,
                                col.left.hjust,
                                col.bold,
                                col.left.space,
                                addaes,
                                addarg,
                                xfrom,
                                xto,
                                text_size,
                                plotcolour,
                                col.heading.space,
                                axis_scale_fn,
                                axis_scale_inverse_fn) {
  x <- unlist(purrr::pmap(
    list(column     = col.left,
         pos        = - col.left.pos,
         heading    = col.left.heading,
         hjust      = col.left.hjust,
         bold       = if (is.null(col.bold)) FALSE else col.bold,
         parse      = FALSE,
         xpos       = round(axis_scale_inverse_fn(axis_scale_fn(xfrom) - (axis_scale_fn(xto) - axis_scale_fn(xfrom)) * col.left.space), 6),
         addaes     = if(is.null(addaes$col.left)){""} else{addaes$col.left},
         addarg     = if(is.null(addarg$col.left)){""} else{addarg$col.left},
         col.heading.space = col.heading.space,
         text_size  = text_size,
         plotcolour = plotcolour),
    forest.col.code))
  c('# Add columns to left side of plots', x)
}

#' code for addtext
#' @noRd
forest.addtext <- function(xto,
                           xfrom,
                           col.right.space,
                           col.bold,
                           col.right.parse,
                           col.right.pos,
                           col.right.hjust,
                           text_size,
                           plotcolour,
                           axis_scale_fn,
                           axis_scale_inverse_fn) {
  make_layer(
    '## addtext',
    f = 'ckbplotr::geom_text_move',
    aes = c('y = row',
            'x = {round(axis_scale_inverse_fn(axis_scale_fn(xto) + (axis_scale_fn(xto) - axis_scale_fn(xfrom)) * col.right.space[[1]]), 6)}',
            if(is.character(col.bold[[1]])){
              if(col.right.parse[[1]]){
                'label = dplyr::if_else({column_name(col.bold[[1]])} & !is.na({column_name(col.bold[[1]])}), paste0("bold(addtext)"), addtext)'
              } else {
                c('label = addtext',
                  'fontface = dplyr::if_else({column_name(col.bold[[1]])} & !is.na({column_name(col.bold[[1]])}),"bold", "plain")')
              }
            } else {
              'label = addtext'
            }),
    arg = c('move_x = {printunit(col.right.pos[[1]])}',
            'hjust  = {col.right.hjust[[1]]}',
            'size   = {text_size}',
            'colour = {quote_string(plotcolour)}',
            'na.rm  = TRUE',
            'parse  = TRUE')
  )
}


#' code for x-axis labels and panel headings
#' @noRd
forest.xlab.panel.headings <- function(addaes,
                                       xmid,
                                       addarg,
                                       text_size,
                                       plotcolour,
                                       xlab,
                                       panel.headings,
                                       col.heading.space) {
  c(
    make_layer(
      '# Add xlab below each axis',
      f = 'geom_text',
      aes = c(addaes$xlab,
              'y = Inf',
              'x = {xmid},',
              'label = xlab'),
      arg = c(addarg$xlab,
              'hjust    = 0.5',
              'size     = {text_size}',
              'colour   = {quote_string(plotcolour)}',
              'vjust    = 4.4',
              'fontface = "bold"',
              'data = dplyr::tibble(panel = sort(unique(datatoplot$panel))',
              indent(21, 'xlab = {ds(xlab)})'))
    ),
    if (!all(panel.headings == "")){
      make_layer(
        '# Add panel name above each panel',
        f = 'geom_text',
        aes = c(addaes$panel.name,
                'y     = - {col.heading.space}',
                'x     = {xmid}',
                'label = title'),
        arg = c(addarg$panel.name,
                'hjust    = 0.5',
                'nudge_y  = 2',
                'size     = {text_size}',
                'colour   = {quote_string(plotcolour)}',
                'fontface = "bold"',
                'data = dplyr::tibble(panel = sort(unique(datatoplot$panel))',
                indent(21, 'title = {ds(panel.headings)})'))
      )
    }
  )
}


#' code to set panel width and/or height
#' @noRd
forest.panel.size <- function(panel.width = NULL,
                              panel.height = NULL) {
  make_layer(
    '# Fix panel size',
    f = 'ggh4x::force_panelsizes',
    arg = c('cols = {printunit(panel.width)}',
            'rows = {printunit(panel.height)}'),
    plus = TRUE
  )
}



#' code for the theme
#' @noRd
forest.theme <- function(base_size,
                         plotcolour,
                         base_line_size,
                         title,
                         left.space,
                         right.space,
                         mid.space,
                         plot.margin) {
  make_layer(
    '# Control the overall look of the plots',
    f = 'theme',
    arg = c('text             = element_text(size = {base_size}, colour = {quote_string(plotcolour)})',
            'line             = element_line(linewidth = {base_line_size})',
            'panel.background = element_blank()',
            'panel.grid.major = element_blank()',
            'panel.grid.minor = element_blank()',
            if (title == ""){
              'plot.title       = element_blank()'
            } else {
              'plot.title.position = "plot"'
            },
            'axis.line.x      = element_line(colour = {quote_string(plotcolour)}, linewidth = {base_line_size}, lineend = "round")',
            'axis.title       = element_blank()',
            'axis.ticks.x     = element_line(colour = {quote_string(plotcolour)})',
            'axis.text.x      = element_text(colour = {quote_string(plotcolour)},',
            indent(32,
                   'margin = margin(t = {base_size/(11/4.4)})',
                   'vjust  = 1)'),
            'axis.ticks.y     = element_blank()',
            'axis.line.y      = element_blank()',
            'axis.text.y      = ggtext::element_markdown(hjust  = 0',
            indent(44,
                   'colour = {quote_string(plotcolour)}',
                   'margin = margin(r = {as.numeric(left.space)}, unit = "{makeunit(left.space)}"))'),
            'panel.border     = element_blank()',
            'panel.spacing    = {printunit(right.space)} + {paste(deparse(mid.space), collapse = "")} + {printunit(left.space)}',
            'strip.background = element_blank()',
            'strip.placement  = "outside"',
            'strip.text       = element_blank()',
            'legend.position  = "none"',
            'plot.background  = element_blank()',
            'plot.margin      = {paste(deparse(plot.margin), collapse = "")} + unit(c(0, {as.numeric(right.space)}, 0, 0), "{makeunit(right.space)}")'
    ),
    plus = FALSE,
    duplicates = TRUE
  )
}

#' code for title
#' @noRd
forest.title <- function(title) {
  make_layer(
    '# Add the title',
    f = 'labs',
    arg = 'title = "{title}"'
  )
}
