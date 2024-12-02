#' code for the axes
#' @noRd
forest.axes <- function(axis_scale,
                        xfrom,
                        xto,
                        xticks,
                        row.labels.heading,
                        bottom.space,
                        col.heading.space) {
  c(
    make_layer(
      '# Set coordinate system',
      f = 'coord_cartesian',
      arg = c('clip = "off"',
              'xlim = c({xfrom}, {xto})')
    ),
    make_layer(
      '# Set the scale for the x axis (the estimates and CIs)',
      f = "scale_x_continuous",
      arg = c('trans  = "{axis_scale}"',
              "limits = c({xfrom}, {xto})",
              "breaks = {paste(deparse(xticks), collapse = '')}",
              'expand = c(0,0)')
    ),
    make_layer(
      '# Set the scale for the y axis (the rows)',
      f = "scale_y_continuous",
      arg = c(
        'trans = "reverse"',
        if (!is.null(row.labels.heading)){
          c('breaks = c({-col.heading.space}, attr(datatoplot, "rowlabels")$row)',
            'labels = c({quote_string(glue::glue("**{row.labels.heading}**"))}, attr(datatoplot, "rowlabels")$row.label)')
        } else {
          c('breaks = attr(datatoplot, "rowlabels")$row',
            'labels = attr(datatoplot, "rowlabels")$row.label')
        },
        'limits = c(max(attr(datatoplot, "rowlabels")$row) + {deparse(bottom.space)}, {if (!is.null(row.labels.heading)) -{col.heading.space} else "NA"})',
        'expand = c(0,0)')
    )
  )
}

#' code to identify narrows CIs when using panel.width
#' @noRd
forest.narrowci <- function(axis_scale,
                            axis_scale_fn,
                            xto,
                            xfrom,
                            pointsize,
                            scalepoints,
                            stroke,
                            panel.width,
                            shape_list) {
  panel.width.mm <- as.numeric(grid::convertUnit(panel.width, "mm"))
  convert_pointsize <- (axis_scale_fn(xto) - axis_scale_fn(xfrom)) * (pointsize + 2 * stroke) / panel.width.mm
  size <- if (scalepoints) "size" else "1"

  x <- c(
    '# Create column for narrow CIs',
    'datatoplot <- dplyr::mutate(datatoplot,',
    indent(28,
           'narrowci = ({axis_scale}(uci_transformed) - {axis_scale}(lci_transformed)) <= ',
           indent(12,
                  '{size} * ',
                  '{convert_pointsize} *',
                  if (!is.null(shape_list$aes)){
                    c('dplyr::case_match({column_name(shape_list$aes)},',
                      indent(18,
                             '"22" ~ sqrt(pi / 4) * 0.7528125,',
                             '"filled square" ~ sqrt(pi / 4) * 0.7528125,',
                             '.default = 0.7528125))'))
                  } else {
                    switch(as.character(shape_list$arg),
                           "22" = "sqrt(pi / 4) * 0.7528125)",
                           "filled square" = "sqrt(pi / 4) * 0.7528125)",
                           "0.7528125)")
                  }
           ),
           '')
  )
  purrr::map_chr(x, glue::glue, .envir = environment())
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

#' code for plotting diamonds
#' @noRd
forest.plotdiamondscode <- function(colour_list,
                                    fill_list,
                                    diamonds.linewidth,
                                    addaes,
                                    addarg) {
  make_layer(
    '# Add diamonds',
    f = 'geom_polygon',
    aes = c(addaes$diamonds,
            'x = x, y = row + y, group = row',
            'colour = {column_name(colour_list$aes)}',
            'fill = {column_name(fill_list$aes)}'),
    arg = c(addarg$diamonds,
            'colour = {quote_string(colour_list$arg)}',
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
                            plotcolour,
                            addarg) {
  make_layer(
    '# Add a line at null effect',
    f = "annotate",
    arg = c(addarg$nullline,
            'geom      = "segment"',
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
                               pointsize,
                               scalepoints) {
  c(
    make_layer(
      '# Plot points at the transformed estimates',
      f = 'geom_point',
      aes = c(addaes$point,
              if (scalepoints){
                'size = size'
              },
              'shape  = {column_name(shape_list$aes)}',
              'colour = {column_name(colour_list$aes)}',
              'fill   = {c(column_name(fill_list$aes), fill_list$string_aes)}'),
      arg = c(addarg$point,
              'data   = ~ dplyr::filter(.x',
              indent(25, c('estimate_transformed > {xfrom}',
                           'estimate_transformed < {xto}',
                           '!as_diamond)')),
              'shape  = {if (is.numeric(shape_list$arg)) shape_list$arg else quote_string(shape_list$arg)}',
              if (!scalepoints){
                'size   = {pointsize}'
              },
              'colour = {quote_string(colour_list$arg)}',
              'fill   = {quote_string(fill_list$arg)}',
              'stroke = {stroke}',
              'na.rm  = TRUE')
    ),
    if (scalepoints){
      make_layer(
        c('# Scale the size of points by their side length',
          '# and make the scale range from zero upwards'),
        f = 'scale_radius',
        arg = c('limits = c(0, 1)',
                'range  = c(0, {pointsize})')
      )
    }
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
            'colour = {c(column_name(cicolour_list$aes[1]), cicolour_list$string_aes)}'),
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


#' code for scales
#' @noRd
forest.scales <- function(xfrom,
                          xto,
                          shape_list,
                          fill_list,
                          colour_list,
                          cicolour_list) {

  shape_scale_needed <- !is.null(shape_list$aes)
  fill_scale_needed <- !is.null(fill_list$aes) | !is.null(fill_list$string_aes)
  colour_scale_needed <- any(!is.null(colour_list$aes),
                             !is.null(cicolour_list$aes),
                             !is.null(cicolour_list$string_aes))
  if (any(shape_scale_needed,
          fill_scale_needed,
          colour_scale_needed)){
    x <- c(
      '# Use identity for aesthetic scales',
      if (shape_scale_needed) {'scale_shape_identity() +'},
      if (fill_scale_needed) {'scale_fill_identity() +'},
      if (colour_scale_needed) {'scale_colour_identity() +'},
      '')
    return(x)
  }
  return(NULL)
}


#' code to add arrows to CIs
#' @noRd
forest.arrows <- function(addaes,
                          cicolour_list,
                          addarg,
                          base_line_size,
                          xfrom,
                          xto) {
  make_layer(
    '# Add tiny segments with arrows when the CIs go outside axis limits',
    f = 'geom_segment',
    aes = c(addaes$ci,
            'y      = row',
            'yend   = row',
            'x      = x',
            'xend   = xend',
            'colour = {c(column_name(cicolour_list$aes[1]), cicolour_list$string_aes)}'),
    arg = c(addarg$ci,
            'data      = ~ dplyr::bind_rows(dplyr::filter(.x, uci_transformed > {xto}) %>% dplyr::mutate(x = {xto} - 1e-6, xend = {xto})',
            indent(31, 'dplyr::filter(.x, lci_transformed < {xfrom}) %>% dplyr::mutate(x = {xfrom} + 1e-6, xend = {xfrom}))'),
            'colour    = {quote_string(cicolour_list$arg[1])}',
            'linewidth = {base_line_size}',
            'arrow     = arrow(type = "closed", length = unit({8 * base_line_size}, "pt"))',
            'na.rm     = TRUE')
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
      aes = c(headingaddaes[headingaddaes!=""],
              'y     = - {col.heading.space}',
              'x     = {xpos}',
              'label = title'),
      arg = c(headingaddarg[headingaddarg!=""],
              'move_x  = {printunit(pos)}',
              'hjust    = {hjust}',
              'vjust    = 0',
              'size     = {text_size}',
              'colour   = {quote_string(plotcolour)}',
              'fontface = "bold"',
              'lineheight = 1',
              'data = ~ dplyr::tibble(panel = sort(unique(.[["panel"]]))',
              indent(23, 'title = {ds(unlist(heading))})'))
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
         xpos       = xto,
         addaes     = if(is.null(addaes$col.right)){""} else{addaes$col.right},
         addarg     = if(is.null(addarg$col.right)){""} else{addarg$col.right},
         col.heading.space = col.heading.space,
         text_size  = text_size,
         plotcolour = plotcolour,
         headingaddaes = if(is.null(addaes$heading.col.right)){""} else{addaes$heading.col.right},
         headingaddarg = if(is.null(addarg$heading.col.right)){""} else{addarg$heading.col.right}),
    forest.col.code))
  c('# Add columns to right side of panels', x)
}

#' code for columns to left of panels
#' @noRd
forest.columns.left <- function(col.left,
                                col.left.pos,
                                col.left.heading,
                                col.left.hjust,
                                col.bold,
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
         xpos       = xfrom,
         addaes     = if(is.null(addaes$col.left)){""} else{addaes$col.left},
         addarg     = if(is.null(addarg$col.left)){""} else{addarg$col.left},
         col.heading.space = col.heading.space,
         text_size  = text_size,
         plotcolour = plotcolour,
         headingaddaes = if(is.null(addaes$heading.col.left.heading)){""} else{addaes$heading.col.left},
         headingaddarg = if(is.null(addarg$heading.col.left.heading)){""} else{addarg$heading.col.left}),
    forest.col.code))
  c('# Add columns to left side of panel', x)
}

#' code for addtext
#' @noRd
forest.addtext <- function(xto,
                           xfrom,
                           col.right.pos,
                           col.right.hjust,
                           text_size,
                           plotcolour,
                           axis_scale_fn,
                           axis_scale_inverse_fn,
                           addaes,
                           addarg) {
  make_layer(
    '## addtext',
    f = 'ckbplotr::geom_text_move',
    aes = c(addaes$addtext,
            'y = row',
            'x = {xto}',
            'label = addtext'),
    arg = c(addarg$addtext,
            'move_x = {printunit(col.right.pos[[1]])}',
            'hjust  = {col.right.hjust[[1]]}',
            'size   = {text_size}',
            'colour = {quote_string(plotcolour)}',
            'na.rm  = TRUE',
            'parse  = TRUE')
  )
}

#' code for horizontal rule under panel headings
#' @noRd
forest.column.headings.rule <- function(col.heading.space,
                                        left.space.inner,
                                        right.space.inner,
                                        base_size,
                                        base_line_size,
                                        addarg){
  make_layer(
    '# Add horizontal rule under column headings',
    f = 'annotation_custom',
    arg = c(
      make_layer(f = 'grob = grid::linesGrob',
                 arg = c('x = unit(c(0, 1), "npc") + c(-1, 0)*{printunit(left.space.inner)} + c(0, 1)*{printunit(right.space.inner)}',
                         'y = unit(c(-{base_size}/8, -{base_size}/8), "mm")',
                         'gp = grid::gpar(lwd = {round(base_line_size * .stroke / 2, 6)})'),
                 plus = FALSE,
                 br = FALSE),
      'ymin = {col.heading.space}',
      'ymax = {col.heading.space}'
    )
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
                                       panel.headings.align,
                                       col.heading.space,
                                       left.space.inner,
                                       right.space.inner) {
  c(
    make_layer(
      '# Add xlab below each axis',
      f = 'geom_text_move',
      aes = c(addaes$xlab,
              'y = Inf',
              'x = {xmid}',
              'label = xlab'),
      arg = c(addarg$xlab,
              'hjust    = 0.5',
              'size     = {text_size}',
              'colour   = {quote_string(plotcolour)}',
              'vjust    = 1',
              'move_y   = unit(-{text_size*2.4}, "mm")',
              'fontface = "bold"',
              'data = ~ dplyr::tibble(panel = sort(unique(.[["panel"]]))',
              indent(23, 'xlab = {ds(xlab)})'))
    ),
    if (!all(panel.headings == "")){
      make_layer(
        '# Add panel name above each panel',
        f = 'geom_text_move',
        aes = c(c(addaes$panel.headings, addaes$panel.name),
                'y     = {- col.heading.space}',
                'x     = {xmid}',
                'label = title'),
        arg = c(c(addarg$panel.headings, addarg$panel.name),
                'hjust    = 0.5',
                'vjust    = 0',
                if (panel.headings.align == "plot"){
                  'move_x   = ({printunit(right.space.inner)} - {printunit(left.space.inner)})/2'
                },
                'move_y   = unit({text_size*3}, "mm")',
                'size     = {text_size}',
                'colour   = {quote_string(plotcolour)}',
                'fontface = "bold"',
                'data = ~ dplyr::tibble(panel = sort(unique(.[["panel"]]))',
                indent(23, 'title = {ds(panel.headings)})'))
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
                         text_size,
                         title,
                         space_for_panel_headings,
                         left.space,
                         right.space,
                         mid.space,
                         plot.margin,
                         add) {
  make_layer(
    '# Control the overall look of the plot',
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
            'axis.ticks.length.x = unit({base_size/4},  "pt")',
            'axis.text.x      = element_text(colour = {quote_string(plotcolour)}',
            indent(32,
                   'margin = margin(t = {base_size/(11/4.4)})',
                   'vjust  = 1)'),
            'axis.ticks.y     = element_blank()',
            'axis.ticks.length.y = unit(0, "pt")',
            'axis.line.y      = element_blank()',
            'axis.text.y      = ggtext::element_markdown(hjust  = 0',
            indent(44,
                   'colour = {quote_string(plotcolour)}',
                   'margin = margin(r = {as.numeric(left.space)}, unit = {makeunit(left.space)}))'),
            'panel.border     = element_blank()',
            'panel.spacing    = {printunit(right.space)} + {paste(deparse(mid.space), collapse = "")} + {printunit(left.space)}',
            'strip.background = element_blank()',
            'strip.placement  = "outside"',
            'strip.text       = element_blank()',
            'legend.position  = "none"',
            'plot.background  = element_blank()',
            'plot.margin      = {paste(deparse(plot.margin), collapse = "")} + unit(c({space_for_panel_headings}, 0, {2*text_size}, 0), "mm") + unit(c(0, {as.numeric(right.space)}, 0, 0), {makeunit(right.space)})'
    ),
    plus = !is.null(add$end),
    duplicates = TRUE,
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
