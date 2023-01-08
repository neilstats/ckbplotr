#' code for the axes
#' @noRd
forest.axes <- function(scale, xticks, bottom.space) {
  c(
    make_layer(
      '# Set the scale for the x axis (the estimates and CIs)',
      f = "scale_x_continuous",
      arg = c(sprintf('trans  = "%s"', scale),
              paste0("breaks = ",paste(deparse(xticks), collapse = "")),
              'expand = c(0,0)')
    ),
    make_layer(
      '# Set the scale for the y axis (the rows)',
      f = "scale_y_continuous",
      arg = c('breaks = -1:-max(datatoplot$row)',
              'labels = rowlabels',
              sprintf('limits = c(-max(datatoplot$row) - %s, NA)',
                      deparse(bottom.space)),
              'expand = c(0,0)')
    )
  )
}

#' code for row labels vector
#' @noRd
forest.row.labels.vec <- function(bold.labels) {
  c(
    '# Get a character vector of the row labels, so these can be used in the plot',
    'rowlabels <- datatoplot %>%',
    indent(14,
           'dplyr::group_by(row) %>%',
           'dplyr::summarise(row.label = dplyr::first(row.label),',
           indent(17, sprintf('bold = all(is.na(estimate_transformed) | all(key %%in%% %s)),',
                              ds(bold.labels)),
                  '.groups = "drop") %>%'),
           'dplyr::mutate(row.label = dplyr::if_else(bold & row.label != "",',
           indent(41, 'paste0("**", row.label, "**"),',
                  'as.character(row.label))) %>% '),
           'dplyr::arrange(row) %>%',
           'dplyr::pull(row.label)'),
    ''
  )
}

#' code for indentifying CIx that extend outside axis limits
#' @noRd
forest.check.cis <- function(xto, xfrom) {
  c(
    '# Identify any CIs that extend outside axis limits',
    'datatoplot <- datatoplot %>%',
    indent(16,
           sprintf('dplyr::mutate(cioverright  = (uci_transformed > %s),', xto),
           indent(14,
                  sprintf('uci_transformed = pmin(uci_transformed, %s),', xto),
                  sprintf('lci_transformed = pmin(lci_transformed, %s),', xto),
                  sprintf('cioverleft  = (lci_transformed < %s),', xfrom),
                  sprintf('lci_transformed = pmax(lci_transformed, %s),', xfrom),
                  sprintf('uci_transformed = pmax(uci_transformed, %s))', xfrom))),
    '')
}

#' code for CI colours if using panel.width
#' @noRd
forest.cicolourcode <- function(scale,
                                inv_tf,
                                xto,
                                xfrom,
                                pointsize,
                                stroke,
                                panel.width,
                                shape,
                                cicolours,
                                panel.names) {

  if(!inherits(panel.width, "unit")){return(NULL)}
  panel.width.mm <- as.numeric(grid::convertUnit(panel.width, "mm"))

  x <- c(
    '# Create column for CI colour',
    'datatoplot <- datatoplot %>%',
    indent(2,
           sprintf('dplyr::mutate(narrowci =  (%s(uci_transformed) - %s(lci_transformed)) <= ',
                   scale, scale),
           indent(26,
                  sprintf('size * %s * dplyr::recode(%s, `22` = 0.6694, .default = 0.7553)) %%>%%',
                          (inv_tf(xto) - inv_tf(xfrom)) * (pointsize + 2 * stroke) / panel.width.mm, c(shape$arg, column_name(shape$aes)))),
           'dplyr::mutate(cicolour = dplyr::case_when('))

  if(is.list(cicolours)){
    for (i in 1:length(cicolours)){
      x<- c(x,
            indent(27,
                   sprintf('panel == %s & narrowci ~ %s,',
                           quote_string(panel.names[[i]]),
                           cicolours[[i]][length(cicolours[[i]])]),
                   sprintf('panel == %s & !narrowci ~ %s,',
                           quote_string(panel.names[[i]]),
                           cicolours[[i]][1])))
    }
    x <- c(x,
           indent(27, 'TRUE ~ "black"))'),
           '')
  } else {
    x <- c(x,
           indent(27, sprintf('narrowci ~ %s,', cicolours[length(cicolours)]),
                  sprintf('TRUE     ~ %s))', cicolours[1])),
           '')
  }
  x
}

#' code for preparing data when fill is a list
#' @noRd
forest.fillcode <- function(fills, panel.names) {
  x <- c(
    '# Create column for fill colour',
    'datatoplot <- datatoplot %>%',
    indent(2,'dplyr::mutate(fill = dplyr::case_when('))
  for (i in 1:length(fills)){
    x <- c(x,
           indent(25,
                  sprintf('panel == %s ~ %s,',
                          quote_string(panel.names[[i]]),
                          quote_string(fills[[i]][1]))))
  }
  x<- c(x,
        indent(25, 'TRUE ~ "black"))'),
        '')
  x
}

#' code for preparing data for ciunder when using panel.width
#' @noRd
forest.ciundercode <- function(ciunder) {
  c('# Create column for CI under',
    'datatoplot <- datatoplot %>%',
    indent(2,
           sprintf('dplyr::mutate(ciunder =  dplyr::if_else(narrowci, %s, %s))',
                   ciunder[length(ciunder)],
                   ciunder[1])),
    '')
}

#' code for preparing data for diamonds
#' @noRd
forest.diamondscode <- function(diamond, col.diamond, panel.width, cicolours, panel.names) {
  if (!is.null(diamond)){
    x <- c(
      '# Create data frame for diamonds to be plotted',
      'diamonds <- datatoplot %>%',
      indent(2,
             sprintf('dplyr::filter(key %%in%% %s) %%>%%', deparse(diamond)),
             'dplyr::mutate(x1 = lci_transformed,',
             indent(14,
                    'x2 = estimate_transformed,',
                    'x3 = uci_transformed,',
                    'x4 = estimate_transformed) %>%'),
             'tidyr::gather(part, x, x1:x4) %>%',
             'dplyr::arrange(panel, row, part) %>%',
             'dplyr::mutate(y = - row + c(0, -0.25, 0, 0.25))'),
      '',
      '# Remove plotting of points if a diamond is to be used',
      'datatoplot <- datatoplot %>% ',
      indent(2,
             sprintf(
               'dplyr::mutate(estimate_transformed = dplyr::if_else(key %%in%% %s, as.numeric(NA), estimate_transformed),', deparse(diamond)),
             indent(7,
                    sprintf(
                      'lci_transformed = dplyr::if_else(key %%in%% %s, as.numeric(NA), lci_transformed),', deparse(diamond)),
                    sprintf(
                      'uci_transformed = dplyr::if_else(key %%in%% %s, as.numeric(NA), uci_transformed))', deparse(diamond))),
             ''
      ))
  } else {
    x <- c(
      '# Create data frame for diamonds to be plotted',
      'diamonds <- datatoplot %>%',
      indent(2,
             sprintf('dplyr::filter(%s == TRUE) %%>%%', column_name(col.diamond)),
             'dplyr::mutate(x1 = lci_transformed,',
             indent(14,
                    'x2 = estimate_transformed,',
                    'x3 = uci_transformed,',
                    'x4 = estimate_transformed) %>%'),
             'tidyr::gather(part, x, x1:x4) %>%',
             'dplyr::arrange(panel, row, part) %>%',
             'dplyr::mutate(y = - row + c(0, -0.25, 0, 0.25))'),
      '',
      '# Remove plotting of points if a diamond is to be used',
      sprintf('if (any(datatoplot[["%s"]])) {', col.diamond),
      indent(2,
             sprintf('  datatoplot[!is.na(datatoplot[["%s"]]) & datatoplot[["%s"]],]$estimate_transformed <- NA', col.diamond, col.diamond),
             sprintf('  datatoplot[!is.na(datatoplot[["%s"]]) & datatoplot[["%s"]],]$lci_transformed <- NA', col.diamond, col.diamond),
             sprintf('  datatoplot[!is.na(datatoplot[["%s"]]) & datatoplot[["%s"]],]$uci_transformed <- NA', col.diamond, col.diamond)),
      '}',
      ''
    )
  }

  if(inherits(panel.width, "mm") && is.list(cicolours)){
    x <- c(
      x,
      '## Add colour',
      'diamonds <- diamonds %>%',
      indent(2,
             'dplyr::mutate(cicolour = dplyr::case_when(')
    )

    for (i in 1:length(cicolours)){
      x <- c(x,
             indent(27,
                    sprintf('panel == %s ~ %s,',
                            quote_string(panel.names[[i]]),
                            cicolours[[i]][1])))
    }
    x <- c(x,
           indent(27, 'TRUE ~ "black"))'),
           '')
  }
  x
}

#' code for plotting diamonds
#' @noRd
forest.plotdiamondscode <- function(cicolour, fill, stroke) {
  make_layer(
    '# Add diamonds',
    f = 'geom_polygon',
    aes = c('x = x, y = y, group = row',
            sprintf('colour = %s', column_name(cicolour$aes[1])),
            sprintf('fill = %s', column_name(fill$aes))),
    arg = c('data = diamonds',
            sprintf('colour = %s', quote_string(cicolour$arg[1])),
            sprintf('fill = %s', quote_string(fill$arg)),
            sprintf('linewidth = %s', stroke))
  )
}

#' code to start ggplot
#' @noRd
forest.start.ggplot <- function() {
  c(
    '# Create the ggplot',
    'ggplot(datatoplot, aes(y=-row, x=estimate_transformed)) +',
    ''
  )
}

#' code to put panels in facets
#' @noRd
forest.facet <- function() {
  make_layer(
    '# Put the different panels in side-by-side plots using facets',
    f = 'facet_wrap',
    arg = c('~panel, nrow = 1')
  )
}

#' code for line at null
#' @noRd
forest.nullline <- function(nullval, base_line_size, plotcolour) {
  make_layer(
    '# Add a line at null effect',
    f = "annotate",
    arg = c('geom = "segment"',
            'y = -0.7, yend = -Inf',
            sprintf('x = %s, xend = %s', nullval, nullval),
            sprintf('linewidth = %s', base_line_size),
            sprintf('colour = %s', quote_string(plotcolour)))
  )
}


#' code to plot points
#' @noRd
forest.plot.points <- function(addaes,
                               shape,
                               colour,
                               fill,
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
              'size = size',
              sprintf('shape = %s', column_name(shape$aes)),
              sprintf('colour = %s', column_name(colour$aes)),
              sprintf('fill = %s', column_name(fill$aes))),
      arg = c(addarg$point,
              sprintf('data = ~ dplyr::filter(.x, estimate_transformed > %s, estimate_transformed < %s)',
                      xfrom, xto),
              sprintf('shape = %s', shape$arg),
              sprintf('colour = %s', quote_string(colour$arg)),
              sprintf('fill = %s', quote_string(fill$arg)),
              sprintf('stroke = %s', stroke),
              'na.rm = TRUE')
    ),
    make_layer(
      c('# Scale the size of points by their side length',
        '# and make the scale range from zero upwards'),
      f = 'scale_radius',
      arg = c('limits = c(0, 1)',
              sprintf('range = c(0, %s)', pointsize))
    )
  )
}

#' code for plotting confidence interval lines
#' @noRd
forest.cis <- function(addaes, cicolour, addarg, ciunder, base_line_size,
                       type = c("all", "before", "after", "null")) {
  if (type == "null"){return(NULL)}
  make_layer(
    switch(type,
           "all"    = '# Plot the CIs',
           "before" = '# Plot the CIs - before plotting points',
           "after"  = '# Plot the CIs - after plotting points'),
    f = 'geom_errorbar',
    aes = c(addaes$ci,
            'xmin = lci_transformed',
            'xmax = uci_transformed',
            sprintf('colour = %s', column_name(cicolour$aes[1]))),
    arg = c(addarg$ci,
            switch(type,
                   "all" = 'data = ~ dplyr::filter(.x, !is.na(estimate_transformed))',
                   "before" = sprintf('data = ~ dplyr::filter(.x, !is.na(estimate_transformed) & %s)', ciunder),
                   "after" = sprintf('data = ~ dplyr::filter(.x, !is.na(estimate_transformed) & !%s)', ciunder)),
            sprintf('colour = %s', quote_string(cicolour$arg[1])),
            'width = 0',
            sprintf('linewidth = %s', base_line_size),
            'na.rm = TRUE')
  )
}


#' code for scales and coordinates
#' @noRd
forest.scales.coords <- function(xfrom, xto) {
  c(
    '# Use identity for aesthetic scales',
    'scale_shape_identity() +',
    'scale_fill_identity() +',
    'scale_colour_identity() +',
    '',
    make_layer(
      '# Set coordinate system',
      f = 'coord_cartesian',
      arg = c('clip = "off"',
              sprintf('xlim = c(%s, %s)', xfrom, xto))
    )
  )
}


#' code to add arrows to CIs
#' @noRd
forest.arrows <- function(addaes, cicolour, addarg, base_line_size) {
  c(make_layer(
    '# Add tiny segments with arrows when the CIs go outside axis limits',
    f = 'geom_segment',
    aes = c(addaes$ci,
            'y = -row',
            'yend = -row',
            'x = uci_transformed-0.000001',
            'xend = uci_transformed',
            sprintf('colour = %s', column_name(cicolour$aes[1]))),
    arg = c(addarg$ci,
            'data = ~ dplyr::filter(.x, cioverright == TRUE)',
            sprintf('colour = %s', quote_string(cicolour$arg[1])),
            sprintf('linewidth = %s', base_line_size),
            sprintf('arrow = arrow(type = "closed", length = unit(%s, "pt"))', 8 * base_line_size),
            'na.rm = TRUE'),
    br = FALSE
  ),
  make_layer(
    f = 'geom_segment',
    aes = c(addaes$ci,
            'y = -row',
            'yend = -row',
            'x = lci_transformed+0.000001',
            'xend = lci_transformed',
            sprintf('colour = %s', column_name(cicolour$aes[1]))),
    arg = c(addarg$ci,
            'data = ~ dplyr::filter(.x, cioverleft == TRUE)',
            sprintf('colour = %s', quote_string(cicolour$arg[1])),
            sprintf('linewidth = %s', base_line_size),
            sprintf('arrow = arrow(type = "closed", length = unit(%s, "pt"))', 8 * base_line_size),
            'na.rm = TRUE'))
  )
}

#' code for columns to right of panel
#' @noRd
forest.col.right.line <- function(col.right.all,
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
                                  base_size,
                                  plotcolour,
                                  col.heading.space,
                                  panel.names,
                                  tf,
                                  inv_tf) {
  x <- unlist(purrr::pmap(
    list(col.right.all,
         as.numeric(col.right.pos),
         rep(makeunit(col.right.pos), length=length(col.right.pos)),
         col.right.heading,
         col.right.hjust,
         if (is.null(col.bold)) FALSE else col.bold,
         col.right.parse,
         col.right.space,
         if(is.null(addaes$col.right)){""} else{addaes$col.right},
         if(is.null(addarg$col.right)){""} else{addarg$col.right}),
    ~ c(
      make_layer(
        sprintf('## column %s', ..1),
        f = 'ckbplotr::geom_text_move',
        aes = c(..9[..9!=""],
                'y = -row',
                sprintf('x = %s', round(tf(inv_tf(xto) + (inv_tf(xto) - inv_tf(xfrom)) * ..8), 6)),
                if(is.character(..6)){
                  if(..7){
                    sprintf('label = dplyr::if_else(%s & !is.na(%s), paste0("bold(", %s,")"), %s)',
                            column_name(..6), column_name(..6), column_name(..1), column_name(..1))
                  } else {
                    c(sprintf('label = %s', column_name(..1)),
                      sprintf('fontface = dplyr::if_else(%s & !is.na(%s),"bold", "plain")', ..6, ..6))
                  }
                } else {
                  sprintf('label = %s', column_name(..1))
                }),
        arg = c(..10[..10!=""],
                sprintf('move_x = unit(%s, "%s")', ..2, ..3),
                sprintf('hjust = %s', ..5),
                sprintf('size  = %s', base_size/(11/3)),
                sprintf('colour  = %s', quote_string(plotcolour)),
                'na.rm = TRUE',
                sprintf('parse = %s', ..7)),
        br = FALSE
      ),
      make_layer(
        f = 'ckbplotr::geom_text_move',
        aes = c(sprintf('y = %s', col.heading.space),
                sprintf('x = %s', round(tf(inv_tf(xto) + (inv_tf(xto) - inv_tf(xfrom)) * ..8), 6)),
                'label = title'),
        arg = c(sprintf('move_x = unit(%s, "%s")', ..2, ..3),
                sprintf('hjust    = %s', ..5),
                sprintf('size     = %s', base_size/(11/3)),
                sprintf('colour  = %s', quote_string(plotcolour)),
                'fontface = "bold"',
                sprintf('data = dplyr::tibble(panel = factor(%s', paste(deparse(panel.names), collapse = '')),
                indent(36,
                       sprintf('levels = %s', paste(deparse(panel.names), collapse = ''))),
                indent(36,
                       'ordered = TRUE)'),
                indent(21,
                       sprintf('title = %s)', ds(unlist(..4)))))
      )
    )
  )
  )

  c('# Add columns to right side of plots', x)
}

#' code for columns to left of panels
#' @noRd
forest.col.left.line <- function(col.left, col.left.pos, col.left.heading, col.left.hjust, col.bold, col.left.space, addaes, addarg, xfrom, xto, base_size, plotcolour, col.heading.space, panel.names, tf, inv_tf) {
  x <- unlist(purrr::pmap(
    list(col.left,
         as.numeric(col.left.pos),
         rep(makeunit(col.left.pos), length=length(col.left.pos)),
         col.left.heading,
         col.left.hjust,
         if (is.null(col.bold)) FALSE else col.bold,
         col.left.space,
         if(is.null(addaes$col.left)){""} else{addaes$col.left},
         if(is.null(addarg$col.left)){""} else{addarg$col.left}),
    ~ c(
      make_layer(
        sprintf('## column %s', ..1),
        f = 'ckbplotr::geom_text_move',
        aes = c(..8[..8!=""],
                'y = -row',
                sprintf('x = %s', round(tf(inv_tf(xfrom) - (inv_tf(xto) - inv_tf(xfrom)) * ..7), 6)),
                sprintf('label = %s', column_name(..1)),
                if(is.character(..6)){
                  sprintf('fontface = dplyr::if_else(%s & !is.na(%s), "bold", "plain")', column_name(..6), column_name(..6))
                } else {
                  'fontface = "plain"'
                }),
        arg = c(..9[..9!=""],
                sprintf('move_x = unit(-%s, "%s")', ..2, ..3),
                sprintf('hjust = %s', ..5),
                sprintf('size  = %s', base_size/(11/3)),
                sprintf('colour  = %s', quote_string(plotcolour)),
                'na.rm = TRUE'),
        br = FALSE
      ),
      make_layer(
        f = 'ckbplotr::geom_text_move',
        aes = c(sprintf('y = %s', col.heading.space),
                sprintf('x = %s', round(tf(inv_tf(xfrom) - (inv_tf(xto) - inv_tf(xfrom)) * ..7), 6)),
                'label = title'),
        arg = c(sprintf('move_x = unit(-%s, "%s")', ..2, ..3),
                sprintf('hjust    = %s', ..5),
                sprintf('size     = %s', base_size/(11/3)),
                sprintf('colour  = %s', quote_string(plotcolour)),
                'fontface = "bold"',
                sprintf('data = dplyr::tibble(panel = factor(%s', paste(deparse(panel.names), collapse = '')),
                indent(36,
                       sprintf('levels = %s', paste(deparse(panel.names), collapse = ''))),
                indent(36,
                       'ordered = TRUE)'),
                indent(21,
                       sprintf('title = %s)', ds(unlist(..4)))))
      )
    )
  )
  )
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
                           base_size,
                           plotcolour,
                           tf,
                           inv_tf) {
  make_layer(
    '## addtext',
    f = 'ckbplotr::geom_text_move',
    aes = c('y = -row',
            sprintf('x = %s',
                    round(tf(inv_tf(xto) + (inv_tf(xto) - inv_tf(xfrom)) * col.right.space[[1]]),
                          6)),
            if(is.character(col.bold[[1]])){
              if(col.right.parse[[1]]){
                sprintf('label = dplyr::if_else(%s & !is.na(%s), paste0("bold(addtext)"), addtext)',
                        column_name(col.bold[[1]]), column_name(col.bold[[1]]))
              } else {
                c('label = addtext',
                  sprintf('fontface = dplyr::if_else(%s & !is.na(%s),"bold", "plain")',
                          column_name(col.bold[[1]]), column_name(col.bold[[1]])))
              }
            } else {
              'label = addtext'
            }),
    arg = c(sprintf('move_x = unit(%s, "%s")',
                    as.numeric(col.right.pos[[1]]),
                    makeunit(col.right.pos[[1]])),
            sprintf('hjust = %s', col.right.hjust[[1]]),
            sprintf('size  = %s', base_size/(11/3)),
            sprintf('colour  = %s', quote_string(plotcolour)),
            'na.rm = TRUE',
            'parse = TRUE')
  )
}


#' ccode for x-axis labels and panel headings
#' @noRd
forest.xlab.panel.headings <- function(addaes, xmid, addarg, base_size, plotcolour, panel.names, xlab, panel.headings, col.heading.space) {
  c(
    make_layer(
      '# Add xlab below each axis',
      f = 'geom_text',
      aes = c(addaes$xlab,
              sprintf('y = -Inf, x = %s, label = xlab', xmid)),
      arg = c(addarg$xlab,
              'hjust = 0.5',
              sprintf('size  = %s', base_size/(11/3)),
              sprintf('colour  = %s', quote_string(plotcolour)),
              'vjust = 4.4',
              'fontface = "bold"',
              sprintf('data = dplyr::tibble(panel = factor(%s', paste(deparse(panel.names), collapse = '')),
              indent(36, sprintf('levels = %s', paste(deparse(panel.names), collapse = ''))),
              indent(36, 'ordered = TRUE)'),
              indent(21, sprintf('xlab = %s)', ds(xlab))))
    ),
    if (!all(panel.headings == "")){
      make_layer(
        '# Add panel name above each panel',
        f = 'geom_text',
        aes = c(addaes$panel.name,
                sprintf('y = %s, x = %s, label = title', col.heading.space, xmid)),
        arg = c(addarg$panel.name,
                'hjust = 0.5',
                'nudge_y = 2',
                sprintf('size  = %s', base_size/(11/3)),
                sprintf('colour  = %s', quote_string(plotcolour)),
                'fontface = "bold"',
                sprintf('data = dplyr::tibble(panel = factor(%s', paste(deparse(panel.names), collapse = '')),
                indent(36, sprintf('levels = %s', paste(deparse(panel.names), collapse = ''))),
                indent(36, 'ordered = TRUE)'),
                indent(21, sprintf('title = %s)', ds(panel.headings))))
      )
    }
  )
}


#' code to set panel width
#' @noRd
forest.panel.width <- function(panel.width) {
  if(!inherits(panel.width, "unit")){return(NULL)}

  make_layer(
    '# Fix panel width',
    f = 'ggh4x::force_panelsizes',
    arg = sprintf('cols = unit(%s, "%s")',
                  as.numeric(panel.width),
                  makeunit(panel.width)),
    plus = TRUE
  )
}



#' code for the theme
#' @noRd
forest.theme <- function(base_size, plotcolour, base_line_size, title, left.space, right.space, mid.space, plot.margin) {
  make_layer(
    '# Control the overall look of the plots',
    f = 'theme',
    arg = c(sprintf('text             = element_text(size = %s, colour = %s)', base_size, quote_string(plotcolour)),
            sprintf('line             = element_line(linewidth = %s)', base_line_size),
            'panel.background = element_blank()',
            'panel.grid.major = element_blank()',
            'panel.grid.minor = element_blank()',
            if (title == ""){
              'plot.title       = element_blank()'
            } else {
              'plot.title.position = "plot"'
            },
            sprintf('axis.line.x      = element_line(colour = %s, linewidth = %s, lineend = "round")',
                    quote_string(plotcolour), base_line_size),
            'axis.title       = element_blank()',
            sprintf('axis.ticks.x     = element_line(colour = %s)', quote_string(plotcolour)),
            sprintf('axis.text.x      = element_text(colour = %s,', quote_string(plotcolour)),
            indent(32,
                   sprintf('margin = margin(t = %s)',base_size/(11/4.4)),
                   'vjust  = 1)'),
            'axis.ticks.y     = element_blank()',
            'axis.line.y      = element_blank()',
            'axis.text.y      = ggtext::element_markdown(hjust  = 0',
            indent(44,
                   sprintf('colour = %s', quote_string(plotcolour)),
                   sprintf('margin = margin(r = %s, unit = "%s"))',
                           as.numeric(left.space), makeunit(left.space))),
            'panel.border     = element_blank()',
            sprintf('panel.spacing    = unit(%s, "%s") + %s + unit(%s, "%s")',
                    as.numeric(right.space),
                    makeunit(right.space),
                    paste(deparse(mid.space), collapse = ''),
                    as.numeric(left.space),
                    makeunit(left.space)),
            'strip.background = element_blank()',
            'strip.placement  = "outside"',
            'strip.text       = element_blank()',
            'legend.position  = "none"',
            'plot.background  = element_blank()',
            sprintf('plot.margin      = %s + unit(c(0, %s, 0, 0), "%s")',
                    paste(deparse(plot.margin), collapse = ''),
                    as.numeric(right.space),
                    makeunit(right.space))),
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
    arg = sprintf('title = "%s"', title)
  )
}
