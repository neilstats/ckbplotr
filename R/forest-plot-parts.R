#' code for the axes
#' @noRd
forest.axes <- function(x) {
  c(
    make_layer(
      '# Set coordinate system',
      f = 'coord_cartesian',
      arg = c('clip = "off"',
              'xlim = c({x$xfrom}, {x$xto})')
    ),
    make_layer(
      '# Set the scale for the x axis (the estimates and CIs)',
      f = "scale_x_continuous",
      arg = c('trans  = "{x$axis_scale}"',
              "limits = c({x$xfrom}, {x$xto})",
              "breaks = {paste(deparse(x$xticks), collapse = '')}",
              'expand = c(0,0)')
    ),
    make_layer(
      '# Set the scale for the y axis (the rows)',
      f = "scale_y_continuous",
      arg = c(
        'trans = "reverse"',
        if (!is.null(x$row.labels.heading)){
          c('breaks = c({-x$heading.space}, attr(datatoplot, "rowlabels")$row)',
            'labels = c({quote_string(glue::glue("**{x$row.labels.heading}**"))}, attr(datatoplot, "rowlabels")$row.label)')
        } else {
          c('breaks = attr(datatoplot, "rowlabels")$row',
            'labels = attr(datatoplot, "rowlabels")$row.label')
        },
        'limits = c(max(attr(datatoplot, "rowlabels")$row) + {deparse(x$bottom.space)}, {if (!is.null(x$row.labels.heading)) -{x$heading.space} else "NA"})',
        'expand = c(0,0)')
    )
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
                             '"filled square" ~ sqrt(pi / 4) * 0.7528125,',
                             '.default = 0.7528125))'))
                  } else {
                    switch(as.character(x$shape_list$arg),
                           "22" = "sqrt(pi / 4) * 0.7528125)",
                           "filled square" = "sqrt(pi / 4) * 0.7528125)",
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
  make_layer(
    '# Add diamonds',
    f = 'geom_polygon',
    aes = c(x$addaes$diamonds,
            'x = x, y = row + y, group = row',
            'colour = {column_name(x$colour_list$aes)}',
            'fill = {column_name(x$fill_list$aes)}'),
    arg = c(x$addarg$diamonds,
            'colour = {quote_string(x$colour_list$arg)}',
            'fill = {quote_string(x$fill_list$arg)}',
            'linewidth = {x$diamonds.linewidth}',
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
forest.nullline <- function(x) {
  if (is.null(x$nullval)){return(NULL)}
  make_layer(
    '# Add a line at null effect',
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
}


#' code to plot points
#' @noRd
forest.plot.points <- function(x) {
  c(
    make_layer(
      '# Plot points at the transformed estimates',
      f = 'geom_point',
      aes = c(x$addaes$point,
              if (x$scalepoints){
                'size = size'
              },
              'shape  = {column_name(x$shape_list$aes)}',
              'colour = {column_name(x$colour_list$aes)}',
              'fill   = {c(column_name(x$fill_list$aes), x$fill_list$string_aes)}'),
      arg = c(x$addarg$point,
              'data   = ~ dplyr::filter(.x',
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
    ),
    if (x$scalepoints){
      make_layer(
        c('# Scale the size of points by their side length',
          '# and make the scale range from zero upwards'),
        f = 'scale_radius',
        arg = c('limits = c(0, 1)',
                'range  = c(0, {x$pointsize})')
      )
    }
  )
}

#' code for plotting confidence interval lines
#' @noRd
forest.cis <- function(x, type = c("all", "before", "after", "null")) {
  if (type == "null"){return(NULL)}
  make_layer(
    switch(type,
           "all"    = '# Plot the CIs',
           "before" = '# Plot the CIs - before plotting points',
           "after"  = '# Plot the CIs - after plotting points'),
    f = 'geom_errorbar',
    aes = c(x$addaes$ci,
            'xmin = pmin(pmax(lci_transformed, {x$xfrom}), {x$xto})',
            'xmax = pmin(pmax(uci_transformed, {x$xfrom}), {x$xto})',
            'colour = {c(column_name(x$cicolour_list$aes[1]), x$cicolour_list$string_aes)}'),
    arg = c(x$addarg$ci,
            switch(type,
                   "all"    = 'data = ~ dplyr::filter(.x, !is.na(estimate_transformed), !as_diamond)',
                   "before" = 'data = ~ dplyr::filter(.x, !is.na(estimate_transformed), {x$ciunder}, !as_diamond)',
                   "after"  = 'data = ~ dplyr::filter(.x, !is.na(estimate_transformed), !{x$ciunder}, !as_diamond)'),
            'colour    = {quote_string(x$cicolour_list$arg[1])}',
            'width     = 0',
            'linewidth = {x$base_line_size}',
            'na.rm     = TRUE')
  )
}


#' code for scales
#' @noRd
forest.scales <- function(x) {

  shape_scale_needed <- !is.null(x$shape_list$aes)
  fill_scale_needed <- !is.null(x$fill_list$aes) | !is.null(x$fill_list$string_aes)
  colour_scale_needed <- any(!is.null(x$colour_list$aes),
                             !is.null(x$cicolour_list$aes),
                             !is.null(x$cicolour_list$string_aes))
  if (any(shape_scale_needed,
          fill_scale_needed,
          colour_scale_needed)){
    code <- c(
      '# Use identity for aesthetic scales',
      if (shape_scale_needed) {'scale_shape_identity() +'},
      if (fill_scale_needed) {'scale_fill_identity() +'},
      if (colour_scale_needed) {'scale_colour_identity() +'},
      '')
    return(code)
  }
  return(NULL)
}


#' code to add arrows to CIs
#' @noRd
forest.arrows <- function(x) {
  if (!x$values_outside_xlim){return(NULL)}
  make_layer(
    '# Add tiny segments with arrows when the CIs go outside axis limits',
    f = 'geom_segment',
    aes = c(x$addaes$ci,
            'y      = row',
            'yend   = row',
            'x      = x',
            'xend   = xend',
            'colour = {c(column_name(x$cicolour_list$aes[1]), x$cicolour_list$string_aes)}'),
    arg = c(x$addarg$ci,
            'data      = ~ dplyr::bind_rows(dplyr::filter(.x, uci_transformed > {x$xto}) %>% dplyr::mutate(x = {x$xto} - 1e-6, xend = {x$xto})',
            indent(31, 'dplyr::filter(.x, lci_transformed < {x$xfrom}) %>% dplyr::mutate(x = {x$xfrom} + 1e-6, xend = {x$xfrom}))'),
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
              'y     = - {heading.space}',
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
  c('# Add columns to right side of panels', code)
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
         parse      = FALSE,
         xpos       = x$xfrom,
         addaes     = if(is.null(x$addaes$col.left)){""} else{x$addaes$col.left},
         addarg     = if(is.null(x$addarg$col.left)){""} else{x$addarg$col.left},
         heading.space = x$heading.space,
         text_size  = x$text_size,
         plotcolour = x$plotcolour,
         headingaddaes = if(is.null(x$addaes$heading.left)){""} else{x$addaes$heading.left},
         headingaddarg = if(is.null(x$addarg$heading.left)){""} else{x$addarg$heading.left}),
    forest.columns.code))
  c('# Add columns to left side of panel', code)
}

#' code for addtext
#' @noRd
forest.addtext <- function(x) {
  if (is.null(x$addtext)){return(NULL)}
  make_layer(
    '## addtext',
    f = 'ckbplotr::geom_text_move',
    aes = c(x$addaes$addtext,
            'y = row',
            'x = {x$xto}',
            'label = addtext'),
    arg = c(x$addarg$addtext,
            'move_x = {printunit(x$right.pos[[1]])}',
            'hjust  = {x$right.hjust[[1]]}',
            'size   = {x$text_size}',
            'colour = {quote_string(x$plotcolour)}',
            'na.rm  = TRUE',
            'parse  = TRUE')
  )
}

#' code for horizontal rule under panel headings
#' @noRd
forest.column.headings.rule <- function(x){
  if (!x$heading.rule){return(NULL)}
  make_layer(
    '# Add horizontal rule under column headings',
    f = 'annotation_custom',
    arg = c(
      make_layer(f = 'grob = grid::linesGrob',
                 arg = c('x = unit(c(0, 1), "npc") + c(-1, 0)*{printunit(x$left.space.inner)} + c(0, 1)*{printunit(x$right.space.inner)}',
                         'y = unit(c(-{x$base_size}/8, -{x$base_size}/8), "mm")',
                         'gp = grid::gpar(lwd = {round(x$base_line_size * .stroke / 2, 6)})'),
                 plus = FALSE,
                 br = FALSE),
      'ymin = {x$heading.space}',
      'ymax = {x$heading.space}'
    )
  )
}


#' code for x-axis labels and panel headings
#' @noRd
forest.xlab.panel.headings <- function(x) {
  c(
    make_layer(
      '# Add xlab below each axis',
      f = 'ckbplotr::geom_text_move',
      aes = c(x$addaes$xlab,
              'y = Inf',
              'x = {x$xmid}',
              'label = xlab'),
      arg = c(x$addarg$xlab,
              'hjust    = 0.5',
              'size     = {x$text_size}',
              'colour   = {quote_string(x$plotcolour)}',
              'vjust    = 1',
              'move_y   = unit(-{x$text_size*2.4}, "mm")',
              'fontface = "bold"',
              'data = ~ dplyr::tibble(panel = sort(unique(.[["panel"]]))',
              indent(23, 'xlab = {ds(x$xlab)})'))
    ),
    if (!all(x$panel.headings == "")){
      make_layer(
        '# Add panel name above each panel',
        f = 'ckbplotr::geom_text_move',
        aes = c(c(x$addaes$panel.headings, x$addaes$panel.name),
                'y     = {- x$heading.space}',
                'x     = {x$xmid}',
                'label = title'),
        arg = c(c(x$addarg$panel.headings, x$addarg$panel.name),
                'hjust    = 0.5',
                'vjust    = 0',
                if (x$panel.headings.align == "plot"){
                  'move_x   = ({printunit(x$right.space.inner)} - {printunit(x$left.space.inner)})/2'
                },
                'move_y   = unit({x$text_size*3}, "mm")',
                'size     = {x$text_size}',
                'colour   = {quote_string(x$plotcolour)}',
                'fontface = "bold"',
                'data = ~ dplyr::tibble(panel = sort(unique(.[["panel"]]))',
                indent(23, 'title = {ds(x$panel.headings)})'))
      )
    }
  )
}


#' code to set panel width and/or height
#' @noRd
forest.panel.size <- function(x) {
  if (!x$fixed_panel_width & !x$fixed_panel_height){return(NULL)}
  make_layer(
    '# Fix panel size',
    f = 'ggh4x::force_panelsizes',
    arg = c('cols = {printunit(x$panel.width)}',
            'rows = {printunit(x$panel.height)}'),
    plus = TRUE
  )
}



#' code for the theme
#' @noRd
forest.theme <- function(x) {
  make_layer(
    '# Control the overall look of the plot',
    f = 'theme',
    arg = c('text             = element_text(size = {x$base_size}, colour = {quote_string(x$plotcolour)})',
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
            'axis.text.y      = ggtext::element_markdown(hjust  = 0',
            indent(44,
                   'colour = {quote_string(x$plotcolour)}',
                   'margin = margin(r = {as.numeric(x$left.space)}, unit = {makeunit(x$left.space)}))'),
            'panel.border     = element_blank()',
            'panel.spacing    = {printunit(x$right.space)} + {paste(deparse(x$mid.space), collapse = "")} + {printunit(x$left.space)}',
            'strip.background = element_blank()',
            'strip.placement  = "outside"',
            'strip.text       = element_blank()',
            'legend.position  = "none"',
            'plot.background  = element_blank()',
            'plot.margin      = {paste(deparse(x$plot.margin), collapse = "")} + unit(c({x$space_for_panel_headings}, 0, {2*x$text_size}, 0), "mm") + unit(c(0, {as.numeric(x$right.space)}, 0, 0), {makeunit(x$right.space)})'
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
  c("# Additional layer",
    paste(c(x$add$start, " +"), collapse = ""),
    "")
}

#' code to add object at end of ggplot
#' @noRd
forest.add.end <- function(x) {
  if (is.null(x$add$end)){return(NULL)}
  c("# Additional layer",
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

