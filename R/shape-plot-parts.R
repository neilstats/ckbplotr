#' code for CI colours if using height
#' @noRd
shape.cicolourcode <- function(scale,
                               ylims,
                               lci_string,
                               uci_string,
                               pointsize,
                               size,
                               stroke,
                               height,
                               ratio,
                               gap,
                               ext,
                               shape,
                               cicolours,
                               col.group) {

  if(!inherits(height, "unit")){return(NULL)}

  height.mm <- as.numeric(grid::convertUnit(height, "mm"))

  ymin <- ylims[[1]]
  ymax <- ylims[[2]]
  if (scale == "log"){
    ymin <- log(ymin)
    ymax <- log(ymax)
    lci_string <- paste0(scale, '(', lci_string, ')')
    uci_string <- paste0(scale, '(', uci_string, ')')
  }

  adjust_size <- (ymax - ymin) * (pointsize + 2 * stroke) / height.mm

  x <- c(
    '# Create column for CI colour',
    'datatoplot <- datatoplot %>%',
    indent(2,
           glue::glue('dplyr::mutate(narrowci = (({uci_string}) - ({lci_string})) <= '),
           indent(26,
                  glue::glue('({size})/max({size}) * {adjust_size} * dplyr::recode({c(shape$arg, column_name(shape$aes))}, `22` = 0.6694, .default = 0.7553)) %>%')),
           'dplyr::mutate(cicolour = dplyr::case_when('))

  if(!is.null(col.group)){
    x <- c(x,
           indent(27,
                  glue::glue('as.numeric({col.group}) / length(levels({col.group})) > 0.5 ~ "black",'),
                  'narrowci ~ "white",',
                  'TRUE     ~ "black"))'),
           '')
  } else {
    x <- c(x,
           indent(27,
                  glue::glue('narrowci ~ {cicolours[length(cicolours)]},'),
                  glue::glue('TRUE     ~ {cicolours[1]}))')),
           '')
  }
  x
}


#' code for CI under if using height
#' @noRd
shape.ciundercode <- function(height) {

  if(!inherits(height, "unit")){return(NULL)}

  c('# Create column for CI under',
    'datatoplot <- datatoplot %>%',
    indent(2,
           'dplyr::mutate(ciunder =  dplyr::if_else(narrowci, FALSE, TRUE))'),
    '')
}



#' code to start ggplot
#' @noRd
shape.start.ggplot <- function(col.x, est_string, group_string) {
  c('# Create the plot with main aesthetics',
    glue::glue('plot <- ggplot(datatoplot, aes(x = {col.x}, y = {est_string}{group_string})) +'),
    '')
}

#' code for axis scales
#' @noRd
shape.axes <- function(xbreaks, scale, ybreaks) {
  c(if (!is.null(xbreaks) && xbreaks != "NULL"){
    c('# Set the x-axis scale',
      glue::glue('scale_x_continuous(breaks = {xbreaks}) +'),
      '')
  },
  if (ybreaks == "NULL" & scale != "identity") {
    c('# Set the y-axis scale',
      glue::glue('scale_y_continuous(trans = "{scale}") +'),
      '')
  } else if (scale != "identity") {
    c('# Set the y-axis scale',
      glue::glue('scale_y_continuous(trans  = "{scale}",'),
      glue::glue('                   breaks = {ybreaks}) +'),
      '')
  })
}


#' code for scales
#' @noRd
shape.scales <- function(one_over_minse, pointsize, scale_fill_string) {
  c(make_layer(
    '# Set the scale for the size of boxes',
    f = "scale_radius",
    arg = c('guide  = "none"',
            'limits = c(0, {one_over_minse})',
            'range  = c(0, {pointsize})')
  ),
  '# Use identity for aesthetic scales',
  'scale_shape_identity() +',
  'scale_colour_identity() +',
  scale_fill_string,
  '')
}

#' code for lines
#' @noRd
shape.lines <- function(addaes,
                        col.lci,
                        col.estimate,
                        col.stderr,
                        addarg,
                        plotcolour) {
  make_layer('# Plot lines (linear fit through estimates, weighted by inverse variance)',
             f = "stat_smooth",
             aes = c(addaes$lines,
                     if (!is.null(col.lci)) {
                       'weight = 1/(({column_name(col.estimate)} - {column_name(col.lci)})^2)'
                     } else {
                       'weight = 1/({column_name(col.stderr)}^2)'
                     }),
             arg = c(addarg$lines,
                     'method    = "glm"',
                     'formula   = y ~ x',
                     'se        = FALSE',
                     'colour    = {quote_string(plotcolour)}',
                     'linetype  = "dashed"',
                     'linewidth = 0.25')
  )
}


#' code for points at estimates
#' @noRd
shape.estimates.points <- function(addaes,
                                   size,
                                   shape,
                                   fill_string,
                                   colour,
                                   addarg,
                                   stroke) {
  make_layer(
    '# Plot the point estimates',
    f = "geom_point",
    aes = c(
      addaes$point,
      'size   = {size}',
      'shape  = {column_name(shape$aes)}',
      '{fill_string$aes}',
      'colour = {column_name(colour$aes)}'),
    arg = c(addarg$point,
            'shape  = {shape$arg}',
            'colour = {quote_string(colour$arg)}',
            '{fill_string$arg}',
            'stroke = {stroke}')
  )
}

#' code for text above points
#' @noRd
shape.estimates.text <- function(addaes,
                                 uci_string,
                                 est_string,
                                 addarg,
                                 text_size,
                                 plotcolour,
                                 digits) {
  make_layer(
    '# Plot point estimates text',
    f = "geom_text",
    aes = c(addaes$estimates,
            'y     = {uci_string}',
            'label = format(round({est_string}, {digits}), nsmall = {digits})'),
    arg = c(addarg$estimates,
            'vjust = -0.8',
            'size  = {text_size}',
            'colour = {quote_string(plotcolour)}')
  )
}

#' code for text below points
#' @noRd
shape.n.events.text <- function(addaes,
                                lci_string,
                                col.n,
                                addarg,
                                text_size,
                                plotcolour) {
  make_layer(
    '# Plot n events text',
    f = "geom_text",
    aes = c(addaes$n,
            'y     = {lci_string}',
            'label = {col.n}'),
    arg = c(addarg$n,
            'vjust  = 1.8',
            'size   = {text_size}',
            'colour = {quote_string(plotcolour)}')
  )
}



#' code for confidence interval lines
#' @noRd
shape.cis <- function(addaes,
                      lci_string,
                      uci_string,
                      cicolour,
                      addarg,
                      ciunder,
                      base_line_size,
                      type = c("all", "before", "after", "null")) {
  if (type == "null"){return(NULL)}
  make_layer(
    '# Plot the CIs',
    f = "geom_linerange",
    aes = c(addaes$ci,
            'ymin = {lci_string}',
            'ymax = {uci_string}',
            'colour = {column_name(cicolour$aes)}'),
    arg = c(addarg$ci,
            switch(type,
                   "all" = NULL,
                   "before" = 'data = ~ dplyr::filter(.x, {column_name(ciunder)})',
                   "after" = 'data = ~ dplyr::filter(.x, !{column_name(ciunder)})'),
            'colour = {quote_string(cicolour$arg)}',
            'linewidth = {base_line_size}')
  )
}



#' code for titles
#' @noRd
shape.titles <- function(xlab, title, ylab) {
  c(
    '# Add titles',
    glue::glue('xlab("{xlab}") +'),
    if (!is.null(title) && !title %in% c("", NA)){
      c(glue::glue('ylab("{ylab}") +'),
        glue::glue('ggtitle("{title}")'))
    } else {
      glue::glue('ylab("{ylab}")')
    },
    ''
  )
}

#' code for plot_like_ckb()
#' @noRd
shape.plot.like.ckb <- function(xlims,
                                ylims,
                                gap,
                                ext,
                                ratio,
                                width,
                                height,
                                base_size,
                                base_line_size,
                                plotcolour) {
  make_layer(
    '# Plot like a CKB plot',
    f = "ckbplotr::plot_like_ckb",
    arg = c('plot           = plot',
            'xlims          = {xlims}',
            'ylims          = {ylims}',
            'gap            = {gap}',
            'ext            = {ext}',
            'ratio          = {ratio}',
            'width          = {printunit(width)}',
            'base_size      = {base_size}',
            'base_line_size = {base_line_size}',
            'colour         = {quote_string(plotcolour)}'),
    plus = TRUE
  )
}

#' code for theme
#' @noRd
shape.theme <- function(legend.position) {
  make_layer(
    '# Add theme',
    f = "theme",
    arg = 'legend.position = {deparse(legend.position)}',
    plus = FALSE
  )
}

