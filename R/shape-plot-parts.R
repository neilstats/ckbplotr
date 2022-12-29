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

  x <- c(
    '# Create column for CI colour',
    'datatoplot <- datatoplot %>%',
    indent(2,
           sprintf('dplyr::mutate(narrowci = ((%s) - (%s)) <= ',
                   uci_string, lci_string),
           indent(26,
                  sprintf('(%s)/max(%s) * %s * dplyr::recode(%s, `22` = 0.6694, .default = 0.7553)) %%>%%',
                          size, size,
                          (ymax - ymin) * (pointsize + 2 * stroke) / height.mm,
                          c(shape$arg, shape$aes))),
           'dplyr::mutate(cicolour = dplyr::case_when('))

  if(!is.null(col.group)){
    x <- c(x,
           indent(27,
                  sprintf('as.numeric(%s) / length(levels(%s)) > 0.5 ~ %s,', col.group, col.group, '"black"'),
                  sprintf('narrowci ~ %s,', '"white"'),
                  sprintf('TRUE     ~ %s))', '"black"')),
           '')
  } else {
    x <- c(x,
           indent(27,
                  sprintf('narrowci ~ %s,', cicolours[length(cicolours)]),
                  sprintf('TRUE     ~ %s))', cicolours[1])),
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
    sprintf('plot <- ggplot(datatoplot, aes(x = %s, y = %s%s)) +', col.x, est_string, group_string),
    '')
}

#' code for axis scales
#' @noRd
shape.axes <- function(xbreaks, scale, ybreaks) {
  c(if (!is.null(xbreaks) && xbreaks != "NULL"){
    c('# Set the x-axis scale',
      sprintf('scale_x_continuous(breaks = %s) +', xbreaks),
      '')
  },
  if (ybreaks == "NULL" & scale != "identity") {
    c('# Set the y-axis scale',
      sprintf('scale_y_continuous(trans = "%s") +', scale),
      '')
  } else if (scale != "identity") {
    c('# Set the y-axis scale',
      sprintf('scale_y_continuous(trans  = "%s",', scale),
      sprintf('                   breaks = %s) +', ybreaks),
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
            sprintf('limits = c(0, %s)', one_over_minse),
            sprintf('range  = c(0, %s)', pointsize))
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
                       sprintf('weight = 1/((%s - %s)^2)', col.estimate, fixsp(col.lci))
                     } else {
                       sprintf('weight = 1/(%s^2)', col.stderr)
                     }),
             arg = c(addarg$lines,
                     'method   = "glm"',
                     'formula  = y ~ x',
                     'se       = FALSE',
                     sprintf('colour = %s', plotcolour),
                     'linetype = "dashed"',
                     'size     = 0.25')
  )
}


#' code for points at estimates
#' @noRd
shape.estimates.points <- function(addaes,
                                   scalepoints,
                                   size,
                                   col.lci,
                                   col.estimate,
                                   col.stderr,
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
      sprintf('size = %s', size),
      sprintf('shape = %s', shape$aes),
      sprintf('%s', fill_string$aes),
      sprintf('colour = %s', colour$aes)),
    arg = c(addarg$point,
            sprintf('shape = %s', shape$arg),
            sprintf('colour = %s', colour$arg),
            sprintf('%s', fill_string$arg),
            sprintf('stroke = %s', stroke))
  )
}

#' code for text above points
#' @noRd
shape.estimates.text <- function(addaes,
                                 uci_string,
                                 est_string,
                                 addarg,
                                 base_size,
                                 plotcolour,
                                 digits) {
  make_layer(
    '# Plot point estimates text',
    f = "geom_text",
    aes = c(addaes$estimates,
            sprintf('y = %s', uci_string),
            sprintf('label = format(round(%s, %s), nsmall = %s)',
                    est_string,
                    digits,
                    digits)),
    arg = c(addarg$estimates,
            'vjust = -0.8',
            sprintf('size  = %s', base_size/(11/3)),
            sprintf('colour = %s', plotcolour))
  )
}

#' code for text below points
#' @noRd
shape.n.events.text <- function(addaes,
                                lci_string,
                                col.n,
                                addarg,
                                base_size,
                                plotcolour) {
  make_layer(
    '# Plot n events text',
    f = "geom_text",
    aes = c(addaes$n,
            sprintf('y = %s', lci_string),
            sprintf('label = %s', col.n)),
    arg = c(addarg$n,
            'vjust = 1.8',
            sprintf('size  = %s', base_size/(11/3)),
            sprintf('colour = %s', plotcolour))
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
            sprintf('ymin = %s', lci_string),
            sprintf('ymax = %s', uci_string),
            sprintf('colour = %s', cicolour$aes)),
    arg = c(addarg$ci,
            switch(type,
                   "all" = '',
                   "before" = sprintf('data = ~ dplyr::filter(.x, %s)', fixsp(ciunder)),
                   "after" = sprintf('data = ~ dplyr::filter(.x, !%s)', fixsp(ciunder))),
            sprintf('colour = %s', cicolour$arg),
            sprintf('linewidth = %s', base_line_size))
  )
}



#' code for titles
#' @noRd
shape.titles <- function(xlab, title, ylab) {
  c(
    '# Add titles',
    sprintf('xlab("%s") +', xlab),
    if (!is.null(title) && !title %in% c("", NA)){
      c(sprintf('ylab("%s") +', ylab),
        sprintf('ggtitle("%s")', title))
    } else {
      sprintf('ylab("%s")', ylab)
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
                                height,
                                base_size,
                                base_line_size,
                                plotcolour) {
  make_layer(
    '# Plot like a CKB plot',
    f = "ckbplotr::plot_like_ckb",
    arg = c('plot           = plot',
            sprintf('xlims          = %s', xlims),
            sprintf('ylims          = %s', ylims),
            sprintf('gap            = %s', gap),
            sprintf('ext            = %s', ext),
            sprintf('ratio          = %s', ratio),
            sprintf('height         = unit(%s, "%s")',
                    as.numeric(height),
                    makeunit(height)),
            sprintf('base_size      = %s', base_size),
            sprintf('base_line_size = %s', base_line_size),
            sprintf('colour         = %s', plotcolour)),
    plus = TRUE
  )
}

#' code for theme
#' @noRd
shape.theme <- function(legend.position) {
  make_layer(
    '# Add theme',
    f = "theme",
    arg = c(sprintf('legend.position = %s', legend.position)),
    plus = FALSE
  )
}

