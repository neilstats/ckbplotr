#' code for CI colours if using height
#' @noRd
shape.cicolourcode <- function(x) {

  if(!inherits(x$height, "unit")){return(NULL)}

  height.mm <- as.numeric(grid::convertUnit(x$height, "mm"))

  ymin <- x$ymin
  ymax <- x$ymax
  lci_string <- x$lci_string
  uci_string <- x$uci_string
  if (x$scale == "log"){
    ymin <- log(ymin)
    ymax <- log(ymax)
    lci_string <- paste0(x$scale, '(', x$lci_string, ')')
    uci_string <- paste0(x$scale, '(', x$uci_string, ')')
  }

  adjust_size <- (ymax - ymin) * (x$pointsize + 2 * x$stroke) / height.mm

  code <- c(
    '# Create column for CI colour',
    'datatoplot <- datatoplot |>',
    indent(2,
           glue::glue('dplyr::mutate(narrowci = (({uci_string}) - ({lci_string})) <= '),
           indent(26,
                  glue::glue('({x$size})/max({x$size}) * {adjust_size} * dplyr::recode({c(x$shape$arg, column_name(x$shape$aes))}, `22` = sqrt(pi / 4) * 0.7528125, .default = 0.7528125)) |>')),
           'dplyr::mutate(cicolour = dplyr::case_when('))

  if(!is.null(x$col.group)){
    code<- c(code,
             indent(27,
                    glue::glue('as.numeric({x$col.group}) / length(levels({x$col.group})) > 0.5 ~ "black",'),
                    'narrowci ~ "white",',
                    'TRUE     ~ "black"))'),
             '')
  } else {
    code <- c(code,
              indent(27,
                     glue::glue('narrowci ~ {x$cicolours[length(x$cicolours)]},'),
                     glue::glue('TRUE     ~ {x$cicolours[1]}))')),
              '')
  }
  code
}


#' code for CI under if using height
#' @noRd
shape.ciundercode <- function(x) {

  if(!inherits(x$height, "unit")){return(NULL)}

  c('# Create column for CI under',
    'datatoplot <- datatoplot |>',
    indent(2,
           'dplyr::mutate(ciunder =  dplyr::if_else(narrowci, FALSE, TRUE))'),
    '')
}



#' code to start ggplot
#' @noRd
shape.start.ggplot <- function(x) {
  c('# Create the plot with main aesthetics',
    glue::glue('plot <- ggplot(datatoplot, aes(x = {column_name(x$col.x)}, y = {x$est_string}{x$group_string})) +'),
    '')
}

#' code for axis scales
#' @noRd
shape.axes <- function(x) {
  c(if (!is.null(x$xbreaks) && x$xbreaks != "NULL"){
    c('# Set the x-axis scale',
      glue::glue('scale_x_continuous(breaks = {x$xbreaks}) +'),
      '')
  },
  if (x$ybreaks == "NULL" & x$scale != "identity") {
    c('# Set the y-axis scale',
      glue::glue('scale_y_continuous(trans = "{x$scale}") +'),
      '')
  } else if (x$scale != "identity") {
    c('# Set the y-axis scale',
      glue::glue('scale_y_continuous(trans  = "{x$scale}",'),
      glue::glue('                   breaks = {x$ybreaks}) +'),
      '')
  } else if (x$ybreaks != "NULL") {
    c('# Set the y-axis scale',
      glue::glue('scale_y_continuous(breaks = {x$ybreaks}) +'),
      '')
  }
  )
}


#' code for scales
#' @noRd
shape.scales <- function(x) {
  c(
    make_layer(
      '# Set the scale for the size of boxes',
      f = "scale_radius",
      arg = c('guide  = "none"',
              'limits = c(0, {x$one_over_minse})',
              'range  = c(0, {x$pointsize})')
    ),
    '# Use identity for aesthetic scales',
    'scale_shape_identity() +',
    'scale_colour_identity() +',
    x$scale_fill_string,
    '')
}

#' code for lines
#' @noRd
shape.lines <- function(x) {
  if (isFALSE(x$lines)){return(NULL)}
  make_layer(
    '# Plot lines (linear fit through estimates, weighted by inverse variance)',
    f = "stat_smooth",
    aes = c(x$addaes$lines,
            if (!is.null(x$col.lci)) {
              'weight = 1/(({column_name(x$col.estimate)} - {column_name(x$col.lci)})^2)'
            } else {
              'weight = 1/({column_name(x$col.stderr)}^2)'
            }),
    arg = c(x$addarg$lines,
            'method    = "glm"',
            'formula   = y ~ x',
            'se        = FALSE',
            'colour    = {quote_string(x$plotcolour)}',
            'linetype  = "dashed"',
            'linewidth = 0.25')
  )
}


#' code for points at estimates
#' @noRd
shape.estimates.points <- function(x) {
  make_layer(
    '# Plot the point estimates',
    f = "geom_point",
    aes = c(x$addaes$point,
            'size   = {x$size}',
            'shape  = {column_name(x$shape$aes)}',
            '{x$fill_string$aes}',
            'colour = {column_name(x$colour$aes)}'),
    arg = c(x$addarg$point,
            'shape  = {x$shape$arg}',
            'colour = {quote_string(x$colour$arg)}',
            '{x$fill_string$arg}',
            'stroke = {x$stroke}')
  )
}

#' code for text above points
#' @noRd
shape.estimates.text <- function(x) {
  make_layer(
    '# Plot point estimates text',
    f = "geom_text",
    aes = c(x$addaes$estimates,
            'y     = {x$uci_string}',
            'label = format(round({x$est_string}, {x$digits}), nsmall = {x$digits})'),
    arg = c(x$addarg$estimates,
            'vjust = -0.8',
            'size  = {x$text_size}',
            'colour = {quote_string(x$plotcolour)}')
  )
}

#' code for text below points
#' @noRd
shape.n.events.text <- function(x) {
  if (is.null(x$col.n)){return(NULL)}
  make_layer(
    '# Plot n events text',
    f = "geom_text",
    aes = c(x$addaes$n,
            'y     = {x$lci_string}',
            'label = {x$col.n}'),
    arg = c(x$addarg$n,
            'vjust  = 1.8',
            'size   = {x$text_size}',
            'colour = {quote_string(x$plotcolour)}')
  )
}



#' code for confidence interval lines
#' @noRd
shape.cis <- function(x, type = c("all", "before", "after", "null")) {
  if (type == "null"){return(NULL)}
  make_layer(
    '# Plot the CIs',
    f = "geom_linerange",
    aes = c(x$addaes$ci,
            'ymin = {x$lci_string}',
            'ymax = {x$uci_string}',
            'colour = {column_name(x$cicolour$aes)}'),
    arg = c(x$addarg$ci,
            switch(type,
                   "all" = NULL,
                   "before" = 'data = \\(x) dplyr::filter(x, {column_name(x$ciunder)})',
                   "after" = 'data = \\(x) dplyr::filter(x, !{column_name(x$ciunder)})'),
            'colour = {quote_string(x$cicolour$arg)}',
            'linewidth = {x$base_line_size}')
  )
}



#' code for titles
#' @noRd
shape.titles <- function(x) {
  c(
    '# Add titles',
    glue::glue('xlab("{x$xlab}") +'),
    glue::glue('ylab("{x$ylab}") +'),
    if (!is.null(x$title) && !x$title %in% c("", NA)){
      glue::glue('ggtitle("{x$title}") +')
    },
    ''
  )
}

#' code for ckb_style()
#' @noRd

shape.ckb.style <- function(x) {
  make_layer(
    '# Plot like a CKB plot',
    f = "ckbplotr::ckb_style",
    arg = c('xlims          = {x$xlims}',
            'ylims          = {x$ylims}',
            'gap            = {x$gap}',
            'ext            = {x$ext}',
            'ratio          = {x$ratio}',
            'width          = {printunit(x$width)}',
            'height         = {printunit(x$height)}',
            'base_size      = {x$base_size}',
            'base_line_size = {x$base_line_size}',
            'colour         = {quote_string(x$plotcolour)}',
            'axis.title.margin = {x$axis.title.margin}',
            'plot.margin    = {x$plot.margin}',
            'clip           = {x$clip}')
  )
}

#' code for theme
#' @noRd
shape.theme <- function(x) {
  make_layer(
    '# Add theme',
    f = "theme",
    arg = 'legend.position = {x$legend.position}',
    plus = !is.null(x$add$end)
  )
}


#' code to add object at start of ggplot
#' @noRd
shape.add.start <- function(x) {
  if (is.null(x$add$start)){return(NULL)}
  c("# Additional layer",
    paste(c(x$add$start, " +"), collapse = ""),
    "")
}

#' code to add object at end of ggplot
#' @noRd
shape.add.end <- function(x) {
  if (is.null(x$add$end)){return(NULL)}
  c("# Additional layer",
    x$add$end,
    "")
}
