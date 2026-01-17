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

#' code for x axis scale
#' @noRd
shape.scale.x <- function(x) {
  if (!is.null(x$add$scale.x)) {
    return(c('# Set the x-axis scale [scale.x]',
             paste(x$add$scale.x, " +"),
             ''))
  }

  if (x$xscale == "discrete") {
    return(
      c('# Set the x-axis scale [scale.x]',
        glue::glue('scale_x_discrete(drop = FALSE) +'),
        '')
    )
  }

  if (x$xbreaks == "NULL" & x$xscale == "identity") {
    return(NULL)
  }

  args <- dplyr::case_when(
    x$xbreaks == "NULL" & x$xscale != "identity" ~
      glue::glue('transform = "{x$xscale}"'),
    x$xbreaks != "NULL" & x$xscale != "identity" ~
      glue::glue('transform = "{x$xscale}", breaks = {x$xbreaks}'),
    x$xbreaks != "NULL" & x$xscale == "identity" ~
      glue::glue('breaks = {x$xbreaks}')
  )

  c('# Set the x-axis scale [scale.x]',
    glue::glue('scale_x_continuous({args}) +'),
    '')
}

#' code for y axis scale
#' @noRd
shape.scale.y <- function(x) {
  if (!is.null(x$add$scale.y)) {
    return(c('# Set the y-axis scale [scale.y]',
             paste(x$add$scale.y, " +"),
             ''))
  }

  if (x$ybreaks == "NULL" & x$yscale == "identity") {
    return(NULL)
  }

  args <- dplyr::case_when(
    x$ybreaks == "NULL" & x$yscale != "identity" ~
      glue::glue('transform = "{x$yscale}"'),
    x$ybreaks != "NULL" & x$yscale != "identity" ~
      glue::glue('transform = "{x$yscale}", breaks = {x$ybreaks}'),
    x$ybreaks != "NULL" & x$yscale == "identity" ~
      glue::glue('breaks = {x$ybreaks}')
  )

  c('# Set the y-axis scale [scale.y]',
    glue::glue('scale_y_continuous({args}) +'),
    '')
}


#' code for radius scale
#' @noRd
shape.scale.radius <- function(x) {
  if (!is.null(x$add$scale.radius)) {
    return(c('# Set the scale for the size of boxes [scale.radius]',
             paste(x$add$scale.radius, " +"),
             ''))
  }

  make_layer(
    '# Set the scale for the size of boxes [scale.radius]',
    f = "scale_radius",
    arg = c('guide  = "none"',
            'limits = c(0, {x$one_over_minse})',
            'range  = c(0, {x$pointsize})')
  )
}

#' code for shape scale
#' @noRd
shape.scale.shape <- function(x) {
  if (!is.null(x$add$scale.shape)) {
    return(c('# Set the scale for shape [scale.shape]',
             paste(x$add$scale.shape, " +"),
             ''))
  }

  c('# Set the scale for shape [scale.shape]',
    'scale_shape_identity() +',
    '')
}

#' code for colour scale
#' @noRd
shape.scale.colour <- function(x) {
  if (!is.null(x$add$scale.colour)) {
    return(c('# Set the scale for colour [scale.colour]',
             paste(x$add$scale.colour, " +"),
             ''))
  }

  c('# Set the scale for colour [scale.colour]',
    'scale_colour_identity() +',
    '')
}

#' code for fill scale
#' @noRd
shape.scale.fill <- function(x) {
  if (!is.null(x$add$scale.fill)) {
    return(c('# Set the scale for fill [scale.fill]',
             paste(x$add$scale.fill, " +"),
             ''))
  }

  c('# Set the scale for fill [scale.fill]',
    x$scale_fill_string,
    '')
}


#' code for lines
#' @noRd
shape.lines <- function(x) {
  if (x$lines == "none"){return(NULL)}

  if (x$lines == "lmw") {
    make_layer(
      '# Plot lines (linear fit through estimates, weighted by inverse variance) [lines]',
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
              'linewidth = {x$base_line_size/2}')
    )
  } else if (x$lines == "lm") {
    make_layer(
      '# Plot lines (linear fit through estimates, unweighted) [lines]',
      f = "stat_smooth",
      aes = c(x$addaes$lines),
      arg = c(x$addarg$lines,
              'method    = "glm"',
              'formula   = y ~ x',
              'se        = FALSE',
              'colour    = {quote_string(x$plotcolour)}',
              'linetype  = "dashed"',
              'linewidth = {x$base_line_size/2}')
    )
  } else if (x$lines == "connect") {
    make_layer(
      '# Plot lines (connect points) [lines]',
      f = "geom_line",
      aes = c(x$addaes$lines),
      arg = c(x$addarg$lines,
              'colour    = {quote_string(x$plotcolour)}',
              'linetype  = "solid"',
              'linewidth = {x$base_line_size/2}')
    )
  }
}


#' code for points at estimates
#' @noRd
shape.estimates.points <- function(x) {
  if (!is.null(x$add$estimates.points)) {
    return(c('# Plot the point estimates [estimates.points]',
             paste(x$add$estimates.points, " +"),
             ''))
  }

  make_layer(
    '# Plot the point estimates [estimates.points]',
    f = "geom_point",
    aes = c(x$addaes$estimates.points,
            'size   = {x$size}',
            'shape  = {column_name(x$shape$aes)}',
            '{x$fill_string$aes}',
            'colour = {column_name(x$colour$aes)}'),
    arg = c(x$addarg$estimates.points,
            'shape  = {x$shape$arg}',
            'colour = {quote_string(x$colour$arg)}',
            '{x$fill_string$arg}',
            'stroke = {x$stroke}')
  )
}

#' code for text above points
#' @noRd
shape.estimates.text <- function(x) {
  if (!is.null(x$add$estimates.text)) {
    return(c('# Plot point estimates text [estimates.text]',
             paste(x$add$estimates.text, " +"),
             ''))
  }

  if (x$ylims != "NULL") {
    x$uci_string <- glue::glue("pmax({x$ymin}, pmin({x$ymax}, {x$uci_string}))")
  }

  make_layer(
    '# Plot point estimates text [estimates.text]',
    f = "geom_text",
    aes = c(x$addaes$estimates.text,
            'y     = {x$uci_string}',
            'label = format(round({x$est_string}, {x$digits}), nsmall = {x$digits})'),
    arg = c(x$addarg$estimates.text,
            'vjust = -0.8',
            'size  = {x$text_size}',
            'colour = {quote_string(x$plotcolour)}')
  )
}

#' code for text below points
#' @noRd
shape.n.text <- function(x) {
  if (is.null(x$col.n)){return(NULL)}

  if (!is.null(x$add$n.text)) {
    return(c('# Plot n text [n.text]', paste(x$add$n.text, " +"), ''))
  }

  if (x$ylims != "NULL") {
    x$lci_string <- glue::glue("pmin({x$ymax}, pmax({x$ymin}, {x$lci_string}))")
  }

  make_layer(
    '# Plot n text [n.text]',
    f = "geom_text",
    aes = c(x$addaes$n.text,
            'y     = {x$lci_string}',
            'label = {x$col.n}'),
    arg = c(x$addarg$n.text,
            'vjust  = 1.8',
            'size   = {x$text_size}',
            'colour = {quote_string(x$plotcolour)}')
  )
}



#' code for confidence interval lines
#' @noRd
shape.ci <- function(x, type = c("all", "before", "after", "null")) {
  if (type == "null"){return(NULL)}

  if (x$ylims != "NULL") {
    x$lci_string <- glue::glue("pmin({x$ymax}, pmax({x$ymin}, {x$lci_string}))")
    x$uci_string <- glue::glue("pmax({x$ymin}, pmin({x$ymax}, {x$uci_string}))")
  }

  make_layer(
    '# Plot the CIs [ci]',
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

shape.ci.before <- function(x){
  if (!is.null(x$add$ci.before)) {
    return(c('# Plot the CIs [ci.before]',
             paste(x$add$ci.before, " +"),
             ''))
  }
  shape.ci(x, type = x$ci_order[[1]])
}

shape.ci.after <- function(x){
  if (!is.null(x$add$ci.after)) {
    return(c('# Plot the CIs [ci.after]',
             paste(x$add$ci.after, " +"),
             ''))
  }
  shape.ci(x, type = x$ci_order[[2]])
}


#' code to add arrows to CIs
#' @noRd
shape.arrows <- function(x) {
  if (x$ylims == "NULL"){return(NULL)}

  if (!is.null(x$add$arrows)) {
    return(c('# Add tiny segments with arrows when the CIs go outside axis limits [arrows]',
             paste(x$add$arrows, " +"),
             ''))
  }

  make_layer(
    '# Add tiny segments with arrows when the CIs go outside axis limits [arrows]',
    f = 'geom_segment',
    aes = c(x$addaes$ci,
            'y = y',
            'yend = yend',
            'colour = {column_name(x$cicolour$aes)}'),
    arg = c(x$addarg$ci,
            'data      = \\(d) dplyr::bind_rows(dplyr::filter(d, {x$uci_string} > {x$ymax}) |> dplyr::mutate(y = {x$ymax} - 1e-6, yend = {x$ymax})',
            indent(31, 'dplyr::filter(d, {x$lci_string} < {x$ymin}) |> dplyr::mutate(y = {x$ymin} + 1e-6, yend = {x$ymin}))'),
            'colour = {quote_string(x$cicolour$arg)}',
            'linewidth = {x$base_line_size}',
            'arrow     = arrow(type = "closed", length = unit({8 * x$base_line_size}, "pt"))',
            'na.rm     = TRUE')
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
  if (!is.null(x$add$ckb.style)) {
    return(c('# Plot like a CKB plot [ckb.style]',
             paste(x$add$ckb.style, " +"),
             ''))
  }

  make_layer(
    '# Plot like a CKB plot [ckb.style]',
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
            'ink            = {quote_string(x$plotcolour)}',
            'axis.title.margin = {x$axis.title.margin}',
            'plot.margin    = {x$plot.margin}',
            'clip           = {x$clip}')
  )
}

#' code for theme
#' @noRd
shape.theme <- function(x) {
  if (!is.null(x$add$theme)) {
    if (!is.null(x$add$end)) {
      x$add$theme <- paste(x$add$theme, " +")
    }
    return(c('# Add theme [theme]', x$add$theme, ''))
  }

  make_layer(
    '# Add theme [theme]',
    f = "theme",
    arg = c(x$addarg$theme,
            'legend.position = {x$legend.position}'),
    plus = !is.null(x$add$end)
  )
}


#' code to add object at start of ggplot
#' @noRd
shape.add.start <- function(x) {
  if (is.null(x$add$start)){return(NULL)}
  c("# Additional [start]",
    paste(x$add$start, " +"),
    "")
}

#' code to add object at end of ggplot
#' @noRd
shape.add.end <- function(x) {
  if (is.null(x$add$end)){return(NULL)}
  c("# Additional [end]",
    x$add$end,
    "")
}
