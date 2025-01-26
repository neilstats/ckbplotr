#' Prepares data set for a forest plot
#'
#'
#' @param panels
#' A list of data frames. These should include columns or point
#' estimates, and standard errors or confidence interval limits. If you
#' specify a row.labels data frame, then they must also all contain a key column
#' with the same name (which can be specified by col.key).
#' @param col.key
#' Name of column that links the results given in each data frame
#' provided in panels and the labels given in row.labels.
#' If row.labels data frame is not given, then this column will be used as row labels.
#' (Default: "key")
#' @param row.labels
#' A data frame that contains the labels to be used for the
#' rows of the plot. Use NA if a lower level heading is not required for a given row.
#' @param row.labels.levels
#' A character vector. The names of columns in row.labels
#' to use as headings/subheadings/labels for labelling rows.
#' @param rows
#' If set, then only rows matching these labels (at the first level) will be included.
#' @param row.labels.space
#' A numeric vector specifying the space
#' after a row label heading, at the end of a row label heading 'section'. (Default: c(0, 1, 0, 0))
#' @param panel.names
#' A character vector. The names to be used for each forest plot panel.
#' If none provided, then they will be numbered 1, 2, 3 ...
#' @param col.estimate,col.stderr,col.lci,col.uci
#' Names of columns for: point estimates, standard errors, lower and upper limits of confidence intervals.
#' @param col.left,col.right
#' Names of columns to be printed to the left/right of the plot.
#' @param col.keep
#' Names of additional columns to be kept in returned data frame.
#' @param ci.delim
#' Character string to separate lower and upper limits of confidence interval. (Default: ", ")
#' @param digits
#' Number of digits after decimal point to show for estimates and confidence intervals. (Default: 2)
#' @param exponentiate
#' Exponentiate estimates (and CIs) before plotting. (Default: TRUE)
#' @param scalepoints
#' Should the points be scaled by inverse of the standard error? (Default: FALSE)
#' @param minse
#' Minimum standard error to use when scaling point size. (Default will use minimum in the data.)
#' @param addtext
#' A list of data frames. List must be the same length as panels.
#' Data frames should contain a column with the name specified in col.key,
#' and one or more of:
#'
#' 1. a column named 'text' containing character strings
#'
#' 2. columns named 'het_dof', 'het_stat', and 'het_p' containing character strings
#'
#' 3. columns names 'trend_stat' and 'trend_p' containing character strings
#'
#' The character strings, heterogeneity test, and trend test results will
#' be plotted in the column of estimates and CIs, below the row with the key
#' given in the col.key column.
#' @param col.diamond
#' Plot estimates and CIs as diamonds. Name of a column of logical values.
#' @param diamond
#' Alternative to col.diamond. A character vectors identify the rows
#' (using the key values) for which the estimate and CI should be plotted using a diamond.
#' @param bold.labels
#' A character vector identifying row labels (using key values) which should additionally be bold. (Default: NULL)
#'
#' @return A dataset from which a forest plot can be generated.
#'
#'
#' @keywords internal
#'
#' @importFrom rlang .data
#' @importFrom utils compareVersion
#' @importFrom utils packageVersion
#'
#' @export

forest_data <- function(
    panels,
    panel.names       = NULL,
    col.key           = "key",
    col.estimate      = "estimate",
    col.stderr        = "stderr",
    col.lci           = NULL,
    col.uci           = NULL,
    col.left          = NULL,
    col.right         = NULL,
    col.keep          = NULL,
    row.labels        = NULL,
    row.labels.levels = NULL,
    rows              = NULL,
    row.labels.space  = c(0, 1, 0, 0),
    ci.delim          = ", ",
    digits            = 2,
    exponentiate      = TRUE,
    scalepoints       = FALSE,
    minse             = NULL,
    addtext           = NULL,
    diamond           = NULL,
    col.diamond       = NULL,
    bold.labels       = NULL
){

  # check function arguments
  if (is.data.frame(panels)) {
    panels <- list(panels)
  }
  if (is.data.frame(addtext)) {
    addtext <- list(addtext)
  }

  ## check columns in addtext are character
  for (addtextframe in addtext){
    for (textcol in c("text", "expr", "het_dof", "het_stat", "het_p", "trend_stat", "trend_p")){
      if (!is.null(addtextframe[[textcol]]) && !is.character(addtextframe[[textcol]])){
        rlang::abort(glue::glue("'{textcol}' in addtext is not character"))
      }
    }
  }

  if (is.null(panel.names)) { panel.names <- as.character(1:length(panels)) }

  column_names_in_data <- purrr::reduce(lapply(panels, names), intersect)

  if (!col.key %in% column_names_in_data) {
    if (col.key != "key") {
      rlang::inform(glue::glue("col.key '{col.key}' not found, using row number as row labels."))
      col.key <- "key"
    }
    for (i in seq_along(panels)){
      panels[[i]][["key"]] <- seq_len(nrow(panels[[i]]))
    }
  }

  if (is.null(row.labels.levels)) {
    row.labels.levels <- names(row.labels)
    row.labels.levels <- row.labels.levels[!row.labels.levels == col.key]
  }

  if (!is.null(col.lci) && is.null(col.uci)) {
    rlang::abort("col.lci and col.uci must both be specified")
  }
  if (is.null(col.lci) && !is.null(col.uci)){
    rlang::abort("col.lci and col.uci must both be specified")
  }
  if (!is.character(panel.names)) {
    rlang::abort("panel.names must be a character vector")
  }
  if (!all(!duplicated(panel.names))) {
    rlang::abort("panel.names must be unique")
  }
  if (length(panels) != length(panel.names)) {
    rlang::abort("panels and panel.names must be the same length")
  }
  if (!(length(row.labels.space) >= 2*(length(row.labels.levels) - 1))) {
    rlang::abort("row.labels.space must be at least 2*(length(row.labels.levels)-1)")
  }
  if (!is.null(row.labels) && !all(row.labels.levels %in% names(row.labels))) {
    rlang::abort("row.labels.levels must be columns in row.labels")
  }
  if(!is.null(row.labels) && !all(sapply(row.labels[row.labels.levels], is.character))) {
    rlang::abort("row.labels.levels columns must be character")
  }


  # filter row.labels according to rows argument
  if (!is.null(rows)) {
    row.labels <- row.labels %>%
      dplyr::filter(.data[[row.labels.levels[[1]]]] %in% rows)
  }

  # Make vector of keys after which extra rows are added for addtext
  addtextcols <- tibble::tibble(text = character(),
                                expr = character(),
                                het_dof = character(),
                                het_stat = character(),
                                het_p = character(),
                                trend_stat = character(),
                                trend_p = character())
  extrarowkeys <- c()
  if (!is.null(addtext)) {
    for (i in 1:length(addtext)) {
      addtext[[i]] <- dplyr::bind_rows(addtextcols, addtext[[i]]) %>%
        dplyr::mutate(addtext = dplyr::case_when(
          !is.na(text) ~ paste0("'", text, "'"),
          !is.na(expr) ~ expr,
          !is.na(het_stat) ~ make_heterogeneity_string(het_dof, het_stat, het_p),
          !is.na(trend_stat) ~ make_trend_string(trend_stat, trend_p)
        )) %>%
        dplyr::select(key = !!rlang::sym(col.key),
                      .data$addtext) %>%
        dplyr::mutate(key = as.character(.data$key)) %>%
        dplyr::group_by(.data$key) %>%
        dplyr::mutate(addtextrow = 1:dplyr::n() - 1) %>%
        dplyr::ungroup()
    }
    extrarowkeys <- purrr::reduce(purrr::map(addtext,
                                             ~ dplyr::count(., .data$key)),
                                  dplyr::bind_rows) %>%
      dplyr::group_by(.data$key) %>%
      dplyr::summarise(n = max(.data$n))
    extrarowkeys <- rep(extrarowkeys$key, extrarowkeys$n)
  }

  # create data frame of row numbers and labels
  if (is.null(row.labels)) {

    keys <- unique(unlist(lapply(panels, function(x) x[[col.key]])))
    out <- tibble::tibble(row.label = keys,
                          key = keys,
                          row.height = NA,
                          spacing_row = FALSE) %>%
      dplyr::select(.data$row.label, .data$key, .data$row.height, .data$spacing_row)
  } else {

    if (!col.key %in% names(row.labels)) rlang::abort(glue::glue("{col.key} must be a column in {deparse(substitute(row.labels))}"))

    for (panel in panels) {
      if (!col.key %in% names(panel)) rlang::abort(glue::glue("{col.key} must be a column in every data frame given in panels"))
    }

    ## number of levels of row labels
    n_row_label_levels <- length(row.labels.levels)

    ## add key column
    row.labels <- row.labels %>%
      dplyr::mutate(key = !!sym(col.key))

    ## make sure levels with smaller indices are not NA
    for (i in 1:n_row_label_levels){
      row.labels <- row.labels %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          "row_label_level_{i}" := {
            non_missing_values <- na.omit(dplyr::c_across(dplyr::all_of(row.labels.levels)))
            ifelse(length(non_missing_values) >= 1, non_missing_values[i], NA)
          })
    }

    ## create out data frame using last level of row labels
    out <- row.labels %>%
      dplyr::mutate(row.label = .data[[paste0("row_label_level_", n_row_label_levels)]],
                    row.height = NA,
                    spacing_row = FALSE)

    ## iterate over row_label_level_ columns to add headings
    if (n_row_label_levels > 1){
      for (i in (n_row_label_levels - 1):1) {
        out <- out %>%
          dplyr::group_by(!!!syms(paste0("row_label_level_", 1:i))) %>%
          tidyr::nest() %>%
          dplyr::mutate(res = purrr::map(.data$data,
                                         ~ add_row_label_above(.,
                                                               .data[[paste0("row_label_level_", i)]],
                                                               row.labels.space[[i*2-1]],
                                                               row.labels.space[[i*2]]))) %>%
          dplyr::select(-.data$data) %>%
          tidyr::unnest(cols = "res") %>%
          dplyr::ungroup()
      }
    }
  }

  # Add extra rows for addtext
  out <- dplyr::mutate(out, extrarowkey = NA_character_)
  if (!is.null(addtext)) {
    for (k in 1:length(extrarowkeys)) {
      out <- out %>%
        dplyr::add_row(row.label = "",
                       extrarowkey = paste0(extrarowkeys[[k]]),
                       spacing_row = FALSE,
                       .after = which(out$key == extrarowkeys[[k]]))
    }
  }

  out <- out %>%
    dplyr::group_by(.data$extrarowkey) %>%
    dplyr::mutate(addtextrow = 1:dplyr::n() - 1) %>%
    dplyr::ungroup()

  # remove any blank rows at bottom if needed
  while (utils::tail(out$row.label, 1) == "" & is.na(utils::tail(out$extrarowkey, 1))) {
    out <- dplyr::slice(out, 1:(dplyr::n() - 1))
  }

  # handle row.labels that start with "+"
  out$row.label <- gsub("^\\+", "&plus;", out$row.label)

  # create row number and select only needed rows and columns
  out <- out %>%
    dplyr::mutate(row = cumsum(dplyr::coalesce(.data$row.height, 1))) %>%
    dplyr::filter(!.data$spacing_row) %>%
    dplyr::select(.data$row, .data$row.label, .data$key, .data$extrarowkey, .data$addtextrow)

  # make datatoplot
  datatoplot <- tibble::tibble()

  for (i in 1:length(panels)) {
    if (!is.null(col.lci)) {
      panels[[i]] <- panels[[i]] %>%
        dplyr::select(key = !!rlang::sym(col.key),
                      !!!rlang::syms(col.left),
                      estimate = !!rlang::sym(col.estimate),
                      lci      = !!rlang::sym(col.lci),
                      uci      = !!rlang::sym(col.uci),
                      !!!rlang::syms(col.right),
                      !!!rlang::syms(col.keep))
    } else {
      panels[[i]] <- panels[[i]] %>%
        dplyr::select(key = !!rlang::sym(col.key),
                      !!!rlang::syms(col.left),
                      estimate = !!rlang::sym(col.estimate),
                      stderr   = !!rlang::sym(col.stderr),
                      !!!rlang::syms(col.right),
                      !!!rlang::syms(col.keep))
    }

    out1 <- merge(out, panels[[i]], by = "key", all.x = TRUE) %>%
      dplyr::mutate(panel = panel.names[[i]])

    if (!is.null(addtext)){
      out1 <- merge(out1, addtext[[i]],
                    by.x = c("extrarowkey", "addtextrow"),
                    by.y = c("key", "addtextrow"),
                    all.x = TRUE)
    } else {
      out1 <- dplyr::mutate(out1, addtext = as.character(NA))
    }

    datatoplot <- dplyr::bind_rows(datatoplot, out1)
  }


  if (exponentiate == TRUE) {
    tf       <- exp
    inv_tf   <- log
  } else {
    tf       <- identity
    inv_tf   <- identity
  }

  # Make 'panel' a factor, so that facet panels will be in the correct order
  datatoplot <- datatoplot %>%
    dplyr::mutate(panel = factor(panel,
                                 levels = panel.names,
                                 labels = panel.names,
                                 ordered = TRUE))


  # Calculate transformed estimates, confidence intervals, and point size
  if (!is.null(col.lci)) {
    datatoplot <- datatoplot %>%
      dplyr::mutate(estimate_transformed = tf(.data$estimate),
                    lci_transformed = tf(.data$lci),
                    uci_transformed = tf(.data$uci)
      )
    if (is.null(minse)){
      minse <- min((datatoplot$uci - datatoplot$lci)/(2*1.96), na.rm = TRUE)
    } else {
      if (minse > min((datatoplot$uci - datatoplot$lci)/(2*1.96), na.rm = TRUE)) rlang::abort("minse is larger than the minimum standard error in the data")
    }
    if (scalepoints){
      datatoplot$size <- 2*1.96*minse/(datatoplot$uci - datatoplot$lci)
    }
  } else {
    datatoplot <- datatoplot %>%
      dplyr::mutate(estimate_transformed = tf(.data$estimate),
                    lci_transformed = tf(.data$estimate - 1.96*.data$stderr),
                    uci_transformed = tf(.data$estimate + 1.96*.data$stderr)
      )
    if (is.null(minse)){
      minse <- min(datatoplot$stderr, na.rm = TRUE)
    } else {
      if (minse > min(datatoplot$stderr, na.rm = TRUE)) rlang::abort("minse is larger than the minimum standard error in the data")
    }
    if (scalepoints) {
      datatoplot$size <- minse/datatoplot$stderr
    }
  }

  # Create auto_estcolumn column (text of estimates and CIs)
  datatoplot <- datatoplot %>%
    dplyr::mutate(auto_estcolumn = dplyr::if_else(
      !is.na(.data$estimate),
      make_auto_estcolumn_text(.data$estimate_transformed,
                               .data$lci_transformed,
                               .data$uci_transformed,
                               digits,
                               ci.delim),
      NA_character_)) %>%
    dplyr::select(-.data$extrarowkey, -.data$addtextrow) %>%
    dplyr::arrange(panel, row)

  # Create diamonds_polygon column
  datatoplot$as_diamond <- FALSE
  if (!is.null(diamond) | !is.null(col.diamond)){
    datatoplot <- datatoplot %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        diamond_polygon = list(data.frame(x = c(.data$lci_transformed,
                                                .data$estimate_transformed,
                                                .data$uci_transformed,
                                                .data$estimate_transformed),
                                          y = c(0, -0.25, 0, 0.25)))) %>%
      dplyr::ungroup()

    if (!is.null(col.diamond)){
      datatoplot <- datatoplot %>%
        dplyr::mutate(diamond_polygon = dplyr::if_else(.data$key %in% diamond | .data[[col.diamond]], .data$diamond_polygon, NA))
    } else {
      datatoplot <- datatoplot %>%
        dplyr::mutate(diamond_polygon = dplyr::if_else(.data$key %in% diamond, .data$diamond_polygon, NA))
    }
    datatoplot$as_diamond <- !sapply(datatoplot$diamond_polygon, is.null)
  }


  # Create rowlabels data frame and add as attribute to datatoplot data frame
  ## Add bold formatting to row labels (if there is no estimate, or key is in bold.labels)
  rowlabels <- datatoplot %>%
    dplyr::group_by(.data$row) %>%
    dplyr::summarise(row.label = dplyr::first(.data$row.label),
                     bold = all(is.na(.data$estimate_transformed) | all(.data$key %in% bold.labels)),
                     .groups = "drop") %>%
    dplyr::mutate(row.label = dplyr::if_else(.data$bold & .data$row.label != "",
                                             paste0("**", .data$row.label, "**"),
                                             as.character(.data$row.label))) %>%
    dplyr::arrange(.data$row) %>%
    dplyr::select(.data$row, .data$row.label)

  attr(datatoplot, "rowlabels") <- rowlabels



  return(datatoplot)
}




#' Create heterogeneity string
#'
#' @keywords internal
#' @noRd
make_heterogeneity_string <- function(het_dof, het_stat, het_p) {
  paste0("paste('Heterogeneity: ', chi[",
         het_dof,
         "]^2,'=",
         het_stat,
         " (p",
         het_p,
         ")', sep='')")
}




#' Create trend string
#'
#' @keywords internal
#' @noRd
make_trend_string <- function(trend_stat, trend_p) {
  paste0("paste('Trend: ', chi[1]^2,'=",
         trend_stat,
         " (p",
         trend_p,
         ")', sep='')")
}




#' Add 'heading' above row label data
#'
#' @keywords internal
#' @noRd
add_row_label_above <- function(data,
                                heading,
                                blank_after_heading,
                                blank_after_section){
  if (!grepl("@nolabel$", heading)) {
    out <- tibble::add_row(data,
                           row.label = !!heading,
                           spacing_row = FALSE,
                           .before = 1) %>%
      tibble::add_row(row.label = "",
                      row.height = blank_after_heading,
                      spacing_row = TRUE,
                      .before = 2)
    if(all(is.na(data$row.label))){
      out <- dplyr::mutate(data, row.label = !!heading)
    }
  } else {
    out <- data
  }
  out <- tibble::add_row(out,
                         row.label = "",
                         row.height = blank_after_section,
                         spacing_row = TRUE)
  out
}


#' Create text for the auto-generated estimate column
#'
#' @keywords internal
#' @noRd
make_auto_estcolumn_text <- function(estimate_transformed,
                                     lci_transformed,
                                     uci_transformed,
                                     digits,
                                     ci.delim) {
  est <- format(round(estimate_transformed, digits), nsmall = digits, trim = T)
  lci <- format(round(lci_transformed, digits),      nsmall = digits, trim = T)
  uci <- format(round(uci_transformed, digits),      nsmall = digits, trim = T)
  text <- glue::glue("{est} ({lci}{ci.delim}{uci})")
  text[lci == "NA" | uci == "NA"] <- est[lci == "NA" | uci == "NA"]
  return(text)
}

