#' Prepares data set for a forest plot
#'
#'
#' @param panels A list of data frames. These should include columns or point
#'   estimates, and standard errors or confidence interval limits. If you
#'   specify a row.labels data frame, then they must also all contain a key column
#'   with the same name (which can be specified by col.key).
#' @param col.key Name of column that links the results given in each data frame
#'   provided in panels and the labels given in row.labels.
#'
#'   If row.labels data frame is not given, then this column will be used as row labels.
#'
#'   (Default: "key")
#' @param row.labels A data frame that contains the labels to be used for the
#'   rows of the plot. Use NA if a lower level heading is not required for a given row.
#' @param row.labels.levels A character vector. The names of columns in row.labels
#'   to use as headings/subheadings/labels for labelling rows.
#' @param panel.names A character vector. The names to be used for each forest plot panel.
#'   If none provided, then they will be numbered 1, 2, 3 ...
#' @param col.estimate Name of column that provides point estimates.
#'   (Default: "estimate")
#' @param col.stderr Name of column that provides standard errors. (Default: "stderr")
#' @param col.lci Name of column that provides lower limit of confidence intervals.
#' @param col.uci Name of column that provides upper limit of confidence intervals.
#' @param col.left Names of columns to be printed to the left of the plot.
#' @param col.right Names of columns to be printed to the right of the plot.
#' @param col.keep Names of additional columns to be kept in returned data frame.
#' @param ci.delim Character string to separate lower and upper limits of
#'   confidence interval. (Default: ", ")
#' @param digits Number of digits after decimal point to show for estimates and confidence intervals. (Default: 2)
#' @param exponentiate Exponentiate estimates (and CIs) before plotting. (Default: TRUE)
#' @param blankrows A numeric vector specifying the number of blank rows
#'   after a row label heading, at the end of a row label heading 'section'. (Default: c(1, 1, 0, 0))
#' @param scalepoints Should the points be scaled by inverse of the standard
#'   error? (Default: FALSE)
#' @param minse Minimum standard error to use when scaling point size. (Default will use minimum in the data.)
#' @param addtext A list of data frames. List must be the same length as panels.
#'   Data frames should contain a column with the name specified in col.key,
#'   and one or more of:
#'
#'   1. a column named 'text' containing character strings
#'
#'   2. columns named 'het_dof', 'het_stat', and 'het_p' containing character strings
#'
#'   3. columns names 'trend_stat' and 'trend_p' containing character strings
#'
#'   The character strings, heterogeneity test, and trend test results will
#'   be plotted in the column of estimates and CIs, below the row with the key
#'   given in the col.key column.
#' @param cols DEPRECATED.
#' @param headings DEPRECATED.
#' @param colnames DEPRECATED.
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
    col.key       = "key",
    row.labels    = NULL,
    row.labels.levels = NULL,
    panel.names   = NULL,
    col.estimate  = "estimate",
    col.stderr    = "stderr",
    col.lci       = NULL,
    col.uci       = NULL,
    col.left      = NULL,
    col.right     = NULL,
    col.keep      = NULL,
    ci.delim      = ", ",
    digits        = 2,
    exponentiate  = TRUE,
    blankrows     = c(1, 1, 0, 0),
    scalepoints   = FALSE,
    minse         = NULL,
    addtext       = NULL,
    cols          = panels,
    headings      = NULL,
    colnames      = NULL,
    bold.labels   = NULL
){

  # legacy arguments
  if (!missing(cols)) {
    panels <- cols
    message("Note: cols argument is now called panels")
  }
  if (!missing(headings)) {
    row.labels <- headings
    message("Note: headings argument is now called row.labels")
  }
  if (!missing(colnames)) {
    panel.names <- colnames
    message("Note: colnames argument is now called panel.names")
  }

  # check function arguments
  if (is.data.frame(panels)) {
    panels <- list(panels)
  }

  if (is.null(panel.names)) { panel.names <- as.character(1:length(panels)) }

  if (any(unlist(lapply(panels, function(x) !col.key %in% names(x))))) {
    rlang::warn(glue::glue("col.key '{col.key}' not found, using row number."))
    col.key <- "key"
    for (i in seq_along(panels)){
      panels[[i]][["key"]] <- seq_len(nrow(panels[[i]]))
    }
  }

  if (is.null(row.labels.levels)) {
    row.labels.levels <- names(row.labels)
    row.labels.levels <- row.labels.levels[!row.labels.levels == col.key]
  }

  check_forest_data_arguments(panels,
                              panel.names,
                              row.labels,
                              row.labels.levels,
                              blankrows,
                              col.lci,
                              col.uci)

  # Make vector of keys after which extra rows are added for addtext
  addtextcols <- tibble::tibble(text = character(),
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
    out <- panels[[1]] %>%
      dplyr::mutate(row.label = !!rlang::sym(col.key),
                    key = !!rlang::sym(col.key),
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
                                                               blankrows[[i*2-1]],
                                                               blankrows[[i*2]]))) %>%
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
    datatoplot$size <- 2*1.96*minse/(datatoplot$uci - datatoplot$lci)
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
    datatoplot$size <- minse/datatoplot$stderr
  }

  if (!scalepoints) {
    datatoplot$size <- 1
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


#' @describeIn forest_data Synonym for `forest_data()`
#' @export
make_forest_data <- forest_data




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
  glue::glue("{est} ({lci}{ci.delim}{uci})")
}


check_forest_data_arguments <- function(panels,
                                        panel.names,
                                        row.labels,
                                        row.labels.levels,
                                        blankrows,
                                        col.lci,
                                        col.uci,
                                        call = rlang::caller_env()) {
  if (!is.null(col.lci) && is.null(col.uci)) {
    rlang::abort("col.lci and col.uci must both be specified",
                 call = call)
  }
  if (is.null(col.lci) && !is.null(col.uci)){
    rlang::abort("col.lci and col.uci must both be specified",
                 call = call)
  }
  if (!is.character(panel.names)) {
    rlang::abort("panel.names must be a character vector",
                 call = call)
  }
  if (!all(!duplicated(panel.names))) {
    rlang::abort("panel.names must be unique",
                 call = call)
  }
  if (length(panels) != length(panel.names)) {
    rlang::abort("panels and panel.names must be the same length",
                 call = call)
  }
  if (!(length(blankrows) >= 2*(length(row.labels.levels) - 1))) {
    rlang::abort("blankrows must be at least 2*(length(row.labels.levels)-1)",
                 call = call)
  }
  if (!is.null(row.labels) && !all(row.labels.levels %in% names(row.labels))) {
    rlang::abort("row.labels.levels must be columns in row.labels",
                 call = call)
  }
  if(!is.null(row.labels) && !all(sapply(row.labels[row.labels.levels], is.character))) {
    rlang::abort("row.labels.levels columns must be character",
                 call = call)
  }
}
