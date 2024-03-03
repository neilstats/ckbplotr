#' prepend character string with space
#'
#' @keywords internal
#' @noRd
#'

indent <- function(n, ...){
  if (rlang::is_empty(c(...))){return (NULL)}
  paste0(paste0(rep(" ", n),
                collapse = ""),
         c(...))
}


#' put backticks around character string if contains a blank
#'
#' @keywords internal
#' @noRd
#'

column_name <- function(x){
  if(is.null(x)){ return(NULL) }
  for (i in 1:length(x)){
    if (grepl("[[:blank:]]|[[:punct:]]", x[[i]])){
      x[[i]] <- paste0("`", x[[i]], "`")
    }
  }
  x
}


#' put escaped quotes around character string
#'
#' @keywords internal
#' @noRd
#'
quote_string <- function(x){
  if(is.null(x)){return(x)}
  if(is.list(x)){return(lapply(x, quote_string))}
  paste0('\"', x, '\"')
}

#. Write code for preparing data using make_forest_data
#'
#' @keywords internal
#' @noRd
#'
argset <- function(x){
  name <- paste(deparse(substitute(x)), collapse = '')
  value <- paste(deparse(x), collapse = '')
  if (!identical(x, eval(formals(ckbplotr::forest_data)[[name]]))){
    glue::glue('{name} = {value}')
  }
}


#' write ggplot layer code
#'
#' @keywords internal
#' @noRd
#'
make_layer <- function(name       = NULL,
                       f,
                       aes        = NULL,
                       arg        = NULL,
                       plus       = TRUE,
                       br         = TRUE,
                       duplicates = FALSE,
                       glue_environment = parent.frame()){

  if (!is.null(aes)){
    aes <- na.omit(purrr::map_chr(aes, \(x){y <- glue::glue(x, .envir = glue_environment); ifelse(length(y) == 1, y, NA)}))
  }
  if (!is.null(arg)){
    arg <- na.omit(purrr::map_chr(arg, \(x){y <- glue::glue(x, .envir = glue_environment); ifelse(length(y) == 1, y, NA)}))
  }

  if (!duplicates){
    aes <- aes[!duplicated(trimws(sub("=.*", "", aes)))]
    arg <- arg[!duplicated(trimws(sub("=.*", "", c(aes, arg))))[(length(aes) + 1):(length(aes) + length(arg))]]
  }

  if (length(aes) > 0){
    aes <- indent(4, paste0(aes, ","))
    aes[[1]] <- paste0("aes(", trimws(aes[[1]]))
    aes[[length(aes)]] <- sub(",$", "),", aes[[length(aes)]])
  } else {
    aes <- NULL
  }
  if (length(arg) > 0){
    arg <- paste0(arg, ",")
  } else {
    arg <- NULL
  }
  args <- indent(nchar(f)+1, c(aes, arg))
  args[[1]] <- paste0(f, "(", trimws(args[[1]]))
  args[[length(args)]] <- paste0(sub(",$", "", args[[length(args)]]), ")", if(plus){" +"})
  c(name, args, if(br){''})
}


#' Write the ggplot2 code to a file in temp directory, and show in RStudio viewer
#'
#' @keywords internal
#'@noRd
#'
displaycode <- function(plotcode, note = ""){

  if (!is.null(knitr::opts_knit$get("out.format"))){
    return(NULL)
  }

  text <- c("---",
            "title: 'Generated R code'",
            "output:",
            "  html_document:",
            "    highlight: kate",
            "---",
            "```{css, echo=FALSE}",
            ".no-border {border: 0px;}",
            "```",
            note,
            "```{r plotcode, class.source='no-border', eval = FALSE}",
            plotcode,
            "```")

  temprmd <- tempfile(fileext = ".Rmd")
  con <- file(temprmd, open = "w", encoding = "UTF-8")
  writeLines(
    text,
    con = con,
    sep = "\n")
  close(con)

  rmarkdown::render(temprmd,
                    output_file = "plotcode.html",
                    quiet = TRUE)

  viewer <- getOption("viewer", default = function(url){})
  viewer(file.path(tempdir(), "plotcode.html"))
}


#' Use deparse, escape and unescape unicode, and collapse to a single string
#'
#' @keywords internal
#'@noRd
#'
ds <- function(x){
  paste(
    stringi::stri_unescape_unicode(
      deparse(
        stringi::stri_escape_unicode(x)
      )
    ),
    collapse = '')
}


#' Turn unit object into a string
#'
#' @keywords internal
#' @noRd
printunit <- function(x){
  if(is.null(x)){return(NULL)}
  glue::glue('unit({deparse(as.numeric(x))}, {makeunit(x)})')
}


#' Turn unit object into name of unit
#'
#' @keywords internal
#' @noRd
makeunit <- function(x){

  if(is.null(x)){return(NULL)}
  ## handle different unit object types (for grid>=4.0)
  if (compareVersion(as.character(packageVersion("grid")), "4.0") >= 0){
    ds(grid::unitType(x))
  } else {
    ds(attr(x, "unit"))
  }

}




#' Turn font size in pt into mm and multiply
#'
#' multiply 0.8 to match default size of axis text in ggplot
#'
#' @keywords internal
#' @noRd

base_size_to_text_size <- function(x, m = 0.8){
  m * x/.pt
}
