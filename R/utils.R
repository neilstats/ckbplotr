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

fixsp <- function(x){
  for (i in 1:length(x)){
    if (grepl("[[:blank:]]", x[[i]])){
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

fixq <- function(x){
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
  if (!identical(x, eval(formals(ckbplotr::make_forest_data)[[name]]))){
    sprintf('%s = %s', name, value)
  }
}


#' write ggplot layer code
#'
#' @keywords internal
#' @noRd
#'
make_layer <- function(name = NULL, f, aes = NULL, arg = NULL, plus = TRUE, br = TRUE, duplicates = FALSE){

  if (!duplicates){
    aes <- aes[!duplicated(trimws(sub("=.*", "", aes)))]
    arg <- arg[!duplicated(trimws(sub("=.*", "", c(aes, arg))))[(length(aes) + 1):(length(aes) + length(arg))]]
  }


  if (!is.null(aes)){
    aes <- indent(4, paste0(aes, ","))
    aes[[1]] <- paste0("aes(", trimws(aes[[1]]))
    aes[[length(aes)]] <- sub(",$", "),", aes[[length(aes)]])
  }
  if (!is.null(arg)){
    arg <- paste0(arg, ",")
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
displaycode <- function(plotcode){
  writeLines(paste(c('# Generated plot code ------------------',
                     '',
                     plotcode),
                   collapse = "\n"),
             file.path(tempdir(), "plotcode.txt"))
  codefile <- file.path(tempdir(), "plotcode.txt")

  if (requireNamespace("highlight", quietly = TRUE)) {
    highlight::highlight(file.path(tempdir(), "plotcode.txt"),
                         output = file.path(tempdir(), "highlight-plotcode.html"),
                         renderer = highlight::renderer_html(document = TRUE))
    codefile <- file.path(tempdir(), "highlight-plotcode.html")
  }

  viewer <- getOption("viewer", default = function(url){})
  viewer(codefile)
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
