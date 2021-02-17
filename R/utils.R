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
  if (grepl("[[:blank:]]", x)){
    paste0("`", x, "`")
  } else {
      x
    }
}


#' put escaped quotes around character string
#'
#' @keywords internal
#' @noRd
#'

fixq <- function(x){
  paste0('\"', x, '\"')
}


#' write ggplot layer code
#'
#' @keywords internal
#' @noRd
#'
make_layer <- function(name = "", f, aes = NULL, arg = NULL){
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
  args[[length(args)]] <- paste0(sub(",$", "", args[[length(args)]]), ") +")
  c(name, args, '')
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
  viewer <- getOption("viewer", default = function(url){})
  viewer(file.path(tempdir(), "plotcode.txt"))
}
