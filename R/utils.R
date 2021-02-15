#' prepend character string with space
#'
#' @keywords internal
#'

indent <- function(n, ...){
  paste0(paste0(rep(" ", n),
                collapse = ""),
         c(...))
}
