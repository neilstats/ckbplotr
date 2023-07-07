.onAttach <- function(...) {
  packageStartupMessage(
    cli::format_inline(
      "This version of the package introduces many changes.\nPlease see {.href [https://neilstats.github.io/ckbplotr/news](https://neilstats.github.io/ckbplotr/news)} for details."
    )
  )
}
