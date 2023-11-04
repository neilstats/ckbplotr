.onAttach <- function(...) {
  packageStartupMessage(
    cli::format_inline(
      "See {.href [https://neilstats.github.io/ckbplotr](https://neilstats.github.io/ckbplotr)} for details on using this package."
    )
  )
}
