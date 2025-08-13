.onAttach <- function(...) {
  packageStartupMessage(
    cli::format_inline(
      "ckbplotr 0.12.0 introduces many changes.\n",
      "See {.href [https://neilstats.github.io/ckbplotr](https://neilstats.github.io/ckbplotr)} for details on using the package."
    )
  )
}
