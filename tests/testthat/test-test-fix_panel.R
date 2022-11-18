test_that("error if both width and height specified", {
  p <- ggplot2::ggplot()
  expect_error(fix_panel(p, width = unit(5, "cm"), height = unit(5, "cm")))
})
