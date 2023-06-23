test_that("forest_data throws an error if column doesn't exist in every panel's data frame", {
  panels <- list(data.frame(a = 1:3), data.frame(a = 4:6))
  expect_error(check_forest_plot_arguments(col.left = "b",
                                           col.right = "a",
                                           panels = panels),
               "Column 'b' does not exist in every panels data frame.")
})

test_that("forest_data throws an error if cicolour is a list or longer than 1 but not using panel.width", {
  panels <- list(data.frame(a = 1:3), data.frame(a = 4:6))
  expect_error(check_forest_plot_arguments(col.left = "a",
                                           col.right = NULL,
                                           panels = panels,
                                           cicolour = c("black", "white"),
                                           fixed_panel_width = FALSE))
})

