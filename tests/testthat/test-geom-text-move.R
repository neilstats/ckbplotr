test_that("geom_text_move moves text correctly", {
  df <- data.frame(x = 1:5, y = 1:5, label = letters[1:5])
  p1 <- ggplot(df, aes(x, y, label = label)) +
    geom_text_move(move_x = unit(5, "mm"),
                   move_y = unit(10, "mm")) +
    geom_text()

  grob_text_move <- layer_grob(p1)[[1]]
  grob_text <- layer_grob(p1, i = 2)[[1]]

  # check that the text in geom_text_move is moved by 5pt horizontally and vertically
  expect_equal(grid::convertWidth(grob_text_move$x - grob_text$x, "mm"),
               unit(rep(5, 5), "mm"))
  expect_equal(grid::convertHeight(grob_text_move$y - grob_text$y, "mm"),
               unit(rep(10, 5), "mm"))
})
