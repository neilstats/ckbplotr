test_that("shape.theme", {
  expect_equal(shape.theme("top"),
               c("# Add theme",
                 "theme(legend.position = \"top\")",
                 ""))
})
