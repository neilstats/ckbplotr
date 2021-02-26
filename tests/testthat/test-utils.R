test_that("ggplot layers are built correctly", {
  expect_equal(make_layer(name = "# add a layer",
                          f = "some_geom",
                          aes = c("x = 1", "y = 1"),
                          arg = c("size = 3", "colour = 'red'")),
               c("# add a layer",
                 "some_geom(aes(x = 1,",
                 "              y = 1),",
                 "          size = 3,",
                 "          colour = 'red') +",
                 ""))
})

test_that("ds handles unicode correctly", {
  expect_equal(ds("a \u2265 b"),
               "\"a \\u2265 b\"")
})
