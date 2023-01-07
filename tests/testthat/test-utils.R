test_that("indent", {
  expect_null(indent(1))
  expect_equal(indent(2, "test text"),
               "  test text")
})

test_that("fixsp", {
  expect_equal(fixsp("x y"), "`x y`")
  expect_equal(fixsp("x-y"), "`x-y`")
  expect_equal(fixsp("xy"), "xy")
})

test_that("fixq", {
  expect_equal(fixq("x"), "\"x\"")
})

test_that("argset", {
  expect_equal(argset("x"), "\"x\" = \"x\"")
})

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

test_that("printunit", {
  expect_equal(printunit(unit(1, "cm")),
               "unit(1, \"cm\")")
})

test_that("makeunit", {
  expect_equal(makeunit(unit(1, "cm")),
               "cm")
  expect_equal(makeunit(unit(1, "in")),
               "inches")
  expect_equal(makeunit(unit(1, "null")),
               "null")
})

