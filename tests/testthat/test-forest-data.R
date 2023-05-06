test_that("forest_data throws an error if col.lci is specified but col.uci is not", {
  expect_error(forest_data(panels = NULL,
                           panel.names = NULL,
                           row.labels = NULL,
                           row.labels.levels = NULL,
                           blankrows = NULL,
                           col.lci = "a",
                           col.uci = NULL),
               "col.lci and col.uci must both be specified")
})

test_that("forest_data throws an error if col.uci is specified but col.lci is not", {
  expect_error(forest_data(panels = NULL,
                           panel.names = NULL,
                           row.labels = NULL,
                           row.labels.levels = NULL,
                           blankrows = NULL,
                           col.lci = NULL,
                           col.uci = "a"),
               "col.lci and col.uci must both be specified")
})

test_that("forest_data throws an error if panel.names is not a character vector", {
  expect_error(forest_data(panels = NULL,
                           panel.names = 1,
                           row.labels = NULL,
                           row.labels.levels = NULL,
                           blankrows = NULL,
                           col.lci = NULL,
                           col.uci = NULL),
               "panel.names must be a character vector")
})

test_that("forest_data throws an error if panel.names is not unique", {
  expect_error(forest_data(panels = NULL,
                           panel.names = c("a", "b", "a"),
                           row.labels = NULL,
                           row.labels.levels = NULL,
                           blankrows = NULL,
                           col.lci = NULL,
                           col.uci = NULL),
               "panel.names must be unique")
})

test_that("forest_data throws an error if panels and panel.names have different lengths", {
  expect_error(forest_data(panels = list(1,2,3),
                           panel.names = c("a", "b"),
                           row.labels = NULL,
                           row.labels.levels = NULL,
                           blankrows = NULL,
                           col.lci = NULL,
                           col.uci = NULL),
               "panels and panel.names must be the same length")
})

test_that("forest_data throws an error if blankrows is less than 2*(length(row.labels.levels)-1)", {
  expect_error(forest_data(panels = list(1),
                           panel.names = "panel",
                           row.labels = NULL,
                           row.labels.levels = c("heading", "subheading"),
                           blankrows = 1,
                           col.lci = NULL,
                           col.uci = NULL))
})

# Test if function throws an error if row.labels.levels is not a column in row.labels
test_that("forest_data throws an error if row.labels.levels is not a column in row.labels", {
  expect_error(forest_data(panels = list(data.frame(x = 1)),
                           panel.names = "panel",
                           row.labels = data.frame(x = 1:10),
                           row.labels.levels = c("y"),
                           blankrows = NULL,
                           col.lci = NULL,
                           col.uci = NULL),
               "row.labels.levels must be columns in row.labels")
})

test_that("forest_data throws an error if row.labels.levels columns are not character", {
  expect_error(forest_data(panels = list(data.frame(x = 1)),
                           panel.names = "panel",
                           row.labels = data.frame(y = 1:5),
                           row.labels.levels = c("y"),
                           blankrows = NULL,
                           col.lci = NULL,
                           col.uci = NULL),
               "row.labels.levels columns must be character")
})


test_that("make_heterogeneity_string returns the expected string", {
  het_dof <- "5"
  het_stat <- "15.68"
  het_p <- "=0.0001"
  expected_output <- "paste('Heterogeneity: ', chi[5]^2,'=15.68 (p=0.0001)', sep='')"
  expect_equal(
    make_heterogeneity_string(het_dof, het_stat, het_p),
    expected_output
  )
})






test_that("make_trend_string returns the expected string", {
  trend_stat <- "10.12"
  trend_p <- "=0.012"
  expected_output <- "paste('Trend: ', chi[1]^2,'=10.12 (p=0.012)', sep='')"
  expect_equal(
    make_trend_string(trend_stat, trend_p),
    expected_output
  )
})





test_that("add_row_label_above returns the expected data frame", {
  data <- tibble::tribble(
    ~row.label, ~row.height, ~spacing_row,
    "A",                 NA,        FALSE,
    "B",                 NA,        FALSE
  )
  heading <- "Heading"
  blank_after_heading <- 0.5
  blank_after_section <- 0.5
  expected_output <- tibble::tribble(
    ~row.label, ~row.height, ~spacing_row,
    "Heading",           NA,        FALSE,
    "",                 0.5,         TRUE,
    "A",                 NA,        FALSE,
    "B",                 NA,        FALSE,
    "",                 0.5,         TRUE
  )

  expect_equal(
    add_row_label_above(data, heading, blank_after_heading, blank_after_section),
    expected_output
  )
})






test_that("make_auto_estcolumn_text returns the expected text", {
  est <- 1.234567
  lci <- 0.987654
  uci <- 1.543210
  digits <- 2
  ci.delim <- "-"
  expected_output <- "1.23 (0.99-1.54)"

  expect_equal(
    make_auto_estcolumn_text(est, lci, uci, digits, ci.delim),
    expected_output
  )
})
