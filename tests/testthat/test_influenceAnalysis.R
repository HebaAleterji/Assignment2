library(testthat)
library(influenceAnalysis)

# Test for cooks_distance_scratch function
test_that("cooks_distance_scratch works correctly", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  result <- cooks_distance_scratch(model)
  expect_true(is.numeric(result))
  expect_length(result, nrow(mtcars))
})

# Test for dffits_scratch function
test_that("dffits_scratch works correctly", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  result <- dffits_scratch(model)
  expect_true(is.numeric(result))
  expect_length(result, nrow(mtcars))
})

# Test for hadis_influence_measure function
test_that("hadis_influence_measure works correctly", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  result <- hadis_influence_measure(model)
  expect_true(is.numeric(result))
  expect_length(result, nrow(mtcars))
})

# Test for influence_diagnostics function
test_that("influence_diagnostics works correctly", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  result <- influence_diagnostics(mtcars, model, "all")
  expect_true(is.list(result))
  expect_true("cooks" %in% names(result))
  expect_true("dffits" %in% names(result))
  expect_true("hadi" %in% names(result))
})

# Test for plot_influence function
test_that("plot_influence works without errors", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  diagnostics <- influence_diagnostics(mtcars, model, "all")
  expect_silent(plot_influence(diagnostics))
})

# Test for validate_inputs function
test_that("validate_inputs works correctly", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  data_subset <- subset_data(mtcars, model)
  expect_silent(validate_inputs(data_subset, model))
  expect_error(validate_inputs(data.frame(), model), "The number of columns in data does not match the number of predictors in the model")
})

# Test for subset_data function
test_that("subset_data works correctly", {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  data_subset <- subset_data(mtcars, model)
  expect_true(is.data.frame(data_subset))
  expect_equal(ncol(data_subset), length(coef(model)))
})
