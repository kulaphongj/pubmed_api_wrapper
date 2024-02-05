library(testthat)
library(htmlwidgets) # Required for testing if the output is a saved HTML widget

# Assuming 'search_functions.R' defines the 'create_word_cloud' function
source("../R/create_word_cloud.R")

context("Testing create_word_cloud function")

# Test with a minimal, valid dataset
test_that("create_word_cloud generates output with valid data", {
  data <- data.frame(
    name = c("Business A", "Business B"),
    rating = c(5, 4),
    review_count = c(100, 50)
  )

  # Test without saving to file
  expect_silent(create_word_cloud(data, FALSE))

  # Test with saving to file, expect an HTML file to be created
  temp_file <- tempfile(fileext = ".html")
  expect_silent(create_word_cloud(data, TRUE, temp_file))
  expect_true(file.exists(temp_file))
  if (file.exists(temp_file)) file.remove(temp_file) # Cleanup
})

# Test handling of NA values in data
test_that("create_word_cloud stops with NA values in data", {
  data_with_na <- data.frame(
    name = c("Business A", NA),
    rating = c(5, NA),
    review_count = c(100, NA)
  )

  expect_error(create_word_cloud(data_with_na, FALSE))
})

# Test with empty data
test_that("create_word_cloud handles empty data gracefully", {
  empty_data <- data.frame(name = character(),
                           rating = numeric(),
                           review_count = numeric())
  expect_warning(create_word_cloud(empty_data, FALSE))
})

# Test with extreme data values
test_that("create_word_cloud handles extreme data values correctly", {
  extreme_data <- data.frame(
    name = c("Business A", "Business B"),
    rating = c(5, 1),
    review_count = c(1000, 1)
  )

  temp_file <- tempfile(fileext = ".html")
  expect_silent(create_word_cloud(extreme_data, TRUE, temp_file))
  expect_true(file.exists(temp_file))
  if (file.exists(temp_file)) file.remove(temp_file) # Cleanup
})
