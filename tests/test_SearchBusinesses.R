# Load the necessary libraries and the function to test
library(testthat)
source("../SearchBusinesses.R")  # Replace with the actual path

# Define a test context
test_that("SearchBusinesses function works correctly", {

  # Test case 1: Ensure a valid API key is used
  expect_error(SearchBusinesses("invalid_key", "New York", "restaurant", "pizza"),
               "API key is invalid.")

  # Test case 2: Ensure the function returns a list of businesses
  result <- SearchBusinesses("valid_key", "Los Angeles", "restaurant", "sushi")
  expect_true(is.list(result), "The result should be a list.")

  # Test case 3: Check if the returned list contains business data
  expect_true("businesses" %in% names(result), "The result should contain 'businesses'.")


})
