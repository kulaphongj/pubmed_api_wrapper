library(testthat)
source("search_functions.R")

# Test search_businesses function
test_that("search_businesses returns expected structure", {
  # Assuming the API key and other parameters are correct and working
  result <- search_businesses(api_key, "San Francisco", "restaurants", "sushi", 0, 1)

  # Check if the result is not NULL
  expect_false(is.null(result))

  # Check if the result is a list (as each business should be a list within the main list)
  expect_true(is.list(result))

  # This part is highly dependent on the API's response structure
  if(length(result) > 0) {
    expect_true("id" %in% names(result[[1]]))
  }
})

# Test get_all_businesses function
test_that("get_all_businesses returns expected number of results", {
  # This test can take a while due to API rate limits and the nature of the function
  # Consider mocking the API response or using a smaller total for testing
  result <- get_all_businesses(api_key, "San Francisco", "restaurants", "sushi", 100)

  # Check if the result is a dataframe
  expect_true(is.data.frame(result))

  # Check the result is not empty (assuming the API and parameters are correct)
  expect_false(nrow(result) == 0)

  # Check the result has expected columns
  expected_columns <- c("id", "name", "rating", "review_count", "location")
  expect_true(all(expected_columns %in% names(result)))
})
