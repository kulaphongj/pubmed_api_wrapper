library(testthat)
library(httr)
library(jsonlite)
library(dplyr)
library(RColorBrewer)
library(wordcloud2)
library(htmlwidgets)


context("get_all_businesses Functionality")

test_that("get_all_businesses returns a dataframe on success", {
  api_key <- 'TpL2Xxv5ezpTSAxUUpuUIMLXM1fv0nFu9g3VyTUD_FhbkHG9OD322THyJSW_b30QMqRbvIdfqmLIDqTFNf7hxl-aXOcXzgii6H7_Wirdj5BOVniojDjRFpPEiyewZXYx'
  location <- "San Francisco"
  business_type <- "food"
  keyword <- "pizza"
  total <- 50 # Using a smaller total for testing to avoid hitting rate limits

  result <- get_all_businesses(api_key, location, business_type, keyword, total)

  # Check if the result is not NULL
  expect_false(is.null(result))

  # Check if the result is a dataframe
  expect_true(is.data.frame(result))

  # Check if the dataframe contains the expected columns
  expected_columns <- c("name", "rating", "review_count")
  expect_true(all(expected_columns %in% names(result)))

  # Check if the dataframe is not empty
  expect_gt(nrow(result), 0)
})

test_that("get_all_businesses handles small total correctly", {
  api_key <- 'your_real_api_key_here' # Use your real API key
  location <- "San Francisco"
  business_type <- "food"
  keyword <- "pizza"
  total <- 5 # Intentionally small to test function's handling of small totals

  result <- get_all_businesses(api_key, location, business_type, keyword, total)

  # Check if the number of returned businesses does not exceed the requested total
  expect_true(nrow(result) <= total)
})

# Add more tests as needed, for example, to check handling of invalid parameters, handling of no results, etc.
