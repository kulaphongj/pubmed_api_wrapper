library(testthat)
library(stringr)
# install.packages("here")
# library(here)



# Source the file containing the function to be tested
source("AnalyzeRatings.R") # source("C:\\PATH\\TO\\FILE\\AnalyzeRatings.R")



# Test for API key handling
test_that("API key is retrieved from environment variable", {
  expect_true(nchar(api_key) > 0)
})



# Test for correct output type
test_that("Output is a list with combined dataframe and parameters", {
  cities <- c('Kelowna', 'Penticton', 'Red Deer')
  result <- AnalyzeRatings(cities)
  expect_is(result, "list")
  expect_named(result, c("combined_df", "parameters"))
})



# Test for correct DataFrame columns
test_that("Combined dataframe has expected columns", {
  cities <- c('Kelowna', 'Penticton', 'Red Deer')
  result <- AnalyzeRatings(cities)
  expected_columns <- c("City", "name", "review_count", "rating", "price", "price_factor")
  expect_true(all(sapply(expected_columns, function(col) any(tolower(col) == tolower(names(result$combined_df))))))
})



# Test for successful API request
test_that("Function retrieves data for each city", {
  cities <- c('Kelowna', 'Penticton', 'Red Deer')
  result <- AnalyzeRatings(cities)
  expect_gt(nrow(result$combined_df), 0)
})



# Test for API request failure handling
test_that("Function handles API request failures", {
  cities <- c('Invalid_City')
  expect_error(AnalyzeRatings(cities), "Failed to retrieve data.")
})



# Test for correct parameter passing
test_that("Function passes correct parameters to API request", {
  cities <- c('Kelowna', 'Penticton', 'Red Deer')
  category <- "food"
  limit <- 20
  result <- AnalyzeRatings(cities, category, limit)
  expect_equal(result$parameters$cities, cities)
  expect_equal(result$parameters$category, category)
  expect_equal(as.character(result$parameters$limit), as.character(limit))  # Convert to character for comparison
})



# Test for price factorization
test_that("Price factorization is performed correctly", {
  cities <- c('Kelowna', 'Penticton', 'Red Deer')
  result <- AnalyzeRatings(cities)
  expect_true(all(levels(result$combined_df$price_factor) %in% c(NA, 1, 2, 3)))
})



# Test for plot generation - FAILED AGAIN TO EXTRACT TITLES/LABELS
#test_that("Plot is generated without errors", {
#  cities <- c('Kelowna', 'Penticton', 'Red Deer')
#  result <- AnalyzeRatings(cities)
#  expect_no_error({
#    category <- str_to_title(result$parameters$category)
#    # Create the plot directly
#    p <- ggplot(result$combined_df, aes(x = rating, fill = City, color = City)) +
#      geom_density(alpha = 0.6) +  # Density plot with transparency
#      labs(x = "Rating", y = "Density", title = paste("Business Ratings in the", category, "Sector")) + # Updated title with category
#      scale_fill_discrete(name = "City") +  # Custom legend title for fill color
#      scale_color_discrete(name = "City") +  # Custom legend title for color
#      facet_wrap(~ City, ncol = 1, scales = "free_y", strip.position = "bottom", shrink = TRUE, nrow = length(unique(result$combined_df$City))) +  # Separate plots for each city with increased height
#      theme(strip.text = element_blank())  # Remove city names from subplots
#    invisible(p)  # Suppress plot display in the test output
#  })
#})

# Test for plot generation
test_that("Plot is generated without errors", {
  cities <- c('Kelowna', 'Penticton', 'Red Deer')
  result <- AnalyzeRatings(cities)
  expect_no_error({
    category <- str_to_title(result$parameters$category)
    # Create the plot directly
    p <- ggplot(result$combined_df, aes(x = rating, fill = City, color = City)) +
      geom_density(alpha = 0.6) +  # Density plot with transparency
      labs(x = "Rating", y = "Density", title = paste("Business Ratings in the", category, "Sector")) + # Updated title with category
      scale_fill_discrete(name = "City") +  # Custom legend title for fill color
      scale_color_discrete(name = "City") +  # Custom legend title for color
      facet_wrap(~ City, ncol = 1, scales = "free_y", strip.position = "bottom", shrink = TRUE, nrow = length(unique(result$combined_df$City))) +  # Separate plots for each city with increased height
      theme(strip.text = element_blank())  # Remove city names from subplots
    # Check if the plot object is created
    expect_true(!is.null(p), info = "Plot object should not be NULL")
    invisible(p)  # Suppress plot display in the test output
  })
})



# Test for plot customization - FAILED AGAIN EXP. VALUES CHAR BUT IT IS A LIST
#test_that("Plot is customized with correct title and labels", {
#  cities <- c('Kelowna', 'Penticton', 'Red Deer')
#  result <- AnalyzeRatings(cities)
#  category <- str_to_title(result$parameters$category)
#  # Create the plot directly
#  p <- ggplot(result$combined_df, aes(x = rating, fill = City, color = City)) +
#    geom_density(alpha = 0.6) +  # Density plot with transparency
#    labs(x = "Rating", y = "Density", title = paste("Business Ratings in the", category, "Sector")) + # Updated title with category
#    scale_fill_discrete(name = "City") +  # Custom legend title for fill color
#    scale_color_discrete(name = "City") +  # Custom legend title for color
#    facet_wrap(~ City, ncol = 1, scales = "free_y", strip.position = "bottom", shrink = TRUE, nrow = length(unique(result$combined_df$City))) +  # Separate plots for each city with increased height
#    theme(strip.text = element_blank())  # Remove city names from subplots
#  # Check if the plot object is created
#  expect_true(!is.null(p), info = "Plot object should not be NULL")
#  # Check if the plot is customized correctly
#  expect_equal(ggplot2::ggtitle(p), paste("Business Ratings in the", category, "Sector"), 
#               info = "Plot title should match expected format")
#  expect_equal(ggplot2::xlab(p), "Rating", 
#               info = "X-axis label should be 'Rating'")
#  expect_equal(ggplot2::ylab(p), "Density", 
#               info = "Y-axis label should be 'Density'")
#})

# Test for plot customization
test_that("Plot is customized with correct title and labels", {
  cities <- c('Kelowna', 'Penticton', 'Red Deer')
  result <- AnalyzeRatings(cities)
  category <- str_to_title(result$parameters$category)
  # Create the plot directly
  p <- ggplot(result$combined_df, aes(x = rating, fill = City, color = City)) +
    geom_density(alpha = 0.6) +  # Density plot with transparency
    labs(x = "Rating", y = "Density", title = paste("Business Ratings in the", category, "Sector")) + # Updated title with category
    scale_fill_discrete(name = "City") +  # Custom legend title for fill color
    scale_color_discrete(name = "City") +  # Custom legend title for color
    facet_wrap(~ City, ncol = 1, scales = "free_y", strip.position = "bottom", shrink = TRUE, nrow = length(unique(result$combined_df$City))) +  # Separate plots for each city with increased height
    theme(strip.text = element_blank())  # Remove city names from subplots
  # Check if the plot object is created
  expect_true(!is.null(p), info = "Plot object should not be NULL")
  # Extract the titles and labels
  plot_title <- as.character(ggplot2::ggtitle(p))
  x_label <- as.character(ggplot2::xlab(p))
  y_label <- as.character(ggplot2::ylab(p))
  # Check if the plot is customized correctly
  expect_equal(plot_title, paste("Business Ratings in the", category, "Sector"), 
               info = "Plot title should match expected format")
  expect_equal(x_label, "Rating", 
               info = "X-axis label should be 'Rating'")
  expect_equal(y_label, "Density", 
               info = "Y-axis label should be 'Density'")
})


