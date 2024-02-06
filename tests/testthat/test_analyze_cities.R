library(testthat)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)

# Source the file containing the function to be tested
# source("analyze_cities.R") # source("C:\\PATH\\TO\\FILE\\analyze_cities.R")

# Test for successful API request
test_that("Function retrieves data for each city", {
  api_key <- '8Jo6kJG76tJHDooxSbrcpFOm1dW2DTKJBQ_sNZuqHDn5FMmNTHe4xTuJXSblClvr5o6mKo4x2YzHsOonykbo7ajvVvWzGmjK0SpVxfGXmXqDWPu9Qivdyx-OjyewZXYx'
  location <- c('Kelowna', 'Penticton', 'Red Deer')
  categories <- 'food'
  result <- analyze_cities(api_key, location, categories, 20)
  expect_equal(result$parameters$api_key, api_key)
  expect_equal(result$parameters$location, location)
  expect_equal(result$parameters$categories, categories)
  expect_equal(as.character(result$parameters$limit), as.character(result$parameters$limit))
  expect_is(result, "list")
  expect_gt(nrow(result$combined_df), 0)
  expect_named(result, c("combined_df", "parameters", "plot_facetted", "plot_interactive"))
  expected_columns <- c("City", "name", "review_count", "rating", "price", "price_factor")
  expect_true(all(sapply(expected_columns, function(col) any(tolower(col) == tolower(names(result$combined_df))))))
  expect_true(all(levels(result$combined_df$price_factor) %in% c(NA, 1, 2, 3)))
})

# Test for API request failure handling with invalid city
test_that("Function handles API request failures", {
  api_key <- '8Jo6kJG76tJHDooxSbrcpFOm1dW2DTKJBQ_sNZuqHDn5FMmNTHe4xTuJXSblClvr5o6mKo4x2YzHsOonykbo7ajvVvWzGmjK0SpVxfGXmXqDWPu9Qivdyx-OjyewZXYx'
  location <- c('Kelowna', 'Invalid City', 'Red Deer')
  categories <- 'food'
  expect_error(analyze_cities(api_key, location, categories, 20), "Failed to retrieve data.")
})

# Test for plot generation
test_that("Plot is generated without errors", {
  api_key <- '8Jo6kJG76tJHDooxSbrcpFOm1dW2DTKJBQ_sNZuqHDn5FMmNTHe4xTuJXSblClvr5o6mKo4x2YzHsOonykbo7ajvVvWzGmjK0SpVxfGXmXqDWPu9Qivdyx-OjyewZXYx'
  location <- c('Kelowna', 'Penticton', 'Red Deer')
  categories <- 'food'
  result <- analyze_cities(api_key, location, categories, 20)
  expect_no_error({
    category <- str_to_title(result$parameters$categories)
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
    # Check if the plot is customized correctly
    expect_equal(p$labels$title, paste("Business Ratings in the", category, "Sector"), 
                 info = "Plot title should match expected format")
    expect_equal(p$labels$x, "Rating", 
                 info = "X-axis label should be 'Rating'")
    expect_equal(p$labels$y, "Density", 
                 info = "Y-axis label should be 'Density'")
  })
})



# Test for plotly part
test_that("Plotly plot is generated without errors", {
  api_key <- '8Jo6kJG76tJHDooxSbrcpFOm1dW2DTKJBQ_sNZuqHDn5FMmNTHe4xTuJXSblClvr5o6mKo4x2YzHsOonykbo7ajvVvWzGmjK0SpVxfGXmXqDWPu9Qivdyx-OjyewZXYx'
  location <- c('Kelowna', 'Penticton', 'Red Deer')
  categories <- 'food'
  result <- analyze_cities(api_key, location, categories, 20)
  expect_no_error({
    category <- str_to_title(result$parameters$categories)
    # Density plot for comparing rating density across different cities
    p <- ggplot(result$combined_df, aes(x = rating, fill = City, color = City)) +
      geom_density(alpha = 0.6) +  # Density plot with transparency
      labs(x = "Rating", y = "Density", title = paste("Business Ratings in the", category, "Sector")) + # Updated title with category
      scale_fill_discrete(name = "City") +  # Custom legend title for fill color
      scale_color_discrete(name = "City") +  # Custom legend title for color
      facet_wrap(~ City, ncol = 1, scales = "free_y", strip.position = "bottom", shrink = TRUE, nrow = length(unique(result$combined_df$City))) +  # Separate plots for each city with increased height
      theme(strip.text = element_blank())  # Remove city names from subplots
    
    # Extract data from ggplot object
    plot_data <- ggplot_build(p)
    
    # Create plotly object
    p <- plot_ly()
    
    # Add density traces for each city
    for(i in seq_along(plot_data$data)){
      city <- plot_data$data[[i]]$group
      city_name <- unique(result$combined_df$City)[city]
      p <- add_trace(p, 
                     x = plot_data$data[[i]]$x, 
                     y = plot_data$data[[i]]$y,
                     type = "scatter", 
                     mode = "lines", 
                     fill = "tozeroy",
                     fillcolor = plot_data$data[[i]]$City,
                     line = list(color = plot_data$data[[i]]$group),
                     name = city_name,
                     text = paste("City: ", city_name, "<br>Rating: ", plot_data$data[[i]]$x, "<br>Density: ", round(plot_data$data[[i]]$y, 2)),
                     hoverinfo = "text+x+y")
    }
    
    # Customize layout
    p <- layout(p, 
                title = paste("Business Ratings in the", category, "Sector"),
                xaxis = list(title = "Rating"),
                yaxis = list(title = "Density"))
    
    # Display the interactive plot
    p
  })
})


