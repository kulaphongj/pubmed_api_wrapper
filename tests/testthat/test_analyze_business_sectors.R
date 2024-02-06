library(testthat)
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)

# Source the file containing the function to be tested
# source("analyze_business_sectors.R") # source("C:\\PATH\\TO\\FILE\\analyze_business_sectors.R")

# Test for correct parameters being used, successful API request, data for each category, 
# correct columns in the dataframe, and price factorization
test_that("Output is a list with combined dataframe, parameters, and plots", {
  api_key <- '8Jo6kJG76tJHDooxSbrcpFOm1dW2DTKJBQ_sNZuqHDn5FMmNTHe4xTuJXSblClvr5o6mKo4x2YzHsOonykbo7ajvVvWzGmjK0SpVxfGXmXqDWPu9Qivdyx-OjyewZXYx'
  location <- 'Kelowna'
  categories <- sort(c('food', 'gyms', 'golf'))
  result <- analyze_business_sectors(api_key, location, categories, 20)
  expect_equal(result$parameters$api_key, api_key)
  expect_equal(result$parameters$location, location)
  expect_equal(result$parameters$categories, categories)
  expect_equal(as.character(result$parameters$limit), as.character(result$parameters$limit))
  expect_is(result, "list")
  expect_gt(nrow(result$combined_df), 0)
  expect_named(result, c("combined_df", "parameters", "plot_facetted", "plot_interactive"))
  expected_columns <- c("name", "review_count", "rating", "price", "price_factor", "Category")
  expect_true(all(sapply(expected_columns, function(col) any(tolower(col) == tolower(names(result$combined_df))))))
  expect_true(all(levels(result$combined_df$price_factor) %in% c(NA, 1, 2, 3)))
})

# Test for API request failure handling with invalid city
test_that("Function handles API request failures", {
  api_key <- '8Jo6kJG76tJHDooxSbrcpFOm1dW2DTKJBQ_sNZuqHDn5FMmNTHe4xTuJXSblClvr5o6mKo4x2YzHsOonykbo7ajvVvWzGmjK0SpVxfGXmXqDWPu9Qivdyx-OjyewZXYx'
  location <- 'Invalid City'
  categories <- c('food', 'gyms', 'golf')
  expect_error(analyze_business_sectors(api_key, location, categories, 20), "Failed to retrieve data.")
})

# Test for ggplot generation
test_that("Plot is generated without errors", {
  api_key <- '8Jo6kJG76tJHDooxSbrcpFOm1dW2DTKJBQ_sNZuqHDn5FMmNTHe4xTuJXSblClvr5o6mKo4x2YzHsOonykbo7ajvVvWzGmjK0SpVxfGXmXqDWPu9Qivdyx-OjyewZXYx'
  location <- 'Kelowna'
  categories <- c('food', 'gyms', 'golf')
  result <- analyze_business_sectors(api_key, location, categories, 20)
  expect_no_error({
    city <- str_to_title(location)
    # Create the plot directly
    p <- ggplot(result$combined_df, aes(x = rating, fill = Category, color = Category)) +
      geom_density(alpha = 0.6) +  # Density plot with transparency
      labs(x = "Rating", y = "Density", title = paste("Business Sector Ratings in", city)) + # Updated title with city name
      scale_fill_discrete(name = "Category") +  # Custom legend title for fill color
      scale_color_discrete(name = "Category") +  # Custom legend title for color
      facet_wrap(~ Category, ncol = 1, scales = "free_y", strip.position = "bottom", shrink = TRUE, nrow = length(unique(result$combined_df$Category))) +  # Separate plots for each category with increased height
      theme_minimal() +
      theme(strip.text = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),  # Remove category names, y-axis labels, and y-axis ticks from subplots
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())  # Remove horizontal grid lines
    # Check if the plot object is created
    expect_true(!is.null(p), info = "Plot object should not be NULL")
    invisible(p)  # Suppress plot display in the test output
    # Check if the plot is customized correctly
    expect_equal(p$labels$title, paste("Business Sector Ratings in", city), 
                 info = "Plot title should match expected format")
    expect_equal(p$labels$x, "Rating", 
                 info = "X-axis label should be 'Rating'")
    expect_equal(p$labels$y, "Density", 
                 info = "Y-axis label should be 'Density'")
  })
})



# Test for plotly plot generation
test_that("Plotly plot is generated without errors", {
  api_key <- '8Jo6kJG76tJHDooxSbrcpFOm1dW2DTKJBQ_sNZuqHDn5FMmNTHe4xTuJXSblClvr5o6mKo4x2YzHsOonykbo7ajvVvWzGmjK0SpVxfGXmXqDWPu9Qivdyx-OjyewZXYx'
  categories <- c('food', 'gyms', 'golf')  # Define categories
  result <- analyze_business_sectors(api_key, 'Kelowna', categories, 20)  # Call the function
  
  expect_no_error({
    # Iterate over categories
    for (category in categories) {
      # Density plot for comparing rating density across different categories
      p <- ggplot(result$combined_df[result$combined_df$Category == category, ], aes(x = rating, fill = Category, color = Category)) +
        geom_density(alpha = 0.6) +  # Density plot with transparency
        labs(x = "Rating", y = "Density", title = paste("Business Ratings in the", category, "Sector")) + # Updated title with category
        scale_fill_discrete(name = "Category") +  # Custom legend title for fill color
        scale_color_discrete(name = "Category") +  # Custom legend title for color
        facet_wrap(~ Category, ncol = 1, scales = "free_y", strip.position = "bottom", shrink = TRUE, nrow = length(unique(result$combined_df$Category))) +  # Separate plots for each category with increased height
        theme(strip.text = element_blank())  # Remove category names from subplots
      
      # Extract data from ggplot object
      plot_data <- ggplot_build(p)
      
      # Create plotly object
      p2 <- plot_ly()
      
      # Add density traces for each category
      for(i in seq_along(plot_data$data)){
        category_name <- unique(result$combined_df$Category)[i]
        p2 <- add_trace(p2, 
                        x = plot_data$data[[i]]$x, 
                        y = plot_data$data[[i]]$y,
                        type = "scatter", 
                        mode = "lines", 
                        fill = "tozeroy",
                        fillcolor = plot_data$data[[i]]$Category,
                        line = list(color = plot_data$data[[i]]$group),
                        name = category_name,
                        text = paste("Category: ", category_name, "<br>Rating: ", plot_data$data[[i]]$x, "<br>Density: ", round(plot_data$data[[i]]$y, 2)),
                        hoverinfo = "text+x+y")
      }
      
      # Customize layout
      p2 <- layout(p2, 
                   title = paste("Business Sector Ratings in", 'Kelowna'),
                   xaxis = list(title = "Rating"),
                   yaxis = list(title = "Density"))
      
      # Display the interactive plot
      print(p2)
    }
  })
})


