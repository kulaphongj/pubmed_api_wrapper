# Required libraries:
# library(httr)
# library(jsonlite)
# library(dplyr)
# library(ggplot2)
# library(plotly)
# library(stringr)

analyze_cities <- function(api_key = NULL, cities = NULL, category = NULL, limit = 20) {
  # Prompt user to enter API key if not provided
  while (is.null(api_key) || api_key == '') {
    cat("Please enter your Yelp API key: ")
    api_key <- readline()
    api_key <- trimws(api_key)  # Trim leading and trailing whitespace
  }
  
  # Prompt user to enter cities if not provided
  while (is.null(cities) || length(cities) == 0 || all(cities == '')) {
    cat("Please enter the cities (separated by commas if multiple): ")
    cities_input <- readline()
    cities <- strsplit(cities_input, ",")[[1]]
    cities <- trimws(cities)  # Trim leading and trailing whitespace
  }
  
  # Ensure cities is a string or a vector of strings
  if (!is.character(cities) || any(!nzchar(cities))) {
    stop("Cities must be a string or a vector of strings.")
  }
  
  # Prompt user to enter category if not provided
  while (is.null(category) || category == '') {
    cat("Please enter the category name: ")
    category <- readline()
    category <- trimws(category)  # Trim leading and trailing whitespace
  }
  
  # Sort cities alphabetically to ensure plotting function maps correctly
  cities <- sort(cities)
  
  # Define the main domain of the url
  domain <- "https://api.yelp.com/v3"
  
  # Define the Authorization token with the API key
  token <- paste("Bearer", api_key, sep = " ")
  
  # Define the URL for categories endpoint
  url_categories <- paste0(domain, "/categories")
  
  # Make request to categories endpoint
  response_categories <- GET(url_categories, add_headers(Authorization = token))
  
  # Check if the request was successful
  if (status_code(response_categories) != 200) {
    stop("Failed to retrieve categorical data.")
  }
  
  # Extracting category titles
  raw_data_categories <- content(response_categories, "parsed")
  categories_yelp <- sapply(raw_data_categories$categories, function(category) {
    if ("alias" %in% names(category)) {
      unlist(category[["alias"]])
    } else {
      NA
    }
  })
  
  # Check if the category is in the list of accepted categories
  if (!(is.null(category) || category %in% unique(unlist(categories_yelp)))) {
    stop("Invalid category. Please enter only one accepted category on Yelp.")
  }
  
  # Create a dictionary to store parameters
  parameters <- list(
    api_key = api_key,
    cities = cities,
    category = category,
    limit = limit
  )
  
  # Create an empty list to store results for each city
  city_results <- list()
  
  # Iterate over each city
  for (city in cities) {
    # Update the parameters for the current city
    parameters_used_city <- list(
      api_key = api_key,
      location = city,
      categories = category,
      sort_by = 'best_match',
      limit = limit
    )
    
    # Define the URL for businesses search endpoint
    url_businesses <- paste0(domain, "/businesses/search?")
    
    # Request the data
    response <- GET(url_businesses, add_headers(Authorization = token), query = parameters_used_city)
    
    # Check if the request was successful
    if (status_code(response) == 200) {
      # Extract the requested data
      raw_data <- content(response, encoding = "UTF-8", as = "text") %>%
        fromJSON(simplifyVector = TRUE) %>%
        `[[`("businesses")
      
      # Create a DataFrame
      df_businesses <- as.data.frame(raw_data)
      
      # Select the columns of interest
      if ('price' %in% names(df_businesses)) {
        df_ratings <- df_businesses %>%
          select(name, review_count, rating, price)
        
        # Convert price column to factor and factorize the values
        df_ratings$price_factor <- as.factor(df_ratings$price)
        levels(df_ratings$price_factor) <- c("$" = 1, "$$" = 2, "$$$" = 3, "nan" = NA)
      } else {
        # If 'price' column doesn't exist, load remaining columns
        df_ratings <- df_businesses %>%
          select(name, review_count, rating)
      }
      
      # Store dataframe for each city
      city_results[[city]] <- df_ratings
    } else {
      # Failed to retrieve data for this city
      warning(paste("Failed to retrieve data for", city))
    }
  }
  
  # Combine dataframes for all cities into one dataframe
  combined_df <- bind_rows(city_results, .id = "City")
  
  # Extract category from parameters
  category <- str_to_title(parameters$category)
  
  # Density plot for comparing rating density across different cities
  p <- ggplot(combined_df, aes(x = rating, fill = City, color = City)) +
    geom_density(alpha = 0.6, adjust = 0.9) +  # Density plot with transparency
    labs(x = "Rating", y = "Density", title = paste("Business Ratings in the", category, "Sector")) + # Updated title with category
    scale_fill_discrete(name = "City") +  # Custom legend title for fill color
    scale_color_discrete(name = "City") +  # Custom legend title for color
    facet_wrap(~ City, ncol = 1, scales = "free_y", strip.position = "bottom", shrink = TRUE, nrow = length(unique(combined_df$City))) +  # Separate plots for each city with increased height
    theme(strip.text = element_blank(), axis.text.y = element_blank())  # Remove city names and y-axis labels from subplots
  
  # Extract data from ggplot object
  plot_data <- ggplot_build(p)
  
  # Create plotly object
  p <- plot_ly()
  
  # Add density traces for each city
  for(i in seq_along(plot_data$data)){
    city <- plot_data$data[[i]]$group
    city_name <- unique(combined_df$City)[city]
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
  
  # Return both the combined dataframe, the parameters used, and the plot
  return(list(combined_df = combined_df, parameters = parameters, plot = p))
}


