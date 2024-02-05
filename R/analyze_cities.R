#' Analyze Cities
#'
#' This function retrieves business data from Yelp API for specified cities and category, analyzes the ratings, and generates facetted and interactive plots.
#'
#' @param api_key A character string representing the Yelp API key. If NULL, the user will be prompted to enter the API key.
#' @param location A character vector containing the names of cities to analyze. If NULL, the user will be prompted to enter the cities.
#' @param categories A character string representing the category of businesses to analyze. If NULL, the user will be prompted to enter a category.
#' @param limit An integer specifying the maximum number of businesses to retrieve per city. Default is 20.
#' @return A list containing the combined dataframe of business ratings for all cities, the parameters used, and both facetted and interactive plots.
#' @import httr jsonlite dplyr ggplot2 plotly stringr
#' @examples
#' analyze_cities()
#' analyze_cities("your_api_key", c("Kelowna", "Penticton", "Red Deer"), "food", 33)
#' analyze_cities(api_key = "your_api_key", location = c("Kelowna", "Penticton", "Red Deer"), categories = "food", limit = 33)

analyze_cities <- function(api_key = NULL, location = NULL, categories = NULL, limit = 20) {

  # Prompt user to enter API key if not provided
  while (is.null(api_key) || api_key == '') {
    cat("Please enter your Yelp API key: ")
    api_key <- readline()
    api_key <- trimws(api_key)  # Trim leading and trailing whitespace
  }
  
  # Prompt user to enter cities if not provided
  while (is.null(location) || length(location) == 0 || all(location == '')) {
    cat("Please enter the cities (separated by commas if multiple): ")
    cities_input <- readline()
    location <- strsplit(cities_input, ",")[[1]]
    location <- trimws(location)  # Trim leading and trailing whitespace
  }
  
  # Ensure cities is a string or a vector of strings
  if (!is.character(location) || any(!nzchar(location))) {
    stop("Cities must be a string or a vector of strings.")
  }
  
  # Prompt user to enter category if not provided or if multiple categories are entered
  while (is.null(categories) || length(categories) != 1 || categories == '') {
    cat("Please enter only one category name: ")
    categories <- readline()
    categories <- trimws(categories)  # Trim leading and trailing whitespace
  }
  
  # Sort cities alphabetically to ensure plotting function maps correctly
  cities <- sort(location)
  
  # Define the main domain of the url
  domain <- "https://api.yelp.com/v3"
  
  # Define the Authorization token with the API key
  token <- paste("Bearer", api_key, sep = " ")

  # UNCOMMENT TO CHECK FOR CATEGORIES; COMMENTED OUT TO SAVE REQUESTS  
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
  categories_yelp <- sapply(raw_data_categories$categories, function(categories) {
    if ("alias" %in% names(categories)) {
      unlist(categories[["alias"]])
    } else {
      NA
    }
  })

  # Check if the category is in the list of accepted categories
  invalid_categories <- setdiff(categories, unique(unlist(categories_yelp)))
  if (!(is.null(categories) || categories %in% unique(unlist(categories_yelp)))) {
    message("Invalid category: ", paste(invalid_categories, collapse = ", "), "\n")
    cat("Please use only one of the accepted Yelp categories: ", "\n", paste(unique(unlist(categories_yelp)), collapse = ", "), "\n")
    categories <- readline()
    categories <- trimws(categories)  # Trim leading and trailing whitespace
  }
  
  # Create a dictionary to store parameters
  parameters <- list(
    api_key = api_key,
    location = location,
    categories = categories,
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
      categories = categories,
      sort_by = 'best_match',
      limit = limit
    )
    
    # Define the URL for businesses search endpoint
    url_businesses <- paste0(domain, "/businesses/search?")
    
    # Request the data
    response <- GET(url_businesses, add_headers(Authorization = token), query = parameters_used_city)
    
    # Check if the request was successful
    if (status_code(response) != 200) {
      stop("Failed to retrieve data.")
    }

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
  }
  
  # Combine dataframes for all cities into one dataframe
  combined_df <- bind_rows(city_results, .id = "City")
  
  # Extract category from parameters
  category <- str_to_title(parameters$categories)

  
  
  # FACETTED PLOT  
  # Density plot for comparing rating density across different cities
  p <- ggplot(combined_df, aes(x = rating, fill = City, color = City)) +
    geom_density(alpha = 0.6, adjust = 0.9) +  # Density plot with transparency
    labs(x = "Rating", y = "Density", title = paste("Business Ratings in the", category, "Sector")) + # Updated title with category
    scale_fill_discrete(name = "City") +  # Custom legend title for fill color
    scale_color_discrete(name = "City") +  # Custom legend title for color
    facet_wrap(~ City, ncol = 1, scales = "free_y", strip.position = "bottom", shrink = TRUE, nrow = length(unique(combined_df$City))) +  # Separate plots for each city with increased height
    theme_minimal() +
    theme(strip.text = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),  # Remove category names, y-axis labels, and y-axis ticks from subplots
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())  # Remove horizontal grid lines
          # panel.grid = element_blank())  # Remove grid lines

  
  
  # INTERACTIVE PLOT
  # Extract data from ggplot object
  plot_data <- ggplot_build(p)
  
  # Create plotly object
  p2 <- plot_ly()
  
  # Add density traces for each city
  for(i in seq_along(plot_data$data)){
    city <- plot_data$data[[i]]$group
    city_name <- unique(combined_df$City)[city]
    p2 <- add_trace(p2, 
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
  p2 <- layout(p2, 
               title = paste("Business Ratings in the", category, "Sector"),
               xaxis = list(title = "Rating", showline = FALSE),
               yaxis = list(title = "Density",
                            showticklabels = FALSE,
                            showgrid = FALSE))
  
  # Return both the combined dataframe, the parameters used, and both plots
  return(list(combined_df = combined_df, parameters = parameters, plot_facetted = p, plot_interactive = p2))
}


