#' Analyze Business Sectors
#'
#' This function retrieves business data from Yelp API for a specified city and categories, analyzes the ratings, and generates facetted and interactive plots.
#'
#' @param api_key A character string representing the Yelp API key. If NULL, the user will be prompted to enter the API key.
#' @param location A character string representing the name of the city to analyze. If NULL, the user will be prompted to enter a city.
#' @param categories A character vector containing the names of categories to analyze. If NULL, the user will be prompted to enter the categories.
#' @param limit An integer specifying the maximum number of businesses to retrieve per category. Default is 20.
#' @return A list containing the combined dataframe of business ratings for all categories, the parameters used, and both facetted and interactive plots.
#' @import httr jsonlite dplyr ggplot2 plotly stringr
#' @examples
#' analyze_business_sectors(api_key = "your_api_key", location = "Kelowna", categories = c("food", "gyms", "golf"), limit = 33)

analyze_business_sectors <- function(api_key = NULL, location = NULL, categories = NULL, limit = 20) {
  
  # Prompt user to enter API key if not provided
  while (is.null(api_key) || api_key == '') {
    cat("Please enter your Yelp API key: ")
    api_key <- readline()
    api_key <- trimws(api_key)  # Trim leading and trailing whitespace
  }
  
  # Prompt user to enter city if not provided
  while (is.null(location) || location == '') {
    cat("Please enter the city name: ")
    location <- readline()
    location <- trimws(location)  # Trim leading and trailing whitespace
  }
  
  # Prompt user to enter categories if not provided
  while (is.null(categories) || length(categories) == 0 || all(categories == '')) {
    cat("Please enter the categories (separated by commas if multiple): ")
    categories_input <- readline()
    categories <- strsplit(categories_input, ",")[[1]]
    categories <- trimws(categories)  # Trim leading and trailing whitespace
  }
  
  # Ensure categories is a string or a vector of strings
  if (!is.character(categories) || any(!nzchar(categories))) {
    stop("Categories must be a string or a vector of strings.")
  }
  
  # Sort categories alphabetically to ensure plotting function maps correctly
  categories <- sort(categories)
  
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

  # Check if all categories are valid
  invalid_categories <- setdiff(categories, unique(unlist(categories_yelp)))
  if (length(invalid_categories) > 0) {
    message("Invalid categories: ", paste(invalid_categories, collapse = ", "), "\n")
    cat("Please include only accepted Yelp categories: ", "\n", paste(unique(unlist(categories_yelp)), collapse = ", "), "\n")
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
  
  # Create an empty list to store results for each category
  category_results <- list()
  
  # Iterate over each category
  for (category in categories) {
    # Update the parameters for the current category
    parameters_used_category <- list(
      api_key = api_key,
      location = location,
      categories = category,
      sort_by = 'best_match',
      limit = limit
    )
    
    # Define the URL for businesses search endpoint
    url_businesses <- paste0(domain, "/businesses/search?")
    
    # Request the data
    response <- GET(url_businesses, add_headers(Authorization = token), query = parameters_used_category)
    
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
    
    # Store dataframe for each category
    category_results[[category]] <- df_ratings
  }
  
  # Combine dataframes for all categories into one dataframe
  combined_df <- bind_rows(category_results, .id = "Category")
  
  # Extract city name from parameters
  city <- str_to_title(parameters$location)

  
  
  # FACETTED PLOT  
  # Density plot for comparing rating density across different categories
  p <- ggplot(combined_df, aes(x = rating, fill = Category, color = Category)) +
    geom_density(alpha = 0.6, adjust = 0.9) +  # Density plot with transparency
    labs(x = "Rating", y = "Density", title = paste("Business Sector Ratings in", city)) + # Updated title with city name
    scale_fill_discrete(name = "Category") +  # Custom legend title for fill color
    scale_color_discrete(name = "Category") +  # Custom legend title for color
    facet_wrap(~ Category, ncol = 1, scales = "free_y", strip.position = "bottom", shrink = TRUE, nrow = length(unique(combined_df$Category))) +  # Separate plots for each category with increased height
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
  
  # Add density traces for each category
  for(i in seq_along(plot_data$data)){
    category <- plot_data$data[[i]]$group
    category_name <- unique(combined_df$Category)[category]
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
               title = paste("Business Sector Ratings in", city),
               xaxis = list(title = "Rating", showline = FALSE),
               yaxis = list(title = "Density",
                            showticklabels = FALSE,
                            showgrid = FALSE))
  
  # Return both the combined dataframe, the parameters used, and both plots
  return(list(combined_df = combined_df, parameters = parameters, plot_facetted = p, plot_interactive = p2))
}


