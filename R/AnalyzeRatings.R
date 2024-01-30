library(jsonlite)
library(magrittr)


# Assign API key to variable beforehand for security purposes using Base R
# Set your API key as an environmental variable IN CURRENT R SESSION ONLY
Sys.setenv(YELP_API = "<<<ENTER YOUR API KEY HERE>>>")

# Extract the value of the YELP_API environment variable
yelp_api_key <- Sys.getenv("YELP_API")



# Obtain the API key from the environmental variable & assign variable locally
api_key <- Sys.getenv("YELP_API")
api_key



AnalyzeRatings <- function(cities, category = NULL, limit = '20') {
  # Define the main domain of the url
  domain <- "https://api.yelp.com/v3"
  
  # Define the Authorization token with the API key
  token <- paste("Bearer", api_key, sep = " ")
  
  # Create a dictionary to store parameters
  parameters <- list(
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
      location = city,
      categories = category,
      sort_by = 'best_match',
      limit = limit
    )
    
    # Define the URL for businesses search endpoint
    url_businesses <- paste0(domain, "/businesses/search?")
    
    # Request the data
    response <- httr::GET(url_businesses, httr::add_headers(Authorization = token), query = parameters_used_city)
    
    # Check if the request was successful
    if (httr::status_code(response) != 200) {
      stop("Failed to retrieve data.")
    }
    
    # Extract the requested data
    raw_data <- httr::content(response, "text") %>%
      jsonlite::fromJSON(simplifyVector = TRUE) %>%
      `[[`("businesses")
    
    # Create a DataFrame
    df_businesses <- as.data.frame(raw_data)
    
    # Select the columns of interest
    if ('price' %in% names(df_businesses)) {
      df_ratings <- df_businesses %>%
        dplyr::select(name, review_count, rating, price)
      
      # Convert price column to factor and factorize the values
      df_ratings$price_factor <- as.factor(df_ratings$price)
      levels(df_ratings$price_factor) <- c("$" = 1, "$$" = 2, "$$$" = 3, "nan" = NA)
    } else {
      # If 'price' column doesn't exist, print a message or load remaining columns
      # print("Price data not available for this category.")
      df_ratings <- df_businesses %>%
        dplyr::select(name, review_count, rating)
    }
    
    # Store dataframe for each city
    city_results[[city]] <- df_ratings
  }
  
  # Combine dataframes for all cities into one dataframe
  combined_df <- dplyr::bind_rows(city_results, .id = "City")
  
  # Return both the combined dataframe and the parameters used
  return(list(combined_df = combined_df, parameters = parameters))
}



# Example usage with cities
cities <- c('Kelowna', 'Penticton', 'Red Deer')
result <- AnalyzeRatings(cities, 'golf', 33)
#View(result)
#View(result$combined_df)
#View(result$parameters)



library(stringr)
library(plotly)

# Extract category from parameters
category <- str_to_title(result$parameters$category)

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


