library(jsonlite)
library(magrittr)
library(purrr)
library(httr)
library(stringr)
library(plotly)

# Assign API key to variable beforehand for security purposes using Base R
# Set your API key as an environmental variable IN CURRENT R SESSION ONLY
Sys.setenv(YELP_API = "<<<ENTER YOUR API KEY HERE>>>")

# Extract the value of the YELP_API environment variable
yelp_api_key <- Sys.getenv("YELP_API")

# Obtain the API key from the environmental variable & assign variable locally
api_key <- Sys.getenv("YELP_API")
api_key



analyze_businesses_sectors <- function(city, categories = NULL, limit = '20') {
  # Define the main domain of the url
  domain <- "https://api.yelp.com/v3"
  
  # Define the Authorization token with the API key
  token <- paste("Bearer", api_key, sep = " ")
  
  # Create a dictionary to store parameters
  parameters <- list(
    cities = city,
    categories = categories,
    limit = limit
  )
  
  # Create an empty list to store results for each category
  category_results <- list()
  
  # Iterate over each category
  for (category in categories) {
    # Update the parameters for the current category
    parameters_used_category <- list(
      location = city,
      categories = category,
      sort_by = 'best_match',
      limit = limit
    )
    
    # Define the URL for businesses search endpoint
    url_businesses <- paste0(domain, "/businesses/search?")
    
    # Request the data
    response <- httr::GET(url_businesses, httr::add_headers(Authorization = token), query = parameters_used_category)
    
    # Check if the request was successful
    if (httr::status_code(response) != 200) {
      stop("Failed to retrieve data.")
    }
    
    # Extract the requested data
    raw_data <- httr::content(response, encoding = "UTF-8", as = "text") %>%
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
      # If 'price' column doesn't exist, load remaining columns
      df_ratings <- df_businesses %>%
        dplyr::select(name, review_count, rating)
    }
    
    # Store dataframe for each category
    category_results[[category]] <- df_ratings
  }
  
  # Combine dataframes for all categories into one dataframe
  combined_df <- dplyr::bind_rows(category_results, .id = "Category")
  
  # Return both the combined dataframe and the parameters used
  return(list(combined_df = combined_df, parameters = parameters))
}

# Example usage with categories
categories <- c('food', 'gyms', 'golf', 'nightlife', 'shopping', 'airports', 'casinos', 'hotelstravel', 'plumbing', 'roofing')
result <- analyze_businesses_sectors('Red Deer', categories, 33)

# Extract city name from parameters
city <- result$parameters$cities

# Density plot for comparing rating density across different categories
p <- ggplot(result$combined_df, aes(x = rating, fill = Category, color = Category)) +
     geom_density(alpha = 0.6) +  # Density plot with transparency
     labs(x = "Rating", y = "Density", title = paste("Business Sector Ratings in", city)) + # Updated title with city name
     scale_fill_discrete(name = "Category") +  # Custom legend title for fill color
     scale_color_discrete(name = "Category") +  # Custom legend title for color
     facet_wrap(~ Category, ncol = 1, scales = "free_y", strip.position = "bottom", shrink = TRUE, nrow = length(unique(result$combined_df$Category))) +  # Separate plots for each category with increased height
     theme(strip.text = element_blank()) + # Remove category names from subplots
     theme(strip.text = element_blank(), axis.text.y = element_blank())  # Remove city names and y-axis labels from subplots

p
