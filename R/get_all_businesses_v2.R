# Function to get all businesses by paginating through Yelp search results
library(httr)
library(jsonlite)
source("R/SearchBusinesses.R")

# Function to get all businesses by paginating through Yelp search results
get_all_businesses <- function(api_key, location, business_type, keyword, total = 1000) {
  # Initialize a list to store the results
  results <- list()
  
  # Loop through paginated results with a step size of 50
  for (offset in seq(0, total, by = 50)) {
    # Search for businesses with the specified parameters
    partial_results <- SearchBusinesses(api_key, location, business_type, keyword, offset)
    
    # Check if there are results and add them to the results list
    if (!is.null(partial_results) && length(partial_results) > 0) {
      results <- c(results, partial_results)
    } else {
      # Break the loop if there are no more results
      break
    }
  }
  
  # add params information from user to result data
  df_business <- c()
  df_business$data <- results
  df_business$location <- location
  df_business$business_type <- business_type
  df_business$keyword <- keyword
  
  # Return the combined list of results
  return(df_business)
}
