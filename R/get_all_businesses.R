# Function to get all businesses by paginating through Yelp search results
library(httr)
library(jsonlite)
source("search_businesses.R")

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

  # Return the combined list of results
  return(results)
}
