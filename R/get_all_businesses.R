# Function to get all businesses by paginating through Yelp search results
library(httr)
library(jsonlite)
source("search_businesses.R")

# Function to get all businesses by paginating through Yelp search results
get_all_businesses <- function(api_key, location, business_type, keyword, total = 1000) {
  results <- list()

  for (offset in seq(0, total, by = 50)) {
    partial_results <- search_businesses(api_key, location, business_type, keyword, offset, 50)

    if (!is.null(partial_results) && length(partial_results) > 0) {
      results <- c(results, list(partial_results))
    } else {
      break
    }
  }
  
  # Flatten the list of results and convert it to a data frame
  all_businesses_df <- bind_rows(results)
  return(all_businesses_df)
}
