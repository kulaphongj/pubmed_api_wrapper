# Load the necessary setup configurations and libraries
source("setup.R")
source("search_businesses.R")

#' Retrieve All Businesses Matching Criteria
#'
#' This function iterates over paginated search results from the Yelp Fusion API, collecting all businesses
#' that match the search criteria up to a specified total number of businesses.
#'
#' @param api_key string containing the Yelp API key for authorization.
#' @param location string specifying the location to search for businesses.
#' @param business_type string specifying the type of business to search for.
#' @param keyword string specifying additional search keywords.
#' @param total integer specifying the total number of businesses to attempt to retrieve (default is 1000).
#' @return A dataframe of all businesses matching the search criteria, up to the specified total.
#' @examples
#' get_all_businesses(api_key, "San Francisco", "food", "pizza", total = 200)
get_all_businesses <- function(api_key, location, business_type, keyword, total = 1000) {
  results <- list()
  # Loop through search results in increments of 50 (API limit) until reaching 'total' count
  for (offset in seq(0, total, by = 50)) {
    # Call the search function with current offset
    partial_results <- search_businesses(api_key, location, business_type, keyword, offset, 50)
    # Check if results were returned and add to the cumulative results list
    if (!is.null(partial_results) && length(partial_results) > 0) {
      results <- c(results, list(partial_results))
    } else {
      # Exit loop if no more results are found
      break
    }
  }
  # Combine all partial results into a single dataframe
  all_businesses_df <- bind_rows(results)
  return(all_businesses_df)
}
