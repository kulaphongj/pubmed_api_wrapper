# Load the necessary setup configurations and libraries
source("setup.R")

#' Search Businesses on Yelp
#'
#' This function queries the Yelp Fusion API to search for businesses based on specified criteria.
#' It constructs a request with the provided parameters and fetches business data from Yelp.
#'
#' @param api_key string containing the Yelp API key for authorization.
#' @param location string specifying the location to search for businesses.
#' @param business_type string specifying the type of business (e.g., restaurants, bars).
#' @param keyword string specifying additional search keywords (e.g., "pizza", "coffee").
#' @param offset integer specifying the offset for pagination (default is 0).
#' @param limit integer specifying the maximum number of results to return (default is 50, max is 50 as per Yelp API limits).
#' @return A list of businesses matching the search criteria if successful; NULL otherwise.
#' @examples
#' search_businesses(api_key, "San Francisco", "food", "pizza")
search_businesses <- function(api_key, location, business_type, keyword, offset = 0, limit = 50) {
  # Construct the request URL and headers for Yelp API
  url <- 'https://api.yelp.com/v3/businesses/search'
  headers <- add_headers('Authorization' = sprintf('Bearer %s', api_key))
  # Set query parameters with the function's arguments
  params <- list(location = location, term = paste(business_type, keyword), limit = limit, offset = offset)
  # Perform the GET request to Yelp API
  response <- httr::GET(url, headers, query = params)
  # Check response status and parse content if successful
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, type = "text", encoding = "UTF-8")
    return(fromJSON(content)$businesses)
  } else {
    # Handle error by printing status code and returning NULL
    print(paste('Error:', httr::status_code(response)))
    return(NULL)
  }
}
