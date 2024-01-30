# Main function to search for businesses on Yelp using the Yelp Fusion API
library(httr)
library(jsonlite)

api_key = '8Jo6kJG76tJHDooxSbrcpFOm1dW2DTKJBQ_sNZuqHDn5FMmNTHe4xTuJXSblClvr5o6mKo4x2YzHsOonykbo7ajvVvWzGmjK0SpVxfGXmXqDWPu9Qivdyx-OjyewZXYx'


#' Search for businesses on Yelp using the Yelp Fusion API
#'
#' This function allows you to search for businesses on Yelp using the Yelp Fusion API.
#'
#' @param api_key Your Yelp Fusion API key.
#' @param location The location where you want to search for businesses (e.g., 'Kelowna').
#' @param business_type The type of business you are looking for (e.g., 'restaurants').
#' @param keyword A keyword to narrow down the search (e.g., 'sushi').
#' @param offset The offset for paginating through results (default is 0).
#' @param limit The maximum number of results to return (default is 50).
#'
#' @return A list of businesses matching the search criteria.
#'
#' @examples
#' api_key <- 'your_api_key'  # Replace with your actual API key
#' results <- SearchBusinesses(api_key, 'Kelowna', 'restaurants', 'sushi', offset = 0, limit = 50)
#'
#' @seealso \code{\link{httr::GET}}, \code{\link{httr::add_headers}}, \code{\link{jsonlite::fromJSON}}
#'
#' @export
SearchBusinesses <- function(api_key, location, business_type, keyword, offset = 0, limit = 50) {
  # Define the API endpoint URL
  url <- 'https://api.yelp.com/v3/businesses/search'
  
  # Set the authorization headers with the API key
  headers <- add_headers('Authorization' = sprintf('Bearer %s', api_key))
  
  # Define parameters for the API request
  params <- list(
    location = location,
    term = paste(business_type, keyword),
    limit = limit,
    offset = offset
  )
  
  # Send the GET request to the Yelp Fusion API
  response <- httr::GET(url, headers, query = params)
  
  # Check if the response status code is 200 (OK)
  if (httr::status_code(response) == 200) {
    # Extract and return the list of businesses from the JSON response
    content <- httr::content(response, type = "text", encoding = "UTF-8")
    return(fromJSON(content)$businesses)
  } else {
    # Print an error message if the response is not successful
    print(paste('Error:', httr::status_code(response)))
    return(NULL)
  }
}
