# Main function to search for businesses on Yelp using the Yelp Fusion API
library(httr)
library(jsonlite)

api_key = '8Jo6kJG76tJHDooxSbrcpFOm1dW2DTKJBQ_sNZuqHDn5FMmNTHe4xTuJXSblClvr5o6mKo4x2YzHsOonykbo7ajvVvWzGmjK0SpVxfGXmXqDWPu9Qivdyx-OjyewZXYx'

# Function to search for businesses on Yelp using the Yelp Fusion API
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
