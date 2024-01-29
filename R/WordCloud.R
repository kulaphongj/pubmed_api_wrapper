# Load required libraries
library(httr)
library(jsonlite)

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

# Function to get all businesses by paginating through Yelp search results
GetAllBusinesses <- function(api_key, location, business_type, keyword, total = 100) {
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

# Example usage
api_key <- 'your_api_key'  # Replace with your actual API key
all_results <- GetAllBusinesses(api_key, 'Kelowna', 'restaurants', 'fries', total = 100)

# Define a color palette for the word cloud
palette <- brewer.pal(8, "Dark2")

# Function to create a word cloud from business data
create_word_cloud <- function(data) {
  # Extract names and ratings from the data
  names <- data$name
  ratings <- data$rating
  
  # Ensure ratings are numeric
  ratings <- as.numeric(ratings)
  
  # Set the size of the output image
  jpeg(filename = "wordcloud.jpg", width = 1280, height = 1024) # Adjust width and height as needed
  
  # Generate the word cloud with ratings as the size factor
  wordcloud(names, freq = ratings, min.freq = 1, max.words = 300, rot.per = 0.25, scale = c(3.6, 0.1), colors = palette, random.order = FALSE)
  
  # Turn off the device to save the plot to the file
  dev.off()
}

# Create a word cloud from the retrieved business data
create_word_cloud(all_results)
