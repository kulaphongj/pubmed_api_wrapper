# Load Libraries
source("search_businesses.R")
source("get_all_businesses.R")


# Example usage
#api_key <- 'your_api_key'  # Replace with your actual API key
#all_results <- GetAllBusinesses(api_key, 'Kelowna', 'restaurants', 'sushi', total = 100)


# Function to create a word cloud from business data
create_word_cloud <- function(data) {
  # Extract names and ratings from the data
  names <- data$name
  ratings <- data$rating

  # Ensure ratings are numeric
  ratings <- as.numeric(ratings)

  # Set the size of the output image
  jpeg(filename = "wordcloud.jpg", width = 1280, height = 1024) # Adjust width and height as needed

  # Define a color palette for the word cloud
  palette <- brewer.pal(8, "Dark2")

  # Generate the word cloud with ratings as the size factor
  wordcloud(names, freq = ratings, min.freq = 1, max.words = 300, rot.per = 0.25, scale = c(3.6, 0.1), colors = palette, random.order = FALSE)

  # Turn off the device to save the plot to the file
  dev.off()
}

# Create a word cloud from the retrieved business data
#create_word_cloud(all_results)
