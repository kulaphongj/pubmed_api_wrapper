# Load the search functions required for data retrieval
source("search_functions.R")
source("get_all_businesses.R")

#' Create a Word Cloud from Yelp Business Data
#'
#' This function generates a word cloud visualization based on the names of businesses,
#' weighted by their ratings and number of reviews. The visualization highlights popular
#' and highly rated businesses more prominently.
#'
#' @param data A dataframe containing Yelp business data, specifically the business names,
#'        ratings, and review counts.
#' @param save_to_file Logical indicating whether to save the word cloud to an HTML file.
#'        Defaults to FALSE, which means the word cloud will be printed instead of saved.
#' @param filename The name of the file to save the word cloud HTML if `save_to_file` is TRUE.
#'        Defaults to "wordcloud.html".
#' @return Generates a word cloud and either saves it to a file or prints it, based on the
#'         `save_to_file` parameter.
#' @examples
#' # Assuming `business_data` is a dataframe with Yelp business info
#' create_word_cloud(business_data, TRUE, "my_wordcloud.html")
create_word_cloud <- function(data, save_to_file = FALSE, filename = "wordcloud.html") {
  # Extract relevant columns and calculate weights
  names <- data$name
  ratings <- as.numeric(data$rating)
  review_counts <- as.numeric(data$review_count)
  weights <- ratings * review_counts  # Weight by rating and review count

  # Normalize weights for visualization
  min_weight <- min(weights)
  max_weight <- max(weights)
  normalized_weights <- (weights - min_weight) / (max_weight - min_weight) * 99 + 1

  # Prepare data for word cloud
  cloud_data <- data.frame(word = names, freq = normalized_weights)
  cloud_data$freq <- as.numeric(cloud_data$freq)  # Ensure frequency is numeric
  if(any(is.na(cloud_data$freq))) {
    stop("NA values found in freq column. Check data conversion.")  # Data integrity check
  }

  # Generate word cloud
  wc <- wordcloud2(data = cloud_data, size = 0.5, color = 'random-light', backgroundColor = "black")

  # Output handling: save to file or print
  if (save_to_file) {
    saveWidget(wc, file = filename, selfcontained = TRUE)
  } else {
    print(wc)  # Directly print the word cloud if not saving
  }
}
