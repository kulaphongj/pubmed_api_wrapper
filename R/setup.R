# /** @file Yelp API data retrieval and visualization
#  *  @brief Retrieves data from Yelp Fusion API and visualizes it.
#  *
#  *  This script uses the Yelp Fusion API to retrieve data and utilizes various R packages
#  *  for data processing and visualization. It ensures all required libraries are installed
#  *  and loaded dynamically before executing the main functionality.
#  *
#  *  @author Nijiati Abulizi
#  *  @version 1.0
#  *  @date 2024-02-05
#  */

# Function to check and install required packages
check_and_install <- function(package){
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# List of required libraries
required_packages <- c("httr", "jsonlite", "dplyr", "RColorBrewer", "wordcloud2", "htmlwidgets")

# Check and install missing libraries
sapply(required_packages, check_and_install)

# API key for Yelp Fusion API
# Note: Store API keys securely and never expose them in shared or public scripts.
api_key <- '8Jo6kJG76tJHDooxSbrcpFOm1dW2DTKJBQ_sNZuqHDn5FMmNTHe4xTuJXSblClvr5o6mKo4x2YzHsOonykbo7ajvVvWzGmjK0SpVxfGXmXqDWPu9Qivdyx-OjyewZXYx'
