\name{analyze_cities}
\alias{analyze_cities}
\title{Analyze Cities}
\usage{
analyze_cities(api_key, location, categories, limit)
}
\description{
This function retrieves business data from Yelp API for specified cities and category, analyzes the ratings, and generates facetted and interactive plots.
}
\arguments{
\item{api_key}{A character string representing the Yelp API key. If NULL, the user will be prompted to enter the API key.}
\item{location}{A character vector containing the names of cities to analyze. If NULL, the user will be prompted to enter the cities.}
\item{categories}{A character string representing the category of businesses to analyze. If NULL, the user will be prompted to enter a category.}
\item{limit}{An integer specifying the maximum number of businesses to retrieve per category. Default is 20.}
}
\value{A list containing the combined dataframe of business ratings for all cities, the parameters used, and both facetted and interactive plots.}
\examples{
\dontrun{
  # Example usage
  result <- analyze_cities()
  result <- analyze_cities("your_api_key", c("Kelowna", "Penticton", "Red Deer"), "food", 33)
  result <- analyze_cities(api_key = "your_api_key", location = c("Kelowna", "Penticton", "Red Deer"), categories = "food", limit = 33)
}
}