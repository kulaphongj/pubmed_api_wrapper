\name{analyze_business_sectors}
\alias{analyze_business_sectors}
\title{Analyze Business Sectors}
\usage{
analyze_business_sectors(api_key = NULL, location = NULL, categories = NULL, limit = 20)
}
\description{
This function retrieves business data from Yelp API for a specified city and categories, analyzes the ratings, and generates facetted and interactive plots.
}
\arguments{
\item{api\_key}{A character string representing the Yelp API key. If NULL, the user will be prompted to enter the API key.}
\item{location}{A character string representing the name of the city to analyze. If NULL, the user will be prompted to enter a city.}
\item{categories}{A character vector containing the names of categories to analyze. If NULL, the user will be prompted to enter the categories.}
\item{limit}{An integer specifying the maximum number of businesses to retrieve per category. Default is 20.}
}
\value{A list containing the combined dataframe of business ratings for all categories, the parameters used, and both facetted and interactive plots.}
\examples{
\dontrun{
  # Example usage
  analyze_business_sectors()
  analyze_business_sectors("your_api_key", "Kelowna", c("food", "gyms", "golf"), 33)
  analyze_business_sectors(api_key = "your_api_key", location = "Kelowna", categories = c("food", "gyms", "golf"), limit = 33)
}
}
