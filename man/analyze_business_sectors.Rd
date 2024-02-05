% File: analyze_business_sectors.Rd
% Title: Analyze Business Sectors
% Description: This function retrieves business data from Yelp API for a specified city and categories, analyzes the ratings, and generates facetted and interactive plots.
% Usage: analyze_business_sectors(api_key = NULL, location = NULL, categories = NULL, limit = 20)
% Arguments:
%   \item{api\_key}{A character string representing the Yelp API key. If NULL, the user will be prompted to enter the API key.}
%   \item{location}{A character string representing the name of the city to analyze. If NULL, the user will be prompted to enter a city.}
%   \item{categories}{A character vector containing the names of categories to analyze. If NULL, the user will be prompted to enter the categories.}
%   \item{limit}{An integer specifying the maximum number of businesses to retrieve per category. Default is 20.}
% Details: This function interacts with the Yelp API to retrieve business data for the specified city and categories. It then analyzes the ratings of these businesses and generates facetted and interactive plots to visualize the data.
% Value: A list containing the combined dataframe of business ratings for all categories, the parameters used, and both facetted and interactive plots.
% @import httr jsonlite dplyr ggplot2 plotly stringr
% @examples
% analyze_business_sectors()
% analyze_business_sectors("your_api_key", "Kelowna", c("food", "gyms", "golf"), 33)
% analyze_business_sectors(api_key = "your_api_key", location = "Kelowna", categories = c("food", "gyms", "golf"), limit = 33)