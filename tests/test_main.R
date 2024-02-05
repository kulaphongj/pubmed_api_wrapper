# Load necessary libraries for testing
library(testthat)

# Source individual test files
source("test_analyze_business_sectors.R")
source("test_analyze_cities.R")
source("test_create_geo_heatmap.R")
source("test_search_businesses.R")

# Run all tests
test_check("yelpviz")
