library(testthat)
library(yelpviz)
library(tools)
library(plotly)
library(dplyr)
# source("./R/geo_heatmap.R")


context("yelpviz Tests")
test_that("Test for geo_heatmap function", 
  {

  # Generate Data
  data_vector <- c(
    "Acquafarina", 23, 4.0, 1992.9025, NA, 49.28119, -123.1154, 1, 0.1,
    "Al Porto", 242, 3.5, 2428.8042, "$$", 49.28467, -123.1093, 1, 2.0,
    "Ask For Luigi", 508, 4.0, 2711.4871, "$$", 49.28445, -123.0977, 1, 2.0,
    "Autostrada", 41, 4.5, 2168.4966, NA, 49.28232, -123.1115, 1, 0.1,
    "Autostrada 2", 15, 3.5, 1635.0210, NA, 49.27444, -123.1309, 2, 0.1,
    "Autostrada 3", 102, 4.0, 2619.2281, "$$$", 49.24172, -123.1018, 3, 3.0,
    "Bacaro", 26, 4.0, 2751.1394, NA, 49.28802, -123.1166, 1, 0.1,
    "Bar Corso", 15, 4.5, 3498.8843, NA, 49.27071, -123.0695, 1, 0.1,
    "Bonta Italian Ristorante", 12, 4.0, 2120.8692, NA, 49.27998, -123.1303, 1, 0.1,
    "Casereccio Foods", 44, 4.5, 2995.8449, NA, 49.26420, -123.1574, 1, 0.1
  )
  
  # Specify column names and data types
  col_names <- c("name", "review_count", "rating", "distance", "price", 
                 "latitude", "longitude", "RunningNumber", "price_factor")
  col_classes <- c("character", "double", "double", "double", "character", 
                   "double", "double", "integer", "double")
  
  # Create the data frame with specified column names and data types
  restaurant_data <- data.frame(matrix(data_vector, ncol = length(col_names), byrow = TRUE),
                                stringsAsFactors = FALSE)
  colnames(restaurant_data) <- col_names
  restaurant_data <- type.convert(restaurant_data, as.is = TRUE)
    
    
  # run function to get data
  plot_geo_res <- geo_heatmap(restaurant_data, factor_plot = 'price_factor')
  
  # Check if the plot_object is a plotly object
  expect_is(plot_geo_res, "plotly")
  # Check if the title is as expected
  expect_equal(plot_geo_res$x$attrs[[1]]$marker$colorbar$title, "Price Factor")
  # Check if the type is as expected
  expect_equal(plot_geo_res$x$attrs[[1]]$type, "scattermapbox")
  }
)



