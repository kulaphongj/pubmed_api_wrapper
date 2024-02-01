library(testthat)
library(tools)
library(plotly)
library(dplyr)
print(getwd())

source("../R/geo_heatmap.R")


test_that("Test for geo_heatmap function", 
  {
    
  # Generate Data
    data_vector <- c(
      "Acquafarina", 23, 4.0, 1992.9025, NA,
      "Al Porto", 242, 3.5, 2428.8042, "$$",
      "Ask For Luigi", 508, 4.0, 2711.4871, "$$",
      "Autostrada", 41, 4.5, 2168.4966, NA,
      "Autostrada 2", 15, 3.5, 1635.0210, NA, 
      "Autostrada 3", 102, 4.0, 2619.2281, "$$$", 
      "Bacaro", 26, 4.0, 2751.1394, NA,
      "Bar Corso", 15, 4.5, 3498.8843, NA, 
      "Bonta Italian Ristorante", 12, 4.0, 2120.8692, NA,
      "Casereccio Foods", 44, 4.5, 2995.8449, NA
      )
 
  # Specify column names and data types
  col_names <- c("name", "review_count", "rating", "distance", "price")
  col_classes <- c("character", "double", "double", "double", "character")
  
  # Create the data frame with specified column names and data types
  restaurant_data <- data.frame(matrix(data_vector, ncol = length(col_names), byrow = TRUE),
                                stringsAsFactors = FALSE)
  colnames(restaurant_data) <- col_names
  restaurant_data <- type.convert(restaurant_data, as.is = TRUE)
  
  # create coordinates dataframe
  latlong <- list(
    "latitude" = c(49.28119, 49.28467, 49.28445, 49.28232, 49.27444, 
                   49.24172, 49.28802, 49.27071, 49.27998, 49.26420),
    "longitude" = c(-123.1154, -123.1093, -123.0977, -123.1115, -123.1309,
                    -123.1018, -123.1166, -123.0695, -123.1303, -123.1574)
  )
  df_latlong <-as.data.frame(latlong)
  restaurant_data$coordinates <- df_latlong

  
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



