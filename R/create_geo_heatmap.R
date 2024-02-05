# create_geo_heatmap ============================================================================================================
#' Generate a geographic heatmap using Plotly
#'
#' This function creates a geographic heatmap using the Plotly library in R. It takes a dataframe
#' with latitude, longitude, and other relevant data from the function get_all_businesses(), and produces an interactive map.
#'
#' @param df_loc A dataframe containing location data, including latitude, longitude, and other factors.
#'               
#' @param factor_plot The factor to be represented by the heatmap. The available factors are price_factor, rating, review_count, and weighted_rating_review
#'
#' @return A Plotly interactive heatmap.
#'
#' @examples
#' \dontrun{
#'   # Example usage
#'   heatmap <- create_geo_heatmap(df, "price_factor")
#'   heatmap
#' }
#'
#' @import plotly
#' @import dplyr
#' @import stringr
#' @import tools
create_geo_heatmap <- function(df_loc, factor_plot) {
  # data preprocessing for plotting
  # create data frame
  list_business_prep <- list(
    name = df_loc$name,
    review_count = df_loc$review_count,
    rating = df_loc$rating,
    distance = df_loc$distance,
    price = df_loc$price,
    latitude = df_loc$coordinates$latitude,
    longitude = df_loc$coordinates$longitude
  )
  df_business_factors <- data.frame(list_business_prep)
  df_business_factors <- df_business_factors[!with(df_business_factors,is.na(latitude)& is.na(longitude)),]
  
  # Convert numerical columns to numeric format
  numeric_columns <- c('review_count', 'rating', 'distance', 'latitude', 'longitude')
  df_business_factors[numeric_columns] <- lapply(df_business_factors[numeric_columns], as.numeric)
  
  # add weighted factor (weighted_rating_review <- ratings * (review_count/max(review_count)))
  df_business_factors$weighted_rating_review <- (df_business_factors$rating/max(df_business_factors$rating)) * (df_business_factors$review_count/max(df_business_factors$review_count))
  
  # Normalize weights for visualization
  min_weight <- min(df_business_factors$weighted_rating_review)
  max_weight <- max(df_business_factors$weighted_rating_review)
  df_business_factors$weighted_rating_review <- (df_business_factors$weighted_rating_review - min_weight) / (max_weight - min_weight) * 4 + 1
  
  # add name to place if they have many branches
  df_business_factors <- df_business_factors %>%
    arrange(name) %>%
    group_by(name) %>%
    mutate(RunningNumber = row_number())
  
  # Add a number to a string value when Value > 1
  df_business_factors <- df_business_factors %>%
    mutate(name = ifelse(RunningNumber > 1, paste(name, " ", RunningNumber, sep = ""), name))
  
  # Map price levels to factors
  # Add price factor column
  df_business_factors$price_factor <- 1
  df_business_factors$price_factor[df_business_factors$price == "$"] <- 1
  df_business_factors$price_factor[df_business_factors$price == "$$"] <- 2
  df_business_factors$price_factor[df_business_factors$price == "$$$"] <- 3
  df_business_factors$price_factor[df_business_factors$price == "$$$"] <- 4
  df_business_factors$price_factor[is.na(df_business_factors$price)] <- 0.1  # for plotting the unknow price
  
  # adjust ratio of point on the map based on the maximum value of factor
  max_val <- max(df_business_factors[[factor_plot]])
  if (max_val<=4){
    ratio_size <- 0.6/max_val  
  }else if (max_val>10){
    ratio_size <- max_val/50
  }else{
    ratio_size <- 0.7/max_val
  }
  
  # Clean string
  factor_plot_clean <- toTitleCase(gsub("_", " ", factor_plot))
  
  # Plot graph
  fig <- plot_ly(
    data = df_business_factors,
    lat = ~latitude,
    lon = ~longitude,
    mode = 'markers',
    marker = list(
      size = ~get(factor_plot),  # Use get() to dynamically access the specified column
      sizemode = 'diameter',
      sizeref = ratio_size,  # Set sizeref based on the maximum value of the factor_plot column
      sizemin = 5,
      color = ~get(factor_plot),
      colorscale = 'Viridis',
      colorbar = list(title = paste(factor_plot_clean))
    ),
    text = ~paste("Name: ", name, "<br>", factor_plot_clean, ":",
                  ifelse(get(factor_plot) == 0.1, "Unknown", get(factor_plot))),
    hoverinfo = "text",  # remove latitude and longitude in the tooltips
    type = 'scattermapbox'
  )
  
  # Plot Map layout
  fig <- fig %>%
    layout(
      mapbox = list(
        style = 'carto-positron',  #  Can change to 'open-street-map'
        zoom = 10,
        center = list(lon = median(df_business_factors$longitude, na.rm=TRUE), lat = median(df_business_factors$latitude, na.rm=TRUE))
      )
    ) 
  
  return(fig) 
}

