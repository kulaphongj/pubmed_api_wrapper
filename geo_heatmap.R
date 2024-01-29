library(httr)
library(jsonlite)
library(dplyr)
library(tools)
library(plotly)

# Define the main domain of the url
domain <- "https://api.yelp.com/v3"

# Define the Authorization token
# token <- ""

# Create a dictionary to hold the parameters for the API request
params <- list(
  location = 'Vancouver',
  categories = 'restaurant',
  term = 'italian',
  sort_by = 'best_match',
  limit = 50
)

# Define the URL for businesses search endpoint
url_businesses <- paste0(domain, "/businesses/search?")

# Request the data
response <- GET(url_businesses, add_headers(Authorization = token), query = params)

# Check if the request was successful
if (status_code(response) != 200) {
  stop("Failed to retrieve data.")
}

# Extract the requested data
raw_data <- content(response, "text") %>%
  fromJSON(simplifyVector = TRUE) %>%
  `[[`("businesses")

# Create a DataFrame
df_businesses <- as.data.frame(raw_data)

# Select the columns of interest
df_ratings <- df_businesses %>%
  dplyr::select(name, review_count, rating, distance, price, coordinates)

df_ratings$latitude <- df_ratings$coordinates$latitude
df_ratings$longitude <- df_ratings$coordinates$longitude
df_ratings = select(df_ratings, -c(coordinates))

# Convert numerical columns to numeric format
numeric_columns <- c('review_count', 'rating', 'distance', 'latitude', 'longitude')
df_ratings[numeric_columns] <- lapply(df_ratings[numeric_columns], as.numeric)


df_ratings <- df_ratings %>%
  arrange(name) %>%
  group_by(name) %>%
  mutate(RunningNumber = row_number())

# Add a number to a string value when Value > 1
df_ratings <- df_ratings %>%
  mutate(name = ifelse(RunningNumber > 1, paste(name, " ", RunningNumber, sep = ""), name))



# Map price levels to factors
price_factors <- c("$" = 1, "$$" = 2, "$$$" = 3, "$$$$" = 4)
price_factors[is.na(price_factors)] <- NULL

# Add price factor column
df_ratings$price_factor <- price_factors[df_ratings$price]
df_ratings$price_factor[is.na(df_ratings$price_factor)] <- 0.1


# GeoHeatmap ============================================================================================================
#' Generate a geographic heatmap using Plotly
#'
#' This function creates a geographic heatmap using the Plotly library in R. It takes a dataframe
#' with latitude, longitude, and other relevant data, and produces an interactive map.
#'
#' @param df_loc A dataframe containing location data, including latitude, longitude, and other factors.
#' @param factor_plot The factor to be represented by the heatmap.
#'
#' @return A Plotly interactive heatmap.
#'
#' @examples
#' \dontrun{
#'   # Example usage
#'   heatmap <- geo_heatmap(df, "price_factor")
#'   plot(heatmap)
#' }
#'
#' @import plotly
#' @import dplyr
#' @import stringr
#' @import tools
geo_heatmap <- function(df_loc, factor_plot) {
  
  # adjust ratio of point on the map based on the maximum value of factor
  max_val <- max(df_loc[[factor_plot]])
  if (max_val<=4){
    ratio_size <- 0.6/max_val  
  }else if (max_val>10){
    ratio_size <- max_val/50
  }else{
    ratio_size <- 0.7/max_val
  }

  # Clean string
  factor_plot_clean <- toTitleCase(gsub("_", " ", factor_plot))
  title_name = paste(params$term, params$categories, "in", params$location, "by", factor_plot_clean)
  title_name_upperfirst <- toTitleCase(title_name)
  
  # Plot graph
  fig <- plot_ly(
    data = df_loc,
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
        center = list(lon = mean(df_loc$longitude, na.rm=TRUE), lat = mean(df_loc$latitude, na.rm=TRUE))
      ),
      title = title_name_upperfirst
    ) 
  
  return(fig) 
}

# call function
test <-  df_ratings[df_ratings$review_count<500, ]
test <- df_ratings

fig_geo_heat <- geo_heatmap(test, factor_plot='price_factor')
fig_geo_heat


fig_geo_heat <- geo_heatmap(test, factor_plot='rating')
fig_geo_heat


fig_geo_heat <- geo_heatmap(test, factor_plot='review_count')
fig_geo_heat

