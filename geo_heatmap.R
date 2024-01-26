library(httr)
library(jsonlite)
library(dplyr)


# Define the main domain of the url
domain <- "https://api.yelp.com/v3"

# Define the Authorization token
token <- ""

# Create a dictionary to hold the parameters for the API request
params <- list(
  location = 'Kelowna',
  categories = 'rehab',
  term = 'clinic',
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


# ============================================================================================================

library(plotly)

fig <- plot_ly(
  data = df_ratings,
  lat = ~latitude,
  lon = ~longitude,
  mode = 'markers',
  marker = list(
    size = ~price_factor,  # Use the formula ~price_factor to reference the column
    sizemode = 'diameter',  # Set sizemode to 'diameter' for direct control over marker size
    sizeref = 0.1,
    sizemin = 5,
    color = ~price_factor,
    colorscale = 'Viridis',
    colorbar = list(title = 'Price Factor')
  ),
  text = ~paste("Name: ", name, "<br>Price Factor: ", price_factor),
  type = 'scattermapbox'
) 

fig <- fig %>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom = 10,
      center = list(lon = mean(df_ratings$longitude, na.rm=TRUE), lat = mean(df_ratings$latitude, na.rm=TRUE))
    )
  ) 

fig
