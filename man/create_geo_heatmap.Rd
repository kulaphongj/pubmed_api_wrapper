\name{create_geo_heatmap}
\alias{create_geo_heatmap}
\title{Generate a geographic heatmap using Plotly}
\usage{
create_geo_heatmap(df_loc, factor_plot)
}
\description{
This function creates a geographic heatmap using the Plotly library in R. It takes a dataframe with latitude, longitude, and other relevant data from the function get_all_businesses(), and produces an interactive map.
}
\arguments{
\item{df_loc}{A dataframe containing location data, including latitude, longitude, and other factors.}

\item{factor_plot}{The factor to be represented by the heatmap. The available factors are price_factor, rating, review_count, and weighted_rating_review.}
}
\value{A Plotly interactive heatmap.}
\examples{
\dontrun{
  # Example usage
  heatmap <- create_geo_heatmap(df, "price_factor")
  heatmap
}
}
