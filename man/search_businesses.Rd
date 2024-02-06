\name{search_businesses}
\alias{search_businesses}
\title{Search Businesses on Yelp}
\usage{
search_businesses(api_key, location, business_type, keyword, offset = 0, limit = 50)
}
\description{
This function queries the Yelp Fusion API to search for businesses based on specified criteria. It constructs a request with the provided parameters and fetches business data from Yelp, returning a list of businesses that match the search criteria.
}
\arguments{
\item{api_key}{A string containing the Yelp API key for authorization.}

\item{location}{A string specifying the location to search for businesses.}

\item{business_type}{A string specifying the type of business (e.g., restaurants, bars).}

\item{keyword}{A string specifying additional search keywords (e.g., "pizza", "coffee").}

\item{offset}{An integer specifying the offset for pagination. The default is 0.}

\item{limit}{An integer specifying the maximum number of results to return. The default is 50, and the maximum is 50 as per Yelp API limits.}
}
\value{
A list of businesses matching the search criteria if successful; NULL otherwise.
}
\examples{
\dontrun{
  # Example usage
  businesses <- search_businesses("your_api_key", "San Francisco", "food", "pizza")
  print(businesses)
}
}
