\name{get_all_businesses}
\alias{get_all_businesses}
\title{Retrieve All Businesses Matching Criteria}
\usage{
get_all_businesses(api_key, location, business_type, keyword, total = 1000)
}
\description{
This function iterates over paginated search results from the Yelp Fusion API, collecting all businesses that match the search criteria up to a specified total number of businesses. It utilizes the `search_businesses` function to fetch results in batches and compiles them into a single dataframe.
}
\arguments{
\item{api_key}{A string containing the Yelp API key for authorization.}

\item{location}{A string specifying the location to search for businesses.}

\item{business_type}{A string specifying the type of business to search for.}

\item{keyword}{A string specifying additional search keywords.}

\item{total}{An integer specifying the total number of businesses to attempt to retrieve. The default is 1000.}
}
\value{
A dataframe of all businesses matching the search criteria, up to the specified total.
}
\examples{
\dontrun{
  # Example usage
  all_businesses <- get_all_businesses("your_api_key", "San Francisco", "food", "pizza", total = 200)
  print(all_businesses)
}
}
