\name{create_word_cloud}
\alias{create_word_cloud}
\title{Generate a Word Cloud from Yelp Business Data}
\usage{
create_word_cloud(data, save_to_file = FALSE, filename = "wordcloud.html")
}
\description{
This function generates a word cloud visualization based on the names of businesses, weighted by their ratings and number of reviews. The visualization highlights popular and highly rated businesses more prominently.
}
\arguments{
\item{data}{A dataframe containing Yelp business data, specifically the business names, ratings, and review counts.}

\item{save_to_file}{Logical indicating whether to save the word cloud to an HTML file. Defaults to FALSE, which means the word cloud will be printed instead of saved.}

\item{filename}{The name of the file to save the word cloud HTML if \code{save_to_file} is TRUE. Defaults to "wordcloud.html".}
}
\value{
Generates a word cloud and either saves it to a file or prints it, based on the \code{save_to_file} parameter. This function does not return a value.
}
\examples{
\dontrun{
  # Assuming `business_data` is a dataframe with Yelp business info
  create_word_cloud(business_data, TRUE, "my_wordcloud.html")
}
}
