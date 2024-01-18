# Install and load the httr and xml2 libraries
# install.packages(c("httr", "xml2"))
library(httr)
library(xml2)

# PubMed API base URL
base_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"

# Your PubMed API key (optional but recommended for higher usage)
api_key <- "02465aadea0bbf3d5ab40feecdaf6634f30a"

# Function to make a generic PubMed API request
pubmed_api_request <- function(endpoint, params = list()) {
  # Construct the URL for the API request
  url <- modify_url(base_url, path = endpoint, query = c(params, list(api_key = api_key)))
  print(url)
  # Make the HTTP GET request
  response <- GET(url)
  
  # Check if the request was successful (status code 200)
  if (response$status_code == 200) {
    # Parse the XML response
    result <- content(response, "text")
    return(result)
  } else {
    stop(paste("Error:", http_status(response)$reason))
  }
}

# Function to search PubMed using the ESearch utility
search_pubmed <- function(query) {
  endpoint <- "entrez/eutils/esearch.fcgi"
  params <- list(db = "pubmed", term = query)
  result <- pubmed_api_request(endpoint, params)
  cat("PubMed Search Results:\n", result, "\n")
}

# Function to retrieve details of a specific article by PubMed ID using EFetch
get_article_details <- function(pubmed_id) {
  endpoint <- "entrez/eutils/efetch.fcgi"
  params <- list(db = "pubmed", id = pubmed_id, retmode = "xml")
  result <- pubmed_api_request(endpoint, params)
  
  # Parse the XML response to extract relevant information
  doc <- read_xml(result)
  title <- xml_text(xml_find_all(doc, "//ArticleTitle"))
  abstract <- xml_text(xml_find_all(doc, "//AbstractText"))
  authors <- xml_text(xml_find_all(doc, "//Author/LastName")) # Extracting only last names for simplicity
  
  cat("Article Details:\n")
  cat("Title:", title, "\n")
  cat("Authors:", authors, "\n")
  cat("Abstract:", abstract, "\n")
}

# Example: Search PubMed for articles related to "R programming"
search_pubmed("R programming")

# Example: Get details of a specific article by PubMed ID
get_article_details("38231332")  # Replace with a valid PubMed ID

