source("R/GetAllBusinesses.R")
source("R/SearchBusinesses.R")
source("R/geo_heatmap.R")

token <- "TpL2Xxv5ezpTSAxUUpuUIMLXM1fv0nFu9g3VyTUD_FhbkHG9OD322THyJSW_b30QMqRbvIdfqmLIDqTFNf7hxl-aXOcXzgii6H7_Wirdj5BOVniojDjRFpPEiyewZXYx"

# df_business <- get_all_businesses(token, "Kelowna", "Restaurant", "italian")
df_business <- GetAllBusinesses(token, "Kelowna", "Restaurant", "italian")
df_business

# test geo_heatmap function
geo_heatmap(df_business, 'review_count')
geo_heatmap(df_business, 'rating')
geo_heatmap(df_business, 'price_factor')



# for unittest
setwd("~/Desktop/ubco_mds/data534/yelp_api_wrapper")
library(testthat)
test_dir("tests")

test_file("./tests/test-geo_heatmap.R")


# for build package
library(devtools)
build()
## isntall package
package_file_path <- "/Users/apple-b/Desktop/ubco_mds/data534/yelpviz_0.1.0.tar.gz"

devtools::install(package_file_path)
library(your_package_name)

library(testthat)
test_dir("/path/to/your/package/tests")

library(covr)
# Run your tests
test_dir("/path/to/your/package/tests")

# Assess coverage
package_coverage("/path/to/your/package")
