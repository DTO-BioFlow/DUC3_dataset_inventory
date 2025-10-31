library(rstac)

url <- "https://catalog.dive.edito.eu"

# Connect to STAC API and fetch collections
stac_api <- stac(url)
collections_data <- collections(stac_api) %>% get_request()

print(collections_data)