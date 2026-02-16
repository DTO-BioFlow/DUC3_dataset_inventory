install.packages("rstac")
library(rstac)

#' Fetch Asset URLs from a STAC Catalog
#'
#' Connects to a SpatioTemporal Asset Catalog (STAC) using the `rstac` package,
#' retrieves collections, searches for items whose collection ID contains a specified
#' variable keyword, and extracts asset URLs matching a specified asset type (e.g., "parquet").
#'
#' @param url Character string specifying the STAC catalog URL. Default is 
#'   "https://catalog.dive.edito.eu".
#' @param variable Character string used to filter collections by matching 
#'   their ID. Only collections with IDs that include this string will be searched.
#'   Default is "occurrence".
#' @param asset Character string specifying the asset key to extract from each item 
#'   (e.g., "parquet"). Default is "parquet".
#'
#' @return A list of asset URLs (e.g., links to parquet files) if found. 
#' Returns `NULL` in case of connection errors, no matching collections/items,
#' or no assets matching the given key.
#'
#' @examples
#' # Default usage
#' fetch_occurrence_data()
#'
#' # Custom STAC URL
#' fetch_occurrence_data(url = "https://another-stac-url.example.com")
#'
#' # Custom variable and asset key
#' fetch_occurrence_data(variable = "species", asset = "data_asset")
#'
#' @export


search_STAC <- function(url = "https://catalog.dive.edito.eu",
                        variable = "occurrence",
                        asset = "parquet") {
  result <- tryCatch({
    # Connect to STAC API
    stac_api <- stac(url)
    
    # Fetch collections
    collections <- collections(stac_api)%>% get_request()
    
    # If no collections found, return NULL
    if (length(collections$collections) == 0) {
      warning("No collections found. Returning NULL.")
      return(NULL)
    }
    
    items <- list()
    occurrence_data_list <- list()
    
    # Loop through collections and find matching items
    for (collection in collections$collections) {
      if (grepl(variable, collection$id)) {
        
        # Get items for the collection
        collection_items <- stac_search(stac_api, collections = collection$id) %>% get_request()
        
        # If no items found, continue
        if (length(collection_items$features) == 0) next
        
        for (item in collection_items$features) {
          items <- append(items, list(item))
        }
      }
    }
    
    # If no items found, return NULL
    if (length(items) == 0) {
      warning("No matching items found. Returning NULL.")
      return(NULL)
    }
    
    # Extract parquet asset URLs
    for (item in items) {
      for (key in names(item$assets)) {
        value <- item$assets[[key]]

        if (key == asset) {
          occurrence_data_list <- append(occurrence_data_list, value$href)
        }
      }
    }
    
    # If no parquet files found, return NULL
    if (length(occurrence_data_list) == 0) {
      warning("No parquet assets found. Returning NULL.")
      return(NULL)
    }
    
    return(occurrence_data_list)
    
  }, error = function(e) {
    print(paste("Error:", e$message))
    return(NULL)
  })
  
  return(result)
}
