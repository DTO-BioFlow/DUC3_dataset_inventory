library(arrow)
library(dplyr)

#' Open Dataset from a Parquet URL
#'
#' This function takes an S3 URL pointing to a Parquet file, extracts the 
#' necessary endpoint and path information, establishes a connection to the
#' S3 file system, and attempts to load the dataset into memory.
#'
#' @param s3_url A character string representing the S3 URL to the Parquet file.
#'
#' @return A dataset (of class `arrow::Table`) if the dataset is loaded 
#' successfully. Returns `NULL` if loading fails.
#'
#' @examples
#' s3_url <- "https://s3.waw3-1.cloudferro.com/emodnet/emodnet_biology/12639/
#' eurobis_parquet_2025-03-14.parquet"
#' eurobis_dataset <- open_eurobis(s3_url)
#' 
#' if (!is.null(eurobis_dataset)) {
#'   print("Dataset loaded successfully.")
#' } else {
#'   print("Failed to load the dataset.")
#' }
open_my_parquet <- function(s3_url) {
  # Parse the S3 URL to extract the endpoint and path
  temp <- strsplit(s3_url, "//")[[1]][2]  # Remove 'https://'
  endpoint <- strsplit(temp, "/")[[1]][1]  # Extract endpoint
  data_path <- sub(paste0(endpoint, "/"), "", temp)  # Extract path
  
  # Create the S3FileSystem connection
  data_lake <- S3FileSystem$create(anonymous = TRUE, 
                                   scheme = "https",
                                   endpoint_override = endpoint)
  
  # Try to open the dataset and handle any errors
  dataset <- NULL
  tryCatch({
    dataset <- arrow::open_dataset(data_path, filesystem = data_lake, format = "parquet")
  }, error = function(e) {
    cat("Error loading dataset:", e$message, "\n")
  })
  
  # Return the dataset (or NULL if loading failed)
  return(dataset)
}
