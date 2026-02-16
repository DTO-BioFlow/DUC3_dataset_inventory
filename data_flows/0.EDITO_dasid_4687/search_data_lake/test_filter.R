library(testthat)
library(arrow)
library(dplyr)  # <-- Ensure dplyr is loaded for collect()
library(tibble) # <-- Ensure tibble is loaded for test data
source("_filter_parquet.R")

test_that("filter_parquet correctly filters the dataset", {
  # Create a mock dataset
  df <- tibble(
    aphiaidaccepted = c(126417, 123456, 126417, 126417),
    longitude = c(0.5, -1, 0.8, 1.2),
    latitude = c(50.5, 49, 50.8, 51.2),
    observationdate = as.POSIXct(c("2019-06-01", "2018-12-01", "2020-05-15", "2021-01-01"), tz = "UTC")
  )
  
  # Convert to an Arrow Table to mimic the real dataset
  dataset <- arrow::Table$create(df)
  
  # Define filtering parameters
  filter_params <- list(
    aphiaidaccepted = 126417,
    longitude = c(0, 1),
    latitude = c(50, 51),
    observationdate = c("2019-01-01", "2020-12-31")
  )
  
  # Apply filtering
  result <- filter_parquet(dataset, filter_params)
  
  # Convert result to a tibble (Arrow tables behave differently)
  result_df <- as.data.frame(result)
  
  # Expected output: Only rows 1 and 3 should match the filter
  expected_df <- df[c(1, 3), ]
  
  # Check that the number of rows is correct
  expect_equal(nrow(result_df), nrow(expected_df))
  
  # Check that the values match
  expect_equal(result_df$aphiaidaccepted, expected_df$aphiaidaccepted)
  expect_equal(result_df$longitude, expected_df$longitude)
  expect_equal(result_df$latitude, expected_df$latitude)
  expect_equal(result_df$observationdate, expected_df$observationdate)
})
