#' Filter a dataset based on specified criteria
#'
#' This function dynamically filters a dataset using a list of filtering conditions.
#' It supports both single-value and range filters and ensures that date columns
#' are correctly handled. If a column does not exist in the dataset, a warning is displayed.
#'
#' @param data A dataset (Arrow `Dataset` or `DataFrame`) to filter.
#' @param filters A named list where keys are column names and values are filter conditions.
#'        - Single-value filters (e.g., `aphiaidaccepted = 126417`) will use `==`.
#'        - Range filters (e.g., `longitude = c(0, 1)`) will use `>` and `<`.
#'        - Date filters (e.g., `observationdate = c("2019-01-01", "2020-12-31")`) are converted to `POSIXct`.
#'
#' @return A filtered dataset collected into memory.
#' @export
#'
#' @examples
#' filter_params <- list(
#'   aphiaidaccepted = 126417,
#'   longitude = c(0, 1),
#'   latitude = c(50, 51),
#'   observationdate = c("2019-01-01", "2020-12-31")
#' )
#' filtered_data <- filter_parquet(dataset, filter_params)
filter_parquet <- function(data, filters) {
  # Get available column names in the dataset
  available_columns <- names(data)
  
  # Initialize filter expression list
  filter_conditions <- list()
  
  # Loop through filters and dynamically apply them
  for (key in names(filters)) {
    if (key %in% available_columns) {
      value <- filters[[key]]
      
      # Convert timestamps if necessary
      if (inherits(data[[key]], "POSIXct") || grepl("date", key, ignore.case = TRUE)) {
        value <- as.POSIXct(value)  # Convert string to POSIXct for filtering
      }
      
      # Apply different filtering logic based on value type
      if (length(value) == 1) {
        # Single value filter (e.g., exact match)
        filter_conditions <- append(filter_conditions, list(rlang::expr(!!rlang::sym(key) == !!value)))
      } else if (length(value) == 2) {
        # Range filter (e.g., between two values)
        filter_conditions <- append(filter_conditions, list(rlang::expr(!!rlang::sym(key) > !!value[1] & !!rlang::sym(key) < !!value[2])))
      }
    } else {
      # Warn if a key is not found in the dataset
      warning(paste("Filter", key, "not applied: column does not exist in dataset"))
    }
  }
  
  # Apply filtering conditions and collect data
  data %>%
    filter(!!!filter_conditions) %>%
    collect()  # Ensures the data is loaded into memory
}