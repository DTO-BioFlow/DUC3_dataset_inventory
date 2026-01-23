library(jsonlite)
library(yaml)
source('test_PH1.R')

# ---- Paths ----
project_root <- normalizePath(file.path(".."))
catalog_path <- file.path(project_root, "data_catalog", "data_catalog_PH1.json")
output_path  <- file.path(project_root, "tests", "PH1_validation_report.yaml")

# ---- Load catalog ----
catalog <- fromJSON(catalog_path, simplifyVector = FALSE)

# ---- Run tests ----
report <- lapply(catalog, function(item) {
  
  data_url <- item$sources$data_store
  
  test_result <- check_data_PH1(data_url)
  
  list(
    dataset_id = item$id,
    data_store = data_url,
    pass = test_result$pass,
    message = test_result$message
  )
})

# ---- Write report ----
write_yaml(
  list(
    test = "PH1 structural validation",
    timestamp = Sys.time(),
    results = report
  ),
  output_path
)

cat("PH1 validation report written to:\n", output_path, "\n")
