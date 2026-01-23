# ------------------------------------------------------------------------------
# Extract, Filter, Classify, and Aggregate Zooplankton Abundance Data
# Wide-format output for new PH1 analysis
# ------------------------------------------------------------------------------

library(yaml)
library(dplyr)
library(purrr)
library(rlang)
library(arrow)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)

source("search_data_lake/_search_STAC.R")
source("search_data_lake/_open_parquet.R")
source("search_data_lake/_filter_parquet.R")
source("utils/ospar_regions.R")

# ------------------------------------------------------------------------------
# get the occurrence parquet file
# ------------------------------------------------------------------------------
my_parquet <- paste0("https://s3.waw3-1.cloudferro.com/emodnet/emodnet_biology", 
                     "/12639/eurobis_parquet_2025-03-14.parquet")

dataset <- open_my_parquet(my_parquet)

filter_params <- list(
  datasetid = 4687,
  parameter = "WaterAbund (#/ml)",
  eventtype = "sample"
)

my_selection <- filter_parquet(dataset, filter_params)

# ------------------------------------------------------------------------------
# filter on Trip Action
# ------------------------------------------------------------------------------
desired_trip_actions <- read_csv("lookup_tables/allTripActions_exp.csv", 
                                 show_col_types = FALSE)

my_selection <- my_selection %>%
  mutate(
    TripActionID = str_extract(event_id, "TripActionID\\d+"),
    TripActionID = as.integer(str_remove(TripActionID, "TripActionID"))
  ) %>%
  filter(TripActionID %in% desired_trip_actions$Tripaction)

# ------------------------------------------------------------------------------
# filter on OSPAR region
# ------------------------------------------------------------------------------
MY_REGION <- "SNS"

filtered_data <- filter_and_plot_region_selection(
  ospar_region = MY_REGION, 
  df = my_selection, 
  filename = paste0("../../data_sets/EDITO_dasid_4687_", MY_REGION, "_holo_mero.png")
)

# ------------------------------------------------------------------------------
# subset columns and format dates
# ------------------------------------------------------------------------------
my_subset <- filtered_data %>%
  select(parameter, parameter_value, datasetid, observationdate,
         scientificname_accepted, eventtype, eventid) %>%
  rename(abundance = parameter_value) %>%
  mutate(
    abundance = as.numeric(abundance),
    Time = as.Date(observationdate, format="%Y-%m-%d %H:%M:%S"),
    period = format(floor_date(Time, "month"), "%Y-%m")
  )

# ------------------------------------------------------------------------------
# classify into lifeforms
# ------------------------------------------------------------------------------
lifeform_map <- read_yaml("lookup_tables/lifeform_lookup_zooplankton.yaml")

my_subset <- my_subset %>%
  mutate(lifeform = purrr::map_chr(scientificname_accepted, function(sp) {
    group <- names(lifeform_map)[sapply(lifeform_map, function(x) sp %in% x)]
    if (length(group) == 0) return(NA)
    return(group)
  })) %>%
  drop_na(lifeform)

# ------------------------------------------------------------------------------
# keep only holo & mero
# ------------------------------------------------------------------------------
my_subset <- my_subset %>% filter(lifeform %in% c("holoplankton", "meroplankton"))

# ------------------------------------------------------------------------------
# aggregate abundances per event
# ------------------------------------------------------------------------------
my_subset <- my_subset %>%
  group_by(period, lifeform, eventid) %>%
  summarise(abundance = sum(abundance), .groups = "drop") %>%
  mutate(num_samples = 1) %>%
  group_by(period, lifeform) %>%
  summarise(
    abundance = sum(abundance) / sum(num_samples),
    num_samples = sum(num_samples),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# reshape to wide format for new PH1 function
# ------------------------------------------------------------------------------
wide_df <- my_subset %>%
  pivot_wider(
    names_from = lifeform,
    values_from = abundance,
    values_fill = list(abundance = 0)
  )

# ensure num_samples per period (take max across columns for consistency)
wide_df <- wide_df %>%
  group_by(period) %>%
  mutate(num_samples = max(num_samples, na.rm = TRUE)) %>%
  ungroup() %>%
  select(period, holoplankton, meroplankton, num_samples)

# ------------------------------------------------------------------------------
# save to CSV
# ------------------------------------------------------------------------------
dest <- paste0("../../data_sets/EDITO_dasid_4687_", MY_REGION, "_holo_mero.csv")
write.csv(wide_df, dest, row.names = FALSE)

print("Finished ETL: wide-format CSV ready for PH1 analysis")
