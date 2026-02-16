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
MY_REGION <- "SCHPM1"

filtered_data <- filter_and_plot_region_selection(
  ospar_region = MY_REGION, 
  df = my_selection, 
  filename = paste0("../../data_sets/EDITO_dasid_4687_", MY_REGION, "PH2_copepod_abundance.png")
)

# subset columns
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
# classify copepods
# ------------------------------------------------------------------------------
copepods <- read_yaml("lookup_tables/copepods.yaml") %>% unlist()
copepod_data <- my_subset %>%
  filter(scientificname_accepted %in% copepods)

# -------------------------
# Sum abundances per eventID
# -------------------------
event_sum <- copepod_data %>%
  group_by(eventid) %>%
  summarise(daily_abundance = sum(abundance, na.rm = TRUE),
            observationdate = first(observationdate),  # keep date for month grouping
            .groups = "drop")

# -------------------------
# Compute monthly average (first day of month)
# -------------------------
monthly_avg <- event_sum %>%
  mutate(month = floor_date(as.Date(observationdate), "month")) %>%
  group_by(month) %>%
  summarise(values = mean(daily_abundance, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    station = "SNS",        # hard-coded station
    date = month            # first day of the month
  ) %>%
  select(station, date, values)


# ------------------------------------------------------------------------------
# save to CSV
# ------------------------------------------------------------------------------
dest <- paste0("../../data_sets/EDITO_dasid_4687_", MY_REGION, "_PH2_copepod_abundance.csv")
write.csv(wide_df, dest, row.names = FALSE)
print("Finished ETL: wide-format CSV ready for PH2 analysis")

