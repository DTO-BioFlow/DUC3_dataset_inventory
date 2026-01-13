#' Extract, Filter, Classify, and Aggregate Zooplankton Abundance Data
#'
#' This script retrieves plankton occurrence data from a remote Eurobis dataset (via STAC/Parquet),
#' filters it by dataset ID, parameter, and event type, restricts it spatially using OSPAR regions,
#' classifies observations into lifeform groups, and aggregates the data into a monthly time series
#' of average abundances.
#' 
#' The result is exported as a CSV file suitable for lifeform indicator calculations and time-series
#' trend analysis in marine biodiversity assessments.
#'
#' @details
#' Main workflow:
#' \itemize{
#'   \item Query STAC catalog and load the occurrence dataset from a Parquet file.
#'   \item Filter by `datasetid`, `parameter`, and `eventtype`.
#'   \item Filter by `TripActionID` using an external lookup table.
#'   \item Restrict data to a selected OSPAR region using spatial filtering.
#'   \item Classify observations into lifeform groups based on a YAML lookup.
#'   \item Aggregate abundance values per period and lifeform.
#'   \item Export the results and generate spatial distribution plots.
#' }
#'
#' @source
#' - Eurobis Occurrence Parquet: 
#'   \url{https://s3.waw3-1.cloudferro.com/emodnet/emodnet_biology/12639/eurobis_parquet_2025-03-14.parquet}
#' - OSPAR Regions: 
#'   \url{https://odims.ospar.org/en/submissions/ospar_comp_au_2023_01/}
#'
#' @author
#' Willem Boone \url{https://github.com/willem0boone/EDITO_PH1}
#'
#' @note
#' Requires helper scripts from `search_data_lake/` and `utils/`, as well as 
#' external lookup tables and YAML classification files stored locally.
#'
#' @keywords zooplankton, EMODnet, lifeform indicator, biodiversity, STAC, Parquet, spatial filtering
#'
library(yaml)
library(dplyr)
library(purrr)
library(rlang)
library(arrow)
library(tidyr)
library(readr)
library(lubridate)

source("search_data_lake/_search_STAC.R")
source("search_data_lake/_open_parquet.R")
source("search_data_lake/_filter_parquet.R")
source("utils/ospar_regions.R")

# ------------------------------------------------------------------------------
# get the occurrence parquet file
# ------------------------------------------------------------------------------

occ = search_STAC()
print(occ)
# my_parquet = occ[[1]]

#' older version of the occ parquet file:
#' 
#' https://s3.waw3-1.cloudferro.com/emodnet/biology/eurobis_occurrence_data/
#' eurobis_occurrences_geoparquet_2024-10-01.parquet
#' 
#' or 
#' 
#' https://s3.waw3-1.cloudferro.com/emodnet/emodnet_biology/12639/
#' eurobis_parquet_2025-03-14.parquet

# at the moment this script was written, this was the parquet url;
my_parquet <- paste0("https://s3.waw3-1.cloudferro.com/emodnet/emodnet_biology", 
                     "/12639/eurobis_parquet_2025-03-14.parquet")

# ------------------------------------------------------------------------------
# filter on dataset
# ------------------------------------------------------------------------------

dataset = open_my_parquet(my_parquet)
print(dataset$schema)
column_names <- dataset$schema$names
print(column_names)

filter_params <- list(
  #longitude = c(0, 1),
  #latitude = c(50, 51),
  datasetid = 4687,
  parameter = "WaterAbund (#/ml)",
  eventtype = "sample"
)

# Apply filtering
my_selection <- filter_parquet(dataset, filter_params)

# ------------------------------------------------------------------------------
# filter on Trip Action
# ------------------------------------------------------------------------------

desired_trip_actions = read_csv("lookup_tables/allTripActions_exp.csv", 
                                show_col_types = FALSE)

my_selection <- filter_parquet(dataset, filter_params) %>%
  mutate(
    TripActionID = stringr::str_extract(event_id, "TripActionID\\d+"),
    TripActionID = as.integer(stringr::str_remove(TripActionID, "TripActionID"))
  ) %>%
  filter(TripActionID %in% desired_trip_actions$Tripaction)

# ------------------------------------------------------------------------------
# filter on OSPAR region
# ------------------------------------------------------------------------------
#'filter_and_plot_region_selection is function sourced from
#'utils/ospar_regions.R

MY_REGION <- "SNS"
# MY_REGION <- "SCHPM1"

filtered_data <- filter_and_plot_region_selection(
  ospar_region = MY_REGION, 
  df = my_selection, 
  filename = paste0("../../data_sets/EDITO_dasid_4687_", MY_REGION,"_holo_mero.png")
  )


# ------------------------------------------------------------------------------
# format column names according to PLET requirements
# ------------------------------------------------------------------------------
my_subset = subset(filtered_data, select=c(parameter,
                                          parameter_value,
                                          datasetid,
                                          observationdate,
                                          scientificname_accepted,
                                          eventtype,
                                          eventid
                                          )
                   )

names(my_subset)[names(my_subset) == "parameter_value"] <- "abundance"
my_subset$abundance <- as.numeric(my_subset$abundance)

my_subset$Time <- as.Date(my_subset$observationdate,format="%Y-%m-%d %H:%M:%S")
my_subset$date = floor_date(my_subset$Time, "month")
my_subset$period = format(my_subset$date, format="%Y-%m")

# ------------------------------------------------------------------------------
# classify in life form groups
# ------------------------------------------------------------------------------

# Load the lifeform lookup tables for mapping
lifeform_map <- read_yaml(paste0("lookup_tables/",
                                 "lifeform_lookup_zooplankton.yaml")
                          )

# Initialize lifeform column
my_subset$lifeform <- NA

# Loop through and classify
for (group in names(lifeform_map)) {
  my_subset$lifeform[my_subset$scientificname_accepted 
                     %in% lifeform_map[[group]]
                     ] <- group
}

my_subset %>% drop_na(lifeform)

# ------------------------------------------------------------------------------
# Aggregate abundances per life form
# ------------------------------------------------------------------------------
print(my_subset)


my_subset <- my_subset[my_subset$lifeform %in% c("meroplankton",
                                                  "holoplankton"),
                        ]
#' Aggregate holoplankton & meroplankton for each event and assign this as 
#' 1 sample

my_subset = aggregate(abundance ~ period + lifeform + eventid, my_subset, sum)
my_subset$num_samples = 1

#' aggregate per period and divide by the numer of samples
my_subset = aggregate(cbind(abundance, num_samples) ~ period + lifeform,
                      my_subset,
                      sum)

my_subset$abundance = my_subset$abundance/my_subset$num_samples

print(my_subset)

# Step 1: Ensure all lifeform-date combinations exist
my_subset <- my_subset %>%
  complete(period, lifeform = c("holoplankton", "meroplankton"), fill = list(abundance = 0))

# Step 2: Fix num_samples by grouping by period and taking the max for each period
my_subset <- my_subset %>%
  group_by(period) %>%
  mutate(num_samples = max(num_samples, na.rm = TRUE)) %>%
  ungroup()

# Done
print(my_subset)

# ------------------------------------------------------------------------------
# save
# ------------------------------------------------------------------------------
dest = file.path(paste0("../../data_sets/EDITO_dasid_4687_", MY_REGION,"_holo_mero.csv"))
write.csv(my_subset, dest, row.names=F)

print("finished get data")

