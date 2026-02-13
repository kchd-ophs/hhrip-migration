### export data for geocoding within ArcGIS Pro.
### geocoding done with the local "Address Points" geocoder
### preferred location type is "address location" not "routing location"

# load packages
library(tidyverse)
library(readxl)
library(here)
source("scripts/fns_dk.R")

access_date_str <- "20251208" # date of file exports

### Complaints
read_excel(paste0("data/complaints_",access_date_str,".xlsx")) |>
  janitor::clean_names() |>
  geocode_prep(property_address,property_zip_code,
               "data/complaints_for_geocoding.csv")

### Inspections
col_types <- c("text",rep("guess",9),"text",rep("guess",54),"text")
insps <- read_excel(
  paste0("data/inspection_history_",access_date_str,".xlsx"),
  col_types = col_types)

insps |>
  janitor::clean_names() |>
  geocode_prep(property_address,property_zip_code,
               "data/inspections_for_geocoding.csv")



### TODO: Properties
janitor::clean_names() |>
  geocode_prep(property_address,property_zip_code,
               "data/properties_for_geocoding.csv")

