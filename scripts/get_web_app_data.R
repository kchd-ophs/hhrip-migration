### get Healthy Homes Web Application Data
library(tidyverse)
library(httr2)
library(jsonlite)
library(purrr)
library(here)

data_dir <- here("data")

### URL constants
b <- "https://hd.kcmo.org/HealthyHomes/api/ReportsService/"
tkn <- Sys.getenv("HHRIP_TKN")

getHhripResps <- function(append_path,base_url=b,api_token=tkn) {
  # function to get data from the relevant HHRIP endpoints
  req <- request(base_url) |>
    req_url_path_append(append_path) |>
    req_url_query(token=api_token)

  df <- req_perform(req) |>
    resp_body_string() |>
    fromJSON()
  return(df)
}

# get data from api
paths <- c(
  active_props="GetActiveProperties",
  inactive_props="GetInactiveProperties",
  payments="GetPayments",
  unpaid_charges="GetUnpaidCharges",
  unpaid_reinsp="GetUnpaidReinspections"
)

all_dat <- lapply(paths,\(x) getHhripResps(x))


# export data to CSVs
today <- Sys.Date() |> str_remove_all("-")
imap(all_dat,\(x,idx) write_csv(x,paste0(data_dir,"/",idx,"_",today,".csv")))

