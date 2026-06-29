library(tidyverse)
### custom functions for this project

print_pct <- function(x,digits=3){
  # Formatting decimal numbers in more readable percent form
  round(x,digits)*100
}

col_missingness <- function(df){
  # Prints percent missing of each column in df
  sapply(names(df), function(x) print_pct(mean(is.na(pull(df,x)))))
}

clean_strs <- function(df) {
  # Capitalizes and trims all text columns
  #TODO: allow ability to select/deselect certain columns
  df |> mutate(
    across(where(is.character), ~ str_squish(str_to_upper(.x)))
  )
}

### DOES NOT WORK
add_count_col_old <- function(df, select_col, outname) {
  df |>
    group_by({{ select_col }}) |>
    mutate(
      "n_{{outname}}" := ifelse(is.na(across(across({{ select_col }}))), NA, n()),
#      "dups_{{outname}}" := ifelse("n_{{outname}}" > 1, TRUE, FALSE)
    ) |>
    ungroup()
}


count_dat <- function(df, select_col){
  df |>
    count(across({{ select_col }})) |>
    arrange(desc(n))
}

count_dat_mult <- function(df, select_cols){
  df |>
    count(across(select_cols)) |>
    arrange(desc(n))
}

geocode_prep <- function(df,addr_col,zip_col,outfile) {
  df |>
  select({{addr_col}},{{zip_col}}) |>
  mutate("{{zip_col}}":=as.character({{zip_col}})) |>
  clean_strs() |>
  mutate("{{zip_col}}":=str_extract({{zip_col}},"^[0-9]*")) |>
  distinct() |>
  mutate(city="KANSAS CITY",state="MO") |>
  write_csv(outfile,na="")
}

make_logicals_easier <- function(df,true_val="Yes",false_val=""){
  # convenience function to replace TRUE/FALSE with strings for better
  # readability
  mutate(df, across(where(is.logical), \(x) ifelse(x, true_val, false_val)))
}

bind_common_cols <- function(...){
#  given a list of data frames, binds them together including only
# the names common across all of them

  dat <- list(...)
  cols_in_all <- lapply(dat, names) |> reduce(intersect)
  dfs_with_all <- lapply(dat, function(x)
    select(x,all_of(cols_in_all))
  )
  do.call(bind_rows,dfs_with_all)

}

read_all_props <- function() {
  # convenience function to read all properties in different categories
  props_id_orig <- readRDS(
    paste(data_dir,"properties_parcel_match_dk.RDS",sep="/")) |>
    mutate(source="PROPS")
  props_inactive_orig <- readRDS(
    paste(data_dir,"properties_inactive_dontmatch_dk.RDS",sep="/")) |>
    mutate(source="PROPS")
  props_unmatch_orig <-readRDS(
    paste(data_dir,"properties_address_match_dk.RDS",sep="/")) |>
    mutate(source="PROPS")

  # combine
  props <- bind_common_cols(
    props_id_orig,props_inactive_orig,props_unmatch_orig
  ) |>
    select(application_date:prop_type,total_num_units:owner_name,
           arcgis_location.x,arcgis_location.y,property_address_orig)
}

clean_prop_names <- function(df){

  if(!"property_name_orig" %in% names(df)){
    df <- df |> mutate(property_name_orig=property_name)
  }

  df |>
    mutate(
      property_name=str_squish(str_replace_all(property_name,"[[:punct:]]"," ")),
      property_name=str_remove_all(property_name," APARTMENTS$"),
      property_name=str_remove_all(property_name," APARTMENT$"),
      property_name=str_remove_all(property_name," APTS$"),
      property_name=str_remove_all(property_name," APT$"),
      property_name=str_remove_all(property_name," TOWNHOME$"),
      property_name=str_remove_all(property_name," TOWNHOMES$"),
      property_name=str_remove_all(property_name,"^THE ")
    ) |>
    mutate(property_name=str_squish(property_name))
}

# constants for exports
keep_cols_nonmatch <- c("original_id","complaint_id","entered_in311on",
  "date_complaint_received","application_date","complaint_completed",
  "complaint_completed_date","complaint_outcome_comments","complainant_name",
  "property_name","permit","property_address","unit_building","property_zip_code",
  "complaint_recived_by","complaint_assigned_to","resolved_date","ineligible",
  "pre_launch","refer","vacant","dup_note","pub_housing","n_ids","from_311",
  "source","match_id"
)

keep_cols_match <- c("property_id","original_id","complaint_id","entered_in311on",
  "date_complaint_received","application_date","complaint_completed",
  "complaint_completed_date","complaint_outcome_comments","complainant_name",
  "property_name_comp","property_name_prop","permit","property_address_comp",
  "property_address_prop","unit_building","property_zip_code","complaint_recived_by",
  "complaint_assigned_to","resolved_date","ineligible","pre_launch","refer","vacant",
  "dup_note","pub_housing","n_ids","from_311","source","match_id",
  "property_name_orig_comp","property_zip","parcel_num","prop_type","total_num_units",
  "owner_id","owner_name"
)

