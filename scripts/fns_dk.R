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



