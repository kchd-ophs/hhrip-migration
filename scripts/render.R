# RMD (input) file name, without extension
rmd_name <- ""

# HTML (output) file name, if different, without extension
html_name <- ""

rmarkdown::render(
  input = paste0("scripts/", rmd_name, ".rmd"),
  output_file = paste(
    ifelse(html_name != "", html_name, rmd_name),
    Sys.Date(),
    sep = "-"
  ),
  output_dir = "output"
)
